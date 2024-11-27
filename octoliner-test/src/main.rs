#![no_std]
#![no_main]

use defmt::*;
use embassy_embedded_hal::shared_bus::asynch::i2c::I2cDevice;
use embassy_executor::Spawner;
use embassy_rp::bind_interrupts;
use embassy_rp::gpio;
use embassy_rp::i2c::{self, Config, InterruptHandler};
use embassy_rp::peripherals::I2C0;
use embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex;
use embassy_sync::mutex::Mutex;
use embassy_time::{Duration, Timer};
use gpio::{Level, Output};
use octoliner_embassy_driver::gpioexp::GpioExpander;
use octoliner_embassy_driver::Octoliner;
use {defmt_rtt as _, panic_probe as _};

bind_interrupts!(struct Irqs {
    I2C0_IRQ => InterruptHandler<I2C0>;
});

#[embassy_executor::main]
async fn main(_spawner: Spawner) {
    info!("Program start");
    let p = embassy_rp::init(Default::default());
    let mut led = Output::new(p.PIN_25, Level::Low);

    let i2c = i2c::I2c::new_async(p.I2C0, p.PIN_17, p.PIN_16, Irqs, Config::default());
    let mutexed_i2c = Mutex::<CriticalSectionRawMutex, _>::new(i2c);

    let device = I2cDevice::new(&mutexed_i2c);
    let mut octoliner = Octoliner::new(device);
    octoliner.init().await.unwrap();
    Timer::after_millis(100).await;
    let mut counter = 0;

    // loop {
    //     led.set_high();
    //     octoliner.set_ir_leds(true).await.unwrap();
    //     let data = octoliner.analog_read_all().await.unwrap();
    //     info!("data: {:?}", data);
    //     let count = octoliner.count_of_black().await.unwrap();
    //     info!("count of black: {}", count);
    //     let blacks = octoliner.get_blacks().await.unwrap();
    //     info!("count of black: {}", blacks);
    //     // let calibration_result = octoliner.calibrate_sensitivity().await.unwrap();
    //     // info!("calibration result: {:?}", calibration_result);
    //     Timer::after(Duration::from_secs(1)).await;
    //
    //     led.set_low();
    //     // octoliner.set_ir_leds(false).await.unwrap();
    //     // let data = octoliner.analog_read_all().await.unwrap();
    //     // info!("data: {:?}", data);
    //     // let count = octoliner.count_of_black().await.unwrap();
    //     // info!("count of black: {}", count);
    //     // // let calibration_result = octoliner.calibrate_sensitivity().await.unwrap();
    //     // // info!("calibration result: {:?}", calibration_result);
    //     // Timer::after(Duration::from_secs(1)).await;
    //
    //     let sens = octoliner.get_sensitivity();
    //     info!("sensitivity: {}", sens);
    //
    //     counter += 1;
    //     info!("counter: {}", counter);
    //     if counter % 5 == 0 {
    //         info!("Calibrating");
    //         octoliner.set_ir_leds(true);
    //         let calibration_result = octoliner.calibrate_sensitivity().await.unwrap();
    //         info!("calibration result: {:?}", calibration_result);
    //     }
    // }

    loop {
        for i in 0..255 {
            octoliner.set_sensitivity(i);
            let data = octoliner.analog_read_all().await.unwrap();
            info!("Sensitivity: {}", i);
            info!("Data: {:?}", data);
            Timer::after(Duration::from_millis(50)).await;
        }
    }
}
