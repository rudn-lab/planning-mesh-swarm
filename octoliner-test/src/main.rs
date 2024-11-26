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
    let mut gpioexp = GpioExpander::new(device);
    gpioexp.reset().await.unwrap();
    Timer::after_millis(100).await;

    loop {
        info!("led on!");
        led.set_high();
        let data = gpioexp.digital_read_port().await.unwrap();
        info!("data: {}", data);
        Timer::after(Duration::from_secs(1)).await;

        info!("led off!");
        led.set_low();
        let data = gpioexp.digital_read_port().await.unwrap();
        info!("data: {}", data);
        Timer::after(Duration::from_secs(1)).await;
    }
}
