#![no_std]
#![no_main]

use defmt::*;
use embassy_embedded_hal::shared_bus::asynch::i2c::I2cDevice;
use embassy_executor::Spawner;
use embassy_rp::bind_interrupts;
use embassy_rp::gpio;
use embassy_rp::i2c::{self, Config, InterruptHandler};
use embassy_rp::peripherals::I2C0;
use embassy_rp::pwm;
use embassy_rp::pwm::Pwm;
use embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex;
use embassy_sync::mutex::Mutex;
use embassy_time::{Duration, Timer};
use gpio::{Level, Output};
use motion_hardware::L298NMotorDriver;
use {defmt_rtt as _, panic_probe as _};

bind_interrupts!(struct Irqs {
    I2C0_IRQ => InterruptHandler<I2C0>;
});

#[embassy_executor::main]
async fn main(_spawner: Spawner) {
    info!("Program start");
    let p = embassy_rp::init(Default::default());
    let mut led = Output::new(p.PIN_25, Level::Low);

    let out1 = Output::new(p.PIN_0, Level::Low);
    let out2 = Output::new(p.PIN_1, Level::Low);
    let out3 = Output::new(p.PIN_2, Level::Low);
    let out4 = Output::new(p.PIN_3, Level::Low);

    let en1 = Pwm::new_output_a(p.PWM_SLICE2, p.PIN_4, pwm::Config::default());
    let en2 = Pwm::new_output_b(p.PWM_SLICE5, p.PIN_11, pwm::Config::default());

    let motor = L298NMotorDriver::new(
        en1,
        en2,
        out1,
        out2,
        out3,
        out4,
        typebool::True,
        typebool::True,
    );
}
