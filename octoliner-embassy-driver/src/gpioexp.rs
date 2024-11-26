use embassy_embedded_hal::shared_bus::{asynch::i2c::I2cDevice, I2cDeviceError};
use embassy_sync::blocking_mutex::raw::RawMutex;
use embedded_hal_async::i2c::I2c;

pub struct GpioExpander<I2C>
where
    I2C: embedded_hal_async::i2c::I2c,
{
    pub device: I2C,
    pub address: u8,
}

// https://github.com/amperka/OctolinerPi/blob/master/octoliner/gpioexp.py
const GPIO_EXPANDER_DEFAULT_I2C_ADDRESS: u8 = 0x2a;
const GPIO_EXPANDER_WHO_AM_I: u8 = 0x00;
const GPIO_EXPANDER_RESET: u8 = 0x01;
const GPIO_EXPANDER_CHANGE_I2C_ADDR: u8 = 0x02;
const GPIO_EXPANDER_SAVE_I2C_ADDR: u8 = 0x03;
const GPIO_EXPANDER_PORT_MODE_INPUT: u8 = 0x04;
const GPIO_EXPANDER_PORT_MODE_PULLUP: u8 = 0x05;
const GPIO_EXPANDER_PORT_MODE_PULLDOWN: u8 = 0x06;
const GPIO_EXPANDER_PORT_MODE_OUTPUT: u8 = 0x07;
const GPIO_EXPANDER_DIGITAL_READ: u8 = 0x08;
const GPIO_EXPANDER_DIGITAL_WRITE_HIGH: u8 = 0x09;
const GPIO_EXPANDER_DIGITAL_WRITE_LOW: u8 = 0x0A;
const GPIO_EXPANDER_ANALOG_WRITE: u8 = 0x0B;
const GPIO_EXPANDER_ANALOG_READ: u8 = 0x0C;
const GPIO_EXPANDER_PWM_FREQ: u8 = 0x0D;
const GPIO_EXPANDER_ADC_SPEED: u8 = 0x0E;

impl<I2C, E> GpioExpander<I2C>
where
    I2C: embedded_hal_async::i2c::I2c<Error = E>,
{
    pub fn new(device: I2C) -> Self {
        Self {
            device,
            address: GPIO_EXPANDER_DEFAULT_I2C_ADDRESS,
        }
    }

    pub fn new_with_address(device: I2C, address: u8) -> Self {
        Self { device, address }
    }

    pub async fn reset(&mut self) -> Result<(), E> {
        self.device
            .write(self.address, &[GPIO_EXPANDER_RESET])
            .await?;
        Ok(())
    }

    pub async fn digital_read_port(&mut self) -> Result<u16, E> {
        let mut buf = [0u8; 2];
        self.device
            .write_read(self.address, &[GPIO_EXPANDER_DIGITAL_READ], &mut buf)
            .await?;
        Ok(u16::from_be_bytes(buf))
    }
}
