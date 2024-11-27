use defmt::{dbg, debug};

pub struct GpioExpander<I2C>
where
    I2C: embedded_hal_async::i2c::I2c,
{
    pub device: I2C,
    pub address: u8,
}

pub enum PinMode {
    Input,
    InputPullup,
    InputPulldown,
    Output,
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

const ANALOG_READ_RESOLUTION: u8 = 10;
const ANALOG_WRITE_RESOLUTION: u8 = 8;

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
        // self.device
        //     .write_read(self.address, &[GPIO_EXPANDER_DIGITAL_READ], &mut buf)
        //     .await?;
        self.device.write(self.address, &[GPIO_EXPANDER_DIGITAL_READ]).await?;
        self.device.read(self.address, &mut buf).await?;
        Ok(u16::from_le_bytes(buf))
    }

    pub async fn digital_write_port(&mut self, value: u16) -> Result<(), E> {
        let buf = value.to_le_bytes();
        self.device
            .write(
                self.address,
                &[GPIO_EXPANDER_DIGITAL_WRITE_HIGH, buf[1], buf[0]],
            )
            .await?;
        self.device
            .write(
                self.address,
                &[GPIO_EXPANDER_DIGITAL_WRITE_LOW, !buf[1], !buf[0]],
            )
            .await?;
        Ok(())
    }

    pub async fn digital_write(&mut self, pin: u8, value: bool) -> Result<(), E> {
        let send_data = (0x0001u16 << pin).to_be_bytes();
        let reg = if value {
            GPIO_EXPANDER_DIGITAL_WRITE_HIGH
        } else {
            GPIO_EXPANDER_DIGITAL_WRITE_LOW
        };
        self.device
            .write(self.address, &[reg, send_data[0], send_data[1]])
            .await?;
        Ok(())
    }

    pub async fn pin_mode(&mut self, pin: u8, mode: PinMode) -> Result<(), E> {
        let send_data = (0x0001u16 << pin).to_le_bytes();
        let reg = match mode {
            PinMode::Input => GPIO_EXPANDER_PORT_MODE_INPUT,
            PinMode::InputPullup => GPIO_EXPANDER_PORT_MODE_PULLUP,
            PinMode::InputPulldown => GPIO_EXPANDER_PORT_MODE_PULLDOWN,
            PinMode::Output => GPIO_EXPANDER_PORT_MODE_OUTPUT,
        };

        self.device
            .write(self.address, &[reg, send_data[0], send_data[1]])
            .await?;
        Ok(())
    }

    pub async fn pwm_freq(&mut self, freq: u16) -> Result<(), E> {
        let buf = freq.to_le_bytes();
        self.device
            .write(self.address, &[GPIO_EXPANDER_PWM_FREQ, buf[0], buf[1]])
            .await?;
        Ok(())
    }

    /// returns from 0 to 4095
    pub async fn analog_read(&mut self, pin: u8) -> Result<u16, E> {
        let mut read_data = [0u8; 2];
        // self.device
        //     .write_read(
        //         self.address,
        //         &[GPIO_EXPANDER_ANALOG_READ, pin, GPIO_EXPANDER_ANALOG_READ],
        //         &mut read_data,
        //     )
        //     .await?;
        self.device
            .write(self.address, &[GPIO_EXPANDER_ANALOG_READ, pin])
            .await?;
        // debug!("analog_read: ->{}", pin);
        self.device.read(self.address, &mut read_data).await?;
        let read_data = u16::from_be_bytes(read_data);
        // debug!("analog_read: <-{} (before mapping)", read_data);

        Ok(Self::map_resolution(read_data, 12, ANALOG_READ_RESOLUTION))
    }

    // only accepts u8 because that's what is observed in practice for the Octoliner board and driver
    pub async fn analog_write(&mut self, pin: u8, value: u8) -> Result<(), E> {
        let data = [GPIO_EXPANDER_ANALOG_WRITE, pin, value, 0];
        debug!("analog_write: {:?}", data);
        self.device
            .write(self.address, &data)
            .await?;
        Ok(())
    }

    fn map_resolution(value: u16, from: u8, to: u8) -> u16 {
        match from.cmp(&to) {
            core::cmp::Ordering::Equal => value,
            core::cmp::Ordering::Greater => value >> (from - to),
            core::cmp::Ordering::Less => value << (to - from),
        }
    }
}
