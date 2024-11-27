#![no_std]
pub mod gpioexp;

use defmt::debug;
use embassy_time::Timer;

pub struct Octoliner<I2C: embedded_hal_async::i2c::I2c> {
    gpioexp: gpioexp::GpioExpander<I2C>,
    sensitivity: u8,
}

const IR_LEDS_PIN: u8 = 9;
const SENSE_PIN: u8 = 0;
const SENSOR_PIN_MAP: [u8; 8] = [4, 5, 6, 8, 7, 3, 2, 1];

// All analog values above this are considered black
const BLACK_THRESHOLD: u8 = 100;

impl<I2C: embedded_hal_async::i2c::I2c> Octoliner<I2C> {
    pub fn new(device: I2C) -> Self {
        Self {
            gpioexp: gpioexp::GpioExpander::new(device),
            sensitivity: 208,
        }
    }

    pub async fn init(&mut self) -> Result<(), I2C::Error> {
        self.gpioexp.reset().await?;
        embassy_time::Timer::after_millis(100).await;
        self.gpioexp
            .pin_mode(IR_LEDS_PIN, gpioexp::PinMode::Output)
            .await?;
        self.gpioexp.pin_mode(SENSE_PIN, gpioexp::PinMode::Output).await?;
        self.gpioexp.digital_write(IR_LEDS_PIN, true).await?;
        self.gpioexp.pwm_freq(8000).await?;
        self.set_sensitivity(self.sensitivity).await?;
        embassy_time::Timer::after_millis(100).await;
        Ok(())
    }

    pub async fn set_ir_leds(&mut self, value: bool) -> Result<(), I2C::Error> {
        self.gpioexp.digital_write(IR_LEDS_PIN, value).await
    }

    /// sensor number from 0 to 7 (wrapped if outside).
    /// returns from 0 to 4095
    pub async fn analog_read(&mut self, sensor: u8) -> Result<u16, I2C::Error> {
        let sensor = sensor & 7;
        let data = self
            .gpioexp
            .analog_read(SENSOR_PIN_MAP[sensor as usize])
            .await?;
        embassy_time::Timer::after_millis(10).await;
        Ok(data)
    }

    pub async fn analog_read_all(&mut self) -> Result<[u16; 8], I2C::Error> {
        let mut data = [0u16; 8];
        for i in 0..8 {
            data[i as usize] = self.analog_read(i).await?;
        }
        Ok(data)
    }

    pub fn get_sensitivity(&self) -> u8 {
        self.sensitivity
    }

    // Value of sensitivity: between 0 and 255 !
    pub async fn set_sensitivity(&mut self, sensitivity: u8) -> Result<(), I2C::Error> {
        self.sensitivity = sensitivity;
        self.gpioexp.analog_write(SENSE_PIN, sensitivity).await?;
        Ok(())
    }

    pub async fn count_of_black(&mut self) -> Result<u8, I2C::Error> {
        let data = self.analog_read_all().await?;
        let mut count = 0;
        for i in 0..8 {
            debug!("Data[{}]: {}; Threshold: {}", i, data[i], BLACK_THRESHOLD);
            if (data[i]) > BLACK_THRESHOLD as u16 {
                count += 1;
            }
        }
        Ok(count)
    }

    pub async fn get_blacks(&mut self) -> Result<[bool; 8], I2C::Error> {
        let data = self.analog_read_all().await?;
        Ok(data.map(|x| x > BLACK_THRESHOLD.into()))
    }

    // If OK, then returns Some(sensitivity).
    // If error in calibration, returns None.
    pub async fn calibrate_sensitivity(&mut self) -> Result<Option<u8>, I2C::Error> {
        let sensitivity_backup = self.get_sensitivity();

        // Minimum adequate sensitivity. For all values below this, channels always
        // see black even on mirror/white surfaces
        const MIN_SENSITIVITY: u8 = 120;

        self.set_sensitivity(255).await?;
        Timer::after_millis(200).await;

        // Starting at the highest possible sensitivity read all channels
        // at each iteration to find the level when all the channels become black.
        let mut sens = 255;
        while sens > MIN_SENSITIVITY {
            debug!("lowering sensitivity: {}", sens);
            self.set_sensitivity(sens).await?;
            Timer::after_millis(200).await;
            debug!("values: {:?}", self.analog_read_all().await?);
            debug!("Count of black: {}", self.count_of_black().await?);
            if self.count_of_black().await? == 8 {
                break;
            }

            sens -= 5;
        }

        // Something is broken
        if sens == MIN_SENSITIVITY {
            self.set_sensitivity(sensitivity_backup).await?;
            defmt::warn!("sensitivity less than: {}", MIN_SENSITIVITY);
            return Ok(None);
        }

        // Forward fine serach to find the level when at least one sensor value will become white
        while sens < 255 {
            debug!("raising sensitivity: {}", sens);
            self.set_sensitivity(sens).await?;
            Timer::after_millis(50).await;
            if self.count_of_black().await? != 8 {
                break;
            }

            sens += 1;
        }

        // Environment has changed since the start of the process
        if sens == 255 {
            self.set_sensitivity(sensitivity_backup).await?;
            defmt::warn!("sensitivity more than: 255");
            return Ok(None);
        }

        // Step back to fall back to all-eight-black
        sens -= 10;

        self.set_sensitivity(sens).await?;
        Ok(Some(sens))
    }
}
