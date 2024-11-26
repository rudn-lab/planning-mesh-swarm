#![no_std]
pub mod gpioexp;

use embassy_embedded_hal::shared_bus::asynch::i2c::I2cDevice;
use embassy_sync::blocking_mutex::raw::RawMutex;
