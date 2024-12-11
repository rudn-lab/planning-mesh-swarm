#![no_std]
use embedded_hal::{
    digital::{OutputPin, PinState},
    pwm::SetDutyCycle,
};
use typebool::Bool;

pub enum EitherError<PinErr, PwmErr> {
    Pin(PinErr),
    Pwm(PwmErr),
}
pub struct L298NMotorDriver<
    EN1,
    EN2,
    OUT1,
    OUT2,
    OUT3,
    OUT4,
    IsFirstPairForward,
    IsSecondPairForward,
> where
    EN1: SetDutyCycle,
    EN2: SetDutyCycle,
    OUT1: OutputPin,
    OUT2: OutputPin,
    OUT3: OutputPin,
    OUT4: OutputPin,
    IsFirstPairForward: Bool,
    IsSecondPairForward: Bool,
{
    enable_1: EN1,
    enable_2: EN2,
    out_1: OUT1,
    out_2: OUT2,
    out_3: OUT3,
    out_4: OUT4,
    _first_pair_forward: IsFirstPairForward,
    _second_pair_forward: IsSecondPairForward,
}

impl<EN1, EN2, OUT1, OUT2, OUT3, OUT4, FPF, SPF, PinErr, PwmErr>
    L298NMotorDriver<EN1, EN2, OUT1, OUT2, OUT3, OUT4, FPF, SPF>
where
    EN1: SetDutyCycle<Error = PwmErr>,
    EN2: SetDutyCycle<Error = PwmErr>,
    OUT1: OutputPin<Error = PinErr>,
    OUT2: OutputPin<Error = PinErr>,
    OUT3: OutputPin<Error = PinErr>,
    OUT4: OutputPin<Error = PinErr>,
    FPF: Bool,
    SPF: Bool,
{
    pub fn new(
        enable_1: EN1,
        enable_2: EN2,
        out_1: OUT1,
        out_2: OUT2,
        out_3: OUT3,
        out_4: OUT4,
        first_pair_forward: FPF,
        second_pair_forward: SPF,
    ) -> Self {
        Self {
            enable_1,
            enable_2,
            out_1,
            out_2,
            out_3,
            out_4,
            _first_pair_forward: first_pair_forward,
            _second_pair_forward: second_pair_forward,
        }
    }

    pub fn forward(&mut self) -> Result<(), EitherError<PinErr, PwmErr>> {
        self.out_1
            .set_state(PinState::from(FPF::VALUE))
            .map_err(EitherError::Pin)?;
        self.out_2
            .set_state(PinState::from(!FPF::VALUE))
            .map_err(EitherError::Pin)?;
        self.out_3
            .set_state(PinState::from(SPF::VALUE))
            .map_err(EitherError::Pin)?;
        self.out_4
            .set_state(PinState::from(!FPF::VALUE))
            .map_err(EitherError::Pin)?;
        self.enable_1
            .set_duty_cycle_fully_on()
            .map_err(EitherError::Pwm)?;
        self.enable_2
            .set_duty_cycle_fully_on()
            .map_err(EitherError::Pwm)?;
        Ok(())
    }

    pub fn stop(&mut self) -> Result<(), PwmErr> {
        self.enable_1.set_duty_cycle_fully_off()?;
        self.enable_2.set_duty_cycle_fully_off()?;
        Ok(())
    }
}
