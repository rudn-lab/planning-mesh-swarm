use std::time::Duration;

use high_level_cmds::{chassis::Chassis, AsyncUtils};

pub(crate) async fn simple_business_logic(mut chassis: impl Chassis + Send + Sync) {
    let mut curve = vec![];
    build_curve(3, 1, &mut curve);
    loop {
        run_hilbert_curve(&mut chassis, &curve).await;

        chassis.utils().log("Waiting").await;
        chassis.utils().sleep(Duration::from_secs(5)).await;
    }
}

#[derive(Clone, Copy)]
enum HilbertCmd {
    Left,
    Forward,
    Right,
}

impl HilbertCmd {
    fn facing(self, facing: i8) -> Self {
        if facing < 0 {
            match self {
                HilbertCmd::Left => HilbertCmd::Right,
                HilbertCmd::Right => HilbertCmd::Left,
                HilbertCmd::Forward => HilbertCmd::Forward,
            }
        } else {
            self
        }
    }
}

async fn run_hilbert_curve<C>(chassis: &mut C, buf: &[HilbertCmd])
where
    C: Chassis + Send + Sync,
{
    let mut accumulated_right_turns = 0;
    for cmd in buf {
        match cmd {
            HilbertCmd::Left => accumulated_right_turns -= 1,
            HilbertCmd::Right => accumulated_right_turns += 1,
            HilbertCmd::Forward => {
                if accumulated_right_turns % 4 != 0 {
                    chassis.turn_right_signed(accumulated_right_turns).await;
                    accumulated_right_turns = 0;
                }
                chassis.forward_one().await;
            }
        }
    }
}

fn build_curve(degree: u8, facing: i8, buf: &mut Vec<HilbertCmd>) {
    if degree == 0 {
        return;
    }
    buf.push(HilbertCmd::Right.facing(facing));

    build_curve(degree - 1, -facing, buf);

    buf.push(HilbertCmd::Forward);
    buf.push(HilbertCmd::Left.facing(facing));

    build_curve(degree - 1, facing, buf);

    buf.push(HilbertCmd::Forward);
    build_curve(degree - 1, facing, buf);

    buf.push(HilbertCmd::Left.facing(facing));
    buf.push(HilbertCmd::Forward);

    build_curve(degree - 1, -facing, buf);

    buf.push(HilbertCmd::Right.facing(facing));
}
