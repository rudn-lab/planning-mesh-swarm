use async_channel::{Receiver, Sender};
use motion_high_level::{chassis::Chassis, MotionCommand};

pub struct VirtualChassis {
    pub tx: Sender<MotionCommand>,
    pub rx: Receiver<()>,
}

impl Chassis for VirtualChassis {
    async fn forward(&mut self, cells: std::num::NonZeroU8) {
        self.tx.send(MotionCommand::Forward(cells)).await.unwrap();
        self.rx.recv().await.unwrap();
    }

    async fn backward(&mut self, cells: std::num::NonZeroU8) {
        self.tx.send(MotionCommand::Backward(cells)).await.unwrap();
        self.rx.recv().await.unwrap();
    }

    async fn turn_left(&mut self, quarter_turns: std::num::NonZeroU8) {
        self.tx
            .send(MotionCommand::TurnLeft(quarter_turns))
            .await
            .unwrap();
        self.rx.recv().await.unwrap();
    }

    async fn turn_right(&mut self, quarter_turns: std::num::NonZeroU8) {
        self.tx
            .send(MotionCommand::TurnRight(quarter_turns))
            .await
            .unwrap();
        self.rx.recv().await.unwrap();
    }
}
