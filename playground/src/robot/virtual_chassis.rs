use async_channel::{Receiver, Sender};
use motion_high_level::{chassis::Chassis, MotionCommand};

pub enum VirtualChassisCommand {
    Motion(MotionCommand),
    Log(String),
}

impl Into<VirtualChassisCommand> for MotionCommand {
    fn into(self) -> VirtualChassisCommand {
        VirtualChassisCommand::Motion(self)
    }
}

pub struct VirtualChassis {
    pub tx: Sender<VirtualChassisCommand>,
    pub rx: Receiver<()>,
}

impl Chassis for VirtualChassis {
    async fn forward(&mut self, cells: std::num::NonZeroU8) {
        self.tx
            .send(MotionCommand::Forward(cells).into())
            .await
            .unwrap();
        self.rx.recv().await.unwrap();
    }

    async fn backward(&mut self, cells: std::num::NonZeroU8) {
        self.tx
            .send(MotionCommand::Backward(cells).into())
            .await
            .unwrap();
        self.rx.recv().await.unwrap();
    }

    async fn turn_left(&mut self, quarter_turns: std::num::NonZeroU8) {
        self.tx
            .send(MotionCommand::TurnLeft(quarter_turns).into())
            .await
            .unwrap();
        self.rx.recv().await.unwrap();
    }

    async fn turn_right(&mut self, quarter_turns: std::num::NonZeroU8) {
        self.tx
            .send(MotionCommand::TurnRight(quarter_turns).into())
            .await
            .unwrap();
        self.rx.recv().await.unwrap();
    }

    async fn log(&mut self, message: &str) {
        self.tx
            .send(VirtualChassisCommand::Log(message.to_string()))
            .await
            .unwrap();
        self.rx.recv().await.unwrap();
    }
}
