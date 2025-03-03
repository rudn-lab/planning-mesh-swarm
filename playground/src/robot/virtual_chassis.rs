use async_channel::{Receiver, Sender};
use high_level_cmds::{chassis::Chassis, MotionCommand};

pub(crate) enum VirtualChassisCommand {
    Motion(MotionCommand),
    Log(String),
}

impl Into<VirtualChassisCommand> for MotionCommand {
    fn into(self) -> VirtualChassisCommand {
        VirtualChassisCommand::Motion(self)
    }
}

pub(crate) struct VirtualChassis {
    pub(crate) tx: Sender<VirtualChassisCommand>,
    pub(crate) rx: Receiver<()>,
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
