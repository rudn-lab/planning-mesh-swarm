use async_channel::Sender;
use high_level_cmds::{chassis::Chassis, AsyncUtils, MotionCommand};

pub(crate) enum VirtualChassisCommand {
    Motion((MotionCommand, oneshot::Sender<()>)),
    Log((String, oneshot::Sender<()>)),
    Sleep((core::time::Duration, oneshot::Sender<()>)),
    LedColorSet(([u8; 3], oneshot::Sender<()>)),
    LedColorGet(oneshot::Sender<[u8; 3]>),
}

trait IntoCmd {
    fn into_cmd(self) -> (VirtualChassisCommand, oneshot::Receiver<()>);
}

impl IntoCmd for MotionCommand {
    fn into_cmd(self) -> (VirtualChassisCommand, oneshot::Receiver<()>) {
        let (tx, rx) = oneshot::channel();
        (VirtualChassisCommand::Motion((self, tx)), rx)
    }
}

impl IntoCmd for String {
    fn into_cmd(self) -> (VirtualChassisCommand, oneshot::Receiver<()>) {
        let (tx, rx) = oneshot::channel();
        (VirtualChassisCommand::Log((self, tx)), rx)
    }
}

impl IntoCmd for core::time::Duration {
    fn into_cmd(self) -> (VirtualChassisCommand, oneshot::Receiver<()>) {
        let (tx, rx) = oneshot::channel();
        (VirtualChassisCommand::Sleep((self, tx)), rx)
    }
}

pub(crate) struct VirtualChassis {
    pub(crate) tx: Sender<VirtualChassisCommand>,
}

pub(crate) struct VirtualAsyncUtils<'a> {
    pub(crate) tx: &'a Sender<VirtualChassisCommand>,
}

impl<'a> AsyncUtils for VirtualAsyncUtils<'a> {
    async fn sleep(&self, duration: core::time::Duration) {
        let (cmd, rx) = duration.into_cmd();
        self.tx.send(cmd).await.unwrap();
        rx.await.unwrap();
    }

    async fn log(&self, data: &str) {
        let (cmd, rx) = data.to_string().into_cmd();
        self.tx.send(cmd).await.unwrap();
        rx.await.unwrap();
    }
}

impl Chassis for VirtualChassis {
    async fn forward(&mut self, cells: std::num::NonZeroU8) {
        let (cmd, rx) = MotionCommand::Forward(cells).into_cmd();
        self.tx.send(cmd).await.unwrap();
        rx.await.unwrap();
    }

    async fn backward(&mut self, cells: std::num::NonZeroU8) {
        let (cmd, rx) = MotionCommand::Backward(cells).into_cmd();
        self.tx.send(cmd).await.unwrap();
        rx.await.unwrap();
    }

    async fn turn_left_quarters(&mut self, quarter_turns: std::num::NonZeroU8) {
        let (cmd, rx) = MotionCommand::TurnLeft(quarter_turns).into_cmd();
        self.tx.send(cmd).await.unwrap();
        rx.await.unwrap();
    }

    async fn turn_right_quarters(&mut self, quarter_turns: std::num::NonZeroU8) {
        let (cmd, rx) = MotionCommand::TurnRight(quarter_turns).into_cmd();
        self.tx.send(cmd).await.unwrap();
        rx.await.unwrap();
    }

    fn utils(&self) -> impl high_level_cmds::AsyncUtils {
        VirtualAsyncUtils { tx: &self.tx }
    }

    async fn get_led_color(&self) -> [u8; 3] {
        let (tx, rx) = oneshot::channel();
        let cmd = VirtualChassisCommand::LedColorGet(tx);
        self.tx.send(cmd).await.unwrap();
        rx.await.expect("failed to get color")
    }

    async fn set_led_color(&mut self, color: [u8; 3]) {
        let (tx, rx) = oneshot::channel();
        let cmd = VirtualChassisCommand::LedColorSet((color, tx));
        self.tx.send(cmd).await.unwrap();
        rx.await.expect("failed to set color");
    }
}
