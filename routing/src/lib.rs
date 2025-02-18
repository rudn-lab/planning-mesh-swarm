use high_level_cmds::network_kit::{AsyncUtils, NetworkKit, ReceiverNic, TransmitterNic};

/// This function is a basic implementation of a routing backend
pub async fn sample_routing<
    const SENDER_COUNT: usize,
    PeerId,
    MsgType,
    RecvNicType,
    SendNicType,
    AsyncUtilsImpl,
>(
    mut kit: NetworkKit<PeerId, MsgType, RecvNicType, SendNicType, AsyncUtilsImpl, SENDER_COUNT>,
) where
    PeerId: Default + Copy + core::fmt::Debug,
    SendNicType: TransmitterNic<PeerId, MsgType>,
    RecvNicType: ReceiverNic<PeerId, MsgType>,
    AsyncUtilsImpl: AsyncUtils,
{
    loop {
        let live_senders = kit.live_senders().await;
        for (i, is_alive) in live_senders.iter().enumerate() {
            if !*is_alive {
                kit.utils
                    .log(format!("Sender {} is dead", i).as_str())
                    .await;
            }
        }
        let any = live_senders.iter().any(|alive| *alive);
        if !any {
            kit.utils
                .log("No senders are alive, sleeping for 5 seconds before trying again")
                .await;
            kit.utils.sleep(core::time::Duration::from_secs(5)).await;
            continue;
        }

        let get_task = kit.receiver.get();
        let sleep_task = kit.utils.sleep(core::time::Duration::from_millis(500));

        match embassy_futures::select::select(get_task, sleep_task).await {
            embassy_futures::select::Either::First(_) => kit.utils.log("Got a message").await,
            embassy_futures::select::Either::Second(_) => kit.utils.log("Sleeping").await,
        }

        let mut peers = [PeerId::default(); 100];
        let scan_count = kit.senders[0].scan(&mut peers).await.unwrap();

        for peer in peers.iter().take(scan_count) {
            kit.utils
                .log(format!("Found peer {:?}", peer).as_str())
                .await;
        }
    }
}
