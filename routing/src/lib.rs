use std::collections::HashSet;

use high_level_cmds::{
    AsyncUtils,
    network_kit::{NetworkKit, ReceiverNic, TransmitterNic},
};

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
    PeerId: Default + Copy + core::fmt::Debug + Eq + core::hash::Hash + Ord,
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

        let mut peers_seen = HashSet::new();
        for peer in peers.iter().take(scan_count) {
            kit.utils
                .log(format!("Found peer {:?}", peer).as_str())
                .await;
            peers_seen.insert(*peer);
        }

        let mut peers_paired = HashSet::new();
        for sender in kit.senders.iter_mut() {
            if let Ok(x) = sender.get_peer().await {
                if let Some(peer) = x {
                    peers_paired.insert(peer);
                }
            }
        }

        let mut peers_unpaired = peers_seen
            .difference(&peers_paired)
            .cloned()
            .collect::<Vec<_>>();

        kit.utils
            .log(
                format!(
                    "I see {} peers that I haven't paired with",
                    peers_unpaired.len()
                )
                .as_str(),
            )
            .await;

        for (i, sender) in kit.senders.iter_mut().enumerate() {
            if let Ok(None) = sender.get_peer().await {
                let Some(peer_to_pair) = peers_unpaired.pop() else {
                    continue;
                };
                kit.utils
                    .log(format!("Pairing NIC {i} with peer {:?}", peer_to_pair).as_str())
                    .await;
                sender.pair(peer_to_pair).await.unwrap();
            }
        }
    }
}
