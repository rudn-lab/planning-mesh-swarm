use std::collections::HashMap;

use bevy::prelude::*;
use high_level_cmds::network_kit::{ConnectionInfo, RSSI};

use crate::{
    pause_controller::PauseState,
    radio::{
        nic_components::VirtualNetworkInterface,
        peer_connection::{PeerConnection, PeerConnectionBundle},
    },
};

use super::motion_types::{RobotProps, RobotState};

pub(crate) fn update_robot_radios(
    mut robot_query: Query<(Entity, &RobotProps, &mut RobotState, &GlobalTransform)>,
    mut nic_query: Query<(
        Entity,
        &mut VirtualNetworkInterface,
        &Parent,
        &GlobalTransform,
    )>,
    mut connections: Query<&mut PeerConnection>,
    mut commands: Commands,
    pause_state: Res<PauseState>,

    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    if pause_state.paused {
        return;
    }

    // Some of the commands, like sending and pairing, relate to multiple robots at once.
    // Those commands are stored here to be dealt with after the main loop over the robots.

    let mut pending_commands: HashMap<Entity, Vec<_>> = HashMap::new();

    for (entity, _props, mut robot_state, _my_transform) in robot_query.iter_mut() {
        // Remove any message receivers that have already closed
        robot_state
            .msg_receivers
            .retain(|receiver| !receiver.is_closed());

        while let Ok(message) = robot_state.from_radio.try_recv() {
            match message {
                crate::radio::virtual_nic::VirtualRadioRequest::Log((msg, ok)) => {
                    robot_state.log.push(msg);
                    ok.send(()).unwrap();
                }
                crate::radio::virtual_nic::VirtualRadioRequest::GetSelfPeerId(sender) => {
                    sender.send(robot_state.id).unwrap();
                }
                crate::radio::virtual_nic::VirtualRadioRequest::Receive(sender) => {
                    robot_state.msg_receivers.push(sender);
                }
                crate::radio::virtual_nic::VirtualRadioRequest::Ping(idx, sender) => {
                    pending_commands.entry(entity).or_default().push(
                        crate::radio::virtual_nic::VirtualRadioRequest::Ping(idx, sender),
                    )
                }
                crate::radio::virtual_nic::VirtualRadioRequest::GetPeerOfTransmitter(
                    idx,
                    sender,
                ) => {
                    pending_commands.entry(entity).or_default().push(
                        crate::radio::virtual_nic::VirtualRadioRequest::GetPeerOfTransmitter(
                            idx, sender,
                        ),
                    );
                }
                crate::radio::virtual_nic::VirtualRadioRequest::GetReachablePeers(idx, sender) => {
                    pending_commands.entry(entity).or_default().push(
                        crate::radio::virtual_nic::VirtualRadioRequest::GetReachablePeers(
                            idx, sender,
                        ),
                    );
                }
                crate::radio::virtual_nic::VirtualRadioRequest::Pair(idx, peer, sender) => {
                    pending_commands.entry(entity).or_default().push(
                        crate::radio::virtual_nic::VirtualRadioRequest::Pair(idx, peer, sender),
                    );
                }
                crate::radio::virtual_nic::VirtualRadioRequest::Unpair(idx, sender) => {
                    pending_commands.entry(entity).or_default().push(
                        crate::radio::virtual_nic::VirtualRadioRequest::Unpair(idx, sender),
                    );
                }
                crate::radio::virtual_nic::VirtualRadioRequest::Send(idx, message_type, sender) => {
                    pending_commands.entry(entity).or_default().push(
                        crate::radio::virtual_nic::VirtualRadioRequest::Send(
                            idx,
                            message_type,
                            sender,
                        ),
                    );
                }
            }
        }
    }

    // Now let's deal with the other commands (if there are any).
    if pending_commands.is_empty() {
        return;
    }

    // All these commands involve NICs, so let's collect them ahead of time.
    // This mapping is from Robot entity to NIC entity.
    let mut nics: HashMap<Entity, Vec<_>> = HashMap::new();
    for (nic_entity, nic, parent, nic_transform) in nic_query
        .iter_mut()
        .filter(|(_, _, parent, _)| pending_commands.contains_key(&parent.get()))
    {
        nics.entry(parent.get())
            .or_default()
            .push((nic_entity, nic, nic_transform));
    }

    // Also collect the mapping from robot entity to robot ID
    let mut robot_ids = HashMap::new();
    for (entity, _, robot_state, _) in robot_query.iter() {
        robot_ids.insert(entity, robot_state.id);
    }

    for (command_sender_robot_entity, pending_command) in pending_commands.into_iter() {
        'next_message: for pending_command in pending_command.into_iter() {
            match pending_command {
                crate::radio::virtual_nic::VirtualRadioRequest::GetPeerOfTransmitter(
                    idx,
                    sender,
                ) => {
                    let my_nics = nics.entry(command_sender_robot_entity).or_default();
                    let Some(nic) = my_nics.get_mut(idx) else {
                        sender.send(Err(())).unwrap();
                        continue;
                    };

                    let (_nic_entity, nic, nic_transform) = nic;

                    // If the NIC is not paired, return Ok(None).
                    match nic.peer_connection {
                        None => {
                            sender.send(Ok(None)).unwrap();
                            continue;
                        }
                        Some(peer_connection_entity) => {
                            // The NIC is paired. Find out with who.
                            let connection = connections.get_mut(peer_connection_entity);
                            let Ok(connection) = connection else {
                                // The connection disappeared when we weren't looking
                                nic.peer_connection = None;
                                sender.send(Ok(None)).unwrap();
                                continue;
                            };

                            match robot_query.get(connection.destination) {
                                Ok((_, _, state, peer_transform)) => {
                                    // The robot that we're paired with exists in the world.
                                    let their_pos = peer_transform.translation().xy();
                                    let my_pos = nic_transform.translation().xy();

                                    // TODO: figure out if we're measuring the correct antenna here
                                    let signal = nic.reach.get_signal_strength(my_pos, their_pos);
                                    let rssi = RSSI::from_float(signal);
                                    sender
                                        .send(Ok(Some(ConnectionInfo {
                                            peer_id: state.id,
                                            rssi,
                                        })))
                                        .unwrap();
                                }
                                Err(_) => {
                                    // The paired robot disappeared while we still have a paired connection to it.
                                    // Return as if it has zero RSSI.
                                    sender
                                        .send(Ok(Some(ConnectionInfo {
                                            peer_id: connection.destination_peer_id,
                                            rssi: RSSI::from_float(0.0),
                                        })))
                                        .unwrap();
                                }
                            }
                        }
                    }
                }

                crate::radio::virtual_nic::VirtualRadioRequest::Ping(idx, sender) => {
                    let my_nics = nics.entry(command_sender_robot_entity).or_default();
                    match my_nics.get_mut(idx) {
                        None => {
                            sender.send(false).unwrap();
                        }
                        Some(_nic) => {
                            sender.send(true).unwrap();
                        }
                    }
                }
                crate::radio::virtual_nic::VirtualRadioRequest::GetReachablePeers(idx, sender) => {
                    let my_nics = nics.entry(command_sender_robot_entity).or_default();
                    let Some(nic) = my_nics.get_mut(idx) else {
                        sender.send(Err(())).unwrap();
                        continue;
                    };

                    let (_nic_entity, nic, nic_transform) = nic;

                    // Find all other robots that are within range of the NIC.
                    let my_pos = nic_transform.translation().xy();

                    let mut reachable_peers = Vec::new();
                    for (peer_entity, _, peer_state, peer_transform) in robot_query.iter() {
                        if peer_entity == command_sender_robot_entity {
                            continue;
                        }

                        let their_pos = peer_transform.translation().xy();
                        let signal = nic.reach.get_signal_strength(my_pos, their_pos);
                        if signal > 0.0 {
                            reachable_peers.push(peer_state.id);
                        }
                    }
                    sender.send(Ok(reachable_peers)).unwrap();
                }
                crate::radio::virtual_nic::VirtualRadioRequest::Pair(idx, pair_with, sender) => {
                    let my_nics = nics.entry(command_sender_robot_entity).or_default();
                    let Some(nic) = my_nics.get_mut(idx) else {
                        sender.send(Err(())).unwrap();
                        continue;
                    };

                    let (nic_entity, nic, nic_transform) = nic;

                    // Find whether the other robot is within range of the NIC.
                    let my_pos = nic_transform.translation().xy();

                    for (peer_entity, _, peer_state, peer_transform) in robot_query.iter() {
                        if peer_entity == command_sender_robot_entity {
                            continue;
                        }

                        let their_pos = peer_transform.translation().xy();
                        let signal = nic.reach.get_signal_strength(my_pos, their_pos);
                        if signal == 0.0 {
                            continue;
                        }

                        if peer_state.id == pair_with {
                            // Found the robot we want to pair with.
                            // Now create the pair connection
                            let new_connection = commands
                                .spawn(PeerConnectionBundle::new(
                                    *nic_entity,
                                    command_sender_robot_entity,
                                    peer_entity,
                                    peer_state.id,
                                    &mut materials,
                                    &mut meshes,
                                ))
                                .id();
                            nic.peer_connection = Some(new_connection);
                            sender.send(Ok(true)).unwrap();
                            continue 'next_message;
                        }
                    }

                    // If we haven't found a match, return a message saying we couldn't pair.
                    sender.send(Ok(false)).unwrap();
                }
                crate::radio::virtual_nic::VirtualRadioRequest::Unpair(idx, sender) => {
                    let my_nics = nics.entry(command_sender_robot_entity).or_default();
                    let Some(nic) = my_nics.get_mut(idx) else {
                        sender.send(Err(())).unwrap();
                        continue;
                    };

                    let (_nic_entity, nic, _nic_transform) = nic;

                    if let Some(connection) = nic.peer_connection {
                        commands.entity(connection).despawn_recursive();
                        nic.peer_connection = None;
                    }
                    sender.send(Ok(())).unwrap();
                }
                crate::radio::virtual_nic::VirtualRadioRequest::Send(idx, message_type, sender) => {
                    let my_nics = nics.entry(command_sender_robot_entity).or_default();
                    let Some(nic) = my_nics.get_mut(idx) else {
                        sender.send(Err(())).unwrap();
                        continue;
                    };

                    let (_nic_entity, nic, nic_transform) = nic;

                    let Some(connection_entity) = nic.peer_connection else {
                        // This NIC is not paired.
                        sender.send(Err(())).unwrap();
                        continue;
                    };

                    let Ok(connection) = connections.get(connection_entity) else {
                        // The connection entity has been deleted while we weren't looking.
                        nic.peer_connection = None;
                        sender.send(Err(())).unwrap();
                        continue;
                    };

                    let peer_robot = connection.destination;
                    let Ok((_, _, mut peer_state, peer_transform)) =
                        robot_query.get_mut(peer_robot)
                    else {
                        // The paired robot has been deleted while we weren't looking.
                        sender.send(Err(())).unwrap();
                        continue;
                    };

                    // Find the other robot.
                    let my_pos = nic_transform.translation().xy();
                    if nic
                        .reach
                        .get_signal_strength(my_pos, peer_transform.translation().xy())
                        == 0.0
                    {
                        continue;
                    }
                    let my_robot_id = robot_ids.get(&command_sender_robot_entity).unwrap();
                    peer_state
                        .queued_messages
                        .push((*my_robot_id, message_type));

                    // Message delivered successfully
                    sender.send(Ok(())).unwrap();
                    continue 'next_message;
                }

                crate::radio::virtual_nic::VirtualRadioRequest::Log(_) => unreachable!(),
                crate::radio::virtual_nic::VirtualRadioRequest::Receive(_sender) => unreachable!(),
                crate::radio::virtual_nic::VirtualRadioRequest::GetSelfPeerId(_sender) => {
                    unreachable!()
                }
            }
        }
    }
}
