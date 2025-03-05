use bevy::app::{App, Plugin};
use nic_components::SelectedAuraVisualizationMode;

use crate::AntennaRenderingPlugin;

pub(crate) mod antenna;
pub(crate) mod antenna_reach_vis_mat;
pub(crate) mod nic_components;
pub(crate) mod peer_connection;
pub(crate) mod radio_reach_tooltip;
pub(crate) mod virtual_nic;

pub(crate) struct RadioPlugin;

impl Plugin for RadioPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(radio_reach_tooltip::RadioReachTooltipPlugin);
        app.add_plugins(AntennaRenderingPlugin);
        app.add_plugins(peer_connection::PeerConnectionPlugin);
        app.insert_resource(SelectedAuraVisualizationMode::default());
    }
}
