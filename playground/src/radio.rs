use bevy::app::{App, Plugin};

use crate::AntennaRenderingPlugin;

pub(crate) mod antenna;
pub(crate) mod antenna_reach_vis_mat;
pub(crate) mod nic_components;
pub(crate) mod radio_reach_tooltip;
pub(crate) mod virtual_nic;

pub(crate) struct RadioPlugin;

impl Plugin for RadioPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(radio_reach_tooltip::RadioReachTooltipPlugin);
        app.add_plugins(AntennaRenderingPlugin);
    }
}
