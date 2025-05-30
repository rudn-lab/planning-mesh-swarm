use bevy::prelude::*;
use bevy_egui::EguiContexts;
use egui::{Color32, RichText, Widget};

use crate::{
    clock::{Simulation, SimulationInstant},
    robot::{onclick_handling::SelectionChanged, RobotId},
};

pub(crate) struct EventLogPlugin;

impl Plugin for EventLogPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        // app.add_systems(Update, bottom_panel); // Don't forget to add this to ui.rs
        app.add_event::<RobotEvent>();
        app.init_resource::<EventLog>();
        app.add_systems(PostUpdate, process_events);
    }
}

#[derive(Event, Clone, Debug)]
pub(crate) enum RobotEvent {
    NicLog {
        this_robot: RobotId,
        message: String,
    },

    /// A robot has tried to establish a connection to another robot with its NIC.
    Pairing {
        this_robot: RobotId,
        peer_robot: RobotId,
        this_robot_nic_idx: usize,
        did_succeed: bool,
    },

    Unpairing {
        this_robot: RobotId,
        this_robot_nic_idx: usize,
    },
}

impl RobotEvent {
    /// Display a short summary UI for this event.
    /// This is used in the log.
    ///
    /// Returns a boolean indicating if a long UI is available.
    fn render_short_ui(
        &self,
        ui: &mut egui::Ui,
        happened_at: &SimulationInstant,
        selector: &mut EventWriter<SelectionChanged>,
    ) -> bool {
        let time_str = RichText::new(format!(
            "[{}] ",
            crate::utils::format_duration(happened_at.into_inner())
        ))
        .monospace()
        .color(Color32::DARK_RED);
        ui.horizontal(|ui| {
            ui.label(time_str);

            match self {
                RobotEvent::Pairing {
                    this_robot,
                    peer_robot,
                    this_robot_nic_idx,
                    did_succeed,
                } => {
                    robot_link(ui, this_robot, selector);
                    ui.label("used its NIC");
                    ui.label(format!("{this_robot_nic_idx}"));
                    ui.label("to try to pair with");
                    robot_link(ui, peer_robot, selector);
                    ui.label(" and it was");
                    if *did_succeed {
                        ui.colored_label(Color32::GREEN, "successful");
                    } else {
                        ui.colored_label(Color32::RED, "unsuccessful");
                    }
                }

                RobotEvent::Unpairing {
                    this_robot,
                    this_robot_nic_idx,
                } => {
                    robot_link(ui, this_robot, selector);
                    ui.label("unpaired its NIC");
                    ui.label(format!("{this_robot_nic_idx}"));
                }

                RobotEvent::NicLog {
                    this_robot,
                    message,
                } => {
                    ui.label("NIC script in");
                    robot_link(ui, this_robot, selector);
                    ui.label("logged:");
                    egui::Label::new(egui::RichText::new(message).monospace())
                        .truncate()
                        .ui(ui);

                    return true;
                }
            }
            false
        })
        .inner
    }

    fn render_long_ui(
        &self,
        ui: &mut egui::Ui,
        happened_at: &SimulationInstant,
        selector: &mut EventWriter<SelectionChanged>,
    ) {
        match self {
            RobotEvent::NicLog {
                this_robot,
                message,
            } => {
                ui.label("Full text:");
                egui::Label::new(egui::RichText::new(message).monospace()).ui(ui);
            }
            _ => {
                ui.label("There is no long UI available for this event type.");
            }
        }
    }
}
#[derive(Default, Resource)]
pub(crate) struct EventLog {
    events: Vec<(SimulationInstant, RobotEvent)>,
}

fn process_events(
    mut events: EventReader<RobotEvent>,
    mut log: ResMut<EventLog>,
    time: Res<Time<Simulation>>,
) {
    use crate::clock::SimulationTime;
    for event in events.read() {
        log.events.push((time.get_instant(), event.clone()));
    }
}

pub(crate) fn bottom_panel(
    mut contexts: EguiContexts,
    mut log: ResMut<EventLog>,
    mut selector: EventWriter<SelectionChanged>,
    mut opened_event_idx: Local<Option<usize>>,
) {
    let ctx = contexts.ctx_mut();
    if let Some(idx) = *opened_event_idx {
        if let Some((happened_at, event)) = log.events.get(idx) {
            let mut keep_open = true;
            egui::Window::new("Event details")
                .open(&mut keep_open)
                .show(ctx, |ui| {
                    event.render_short_ui(ui, happened_at, &mut selector);
                    ui.separator();

                    event.render_long_ui(ui, happened_at, &mut selector);
                });

            if !keep_open {
                *opened_event_idx = None;
            }
        } else {
            // Event list was cleared, so close the window.
            *opened_event_idx = None;
        }
    }

    egui::TopBottomPanel::bottom("bottom_panel")
        .resizable(true)
        .show(ctx, |ui| {
            if ui.button("Clear events").clicked() {
                log.events.clear();
            }

            ui.separator();

            let text_style = egui::TextStyle::Body;
            let row_height = ui.text_style_height(&text_style);
            // let row_height = ui.spacing().interact_size.y; // if you are adding buttons instead of labels.
            let total_rows = log.events.len();

            egui::ScrollArea::vertical()
                .stick_to_bottom(true)
                .auto_shrink([false; 2])
                .show_rows(ui, row_height, total_rows, |ui, row_range| {
                    for row in row_range {
                        let (happened_at, event) = &log.events[row];

                        ui.horizontal(|ui| {
                            let add_long_ui_button =
                                event.render_short_ui(ui, happened_at, &mut selector);
                            if add_long_ui_button {
                                if ui.small_button("+").clicked() {
                                    *opened_event_idx = Some(row);
                                }
                            }
                        });
                    }

                    if log.events.is_empty() {
                        ui.colored_label(Color32::GRAY, "No events yet...");
                    }
                });
        });
}

fn robot_link(ui: &mut egui::Ui, robot_id: &RobotId, selector: &mut EventWriter<SelectionChanged>) {
    let resp = ui.link(format!("Robot {}", robot_id));
    if resp.clicked() {
        selector.send(SelectionChanged::SelectedRobotById(*robot_id));
    }
}
