//! Discord bot frontend for the calculator
//!
//! Requires a `.env` file containing DISCORD_TOKEN

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use serenity::{async_trait, Client};
use serenity::builder::CreateEmbed;
use serenity::client::{Context, EventHandler};
use serenity::http::Http;
use serenity::model::application::interaction::{Interaction, InteractionResponseType};
use serenity::model::gateway::{Activity, Ready};
use serenity::model::id::{ChannelId, GuildId};
use serenity::model::prelude::command::Command;
use serenity::model::prelude::Guild;
use serenity::prelude::GatewayIntents;
use serenity::utils::Colour;

use calculator::{Calculator, data_dir, Environment};

const SESSIONS_DIRECTORY: &str = "dc_sessions";

type Sessions = HashMap<ChannelId, (String, Environment)>;

#[derive(Default, Debug)]
struct Data {
    sessions: HashMap<GuildId, Sessions>,
}

struct DataKey;

impl serenity::prelude::TypeMapKey for DataKey {
    type Value = Data;
}

#[tokio::main]
async fn main() {
    if let Err(e) = dotenv::dotenv() {
        eprintln!("Failed to load .env file: {}", e);
        std::process::exit(1);
    }

    let token = std::env::var("DISCORD_TOKEN")
        .expect("Expected a token in the environment");

    let intents = GatewayIntents::GUILD_MESSAGES | GatewayIntents::MESSAGE_CONTENT;

    let mut client = Client::builder(&token, intents)
        .event_handler(Handler)
        .await
        .expect("Failed to create the client.");

    // Update currencies every 5 minutes
    tokio::task::spawn(async {
        loop {
            tokio::time::sleep(Duration::from_secs(5 * 60)).await; // (5min)
            Calculator::update_currencies();
        }
    });

    {
        let mut data = client.data.write().await;
        let new_data = load_sessions(client.cache_and_http.http.clone()).await;
        println!("Loaded session data for {} guilds", new_data.sessions.len());
        data.insert::<DataKey>(new_data);
    }

    if let Err(e) = client.start().await {
        println!("Client error {:?}", e);
    }
}

fn sessions_directory() -> PathBuf {
    data_dir().join(SESSIONS_DIRECTORY)
}

async fn load_sessions(http: Arc<Http>) -> Data {
    let mut data = Data::default();

    if !sessions_directory().try_exists().unwrap_or(false) { return data; }

    let mut needs_to_resave = false;

    let paths = match fs::read_dir(sessions_directory()) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Failed to read_dir: {}", e);
            return data;
        }
    };

    for path in paths {
        println!("path: {:?}", path);
        match path {
            Ok(entry) => {
                let Some(id) = entry.file_name()
                    .into_string()
                    .ok()
                    .and_then(|str| str.parse::<u64>().ok()) else { continue; };
                let guild_id = GuildId(id);
                let Ok(guild) = Guild::get(http.clone(), guild_id).await else {
                    needs_to_resave = true;
                    continue;
                };

                let Some(contents) = fs::read_to_string(entry.path()).ok() else { continue; };
                let Some(src_sessions) = ron::from_str::<Sessions>(&contents).ok() else { continue; };

                // Custom HashMap::retain, since we need async
                let mut sessions = Sessions::new();
                for (id, value) in src_sessions {
                    if !guild.channels(http.clone())
                        .await
                        .ok()
                        .map(|map| map.contains_key(&id))
                        .unwrap_or(false) {
                        needs_to_resave = true;
                        continue;
                    }
                    sessions.insert(id, value);
                }

                data.sessions.insert(guild_id, sessions);
            }
            Err(_) => continue,
        }
    }

    if needs_to_resave {
        for (guild_id, sessions) in &data.sessions {
            save_sessions(sessions.clone(), *guild_id);
        }
    }

    data
}

fn save_sessions(sessions: Sessions, guild_id: GuildId) {
    tokio::spawn(async move {
        if !sessions_directory().try_exists().unwrap_or(false) {
            if let Err(e) = fs::create_dir_all(sessions_directory()) {
                eprintln!("Failed to create directory {:?}: {}", sessions_directory(), e);
                return;
            }
        }

        let path = sessions_directory().join(guild_id.to_string());

        if sessions.is_empty() {
            if !path.try_exists().unwrap_or(false) { return; }

            if let Err(e) = fs::remove_file(path.clone()) {
                eprintln!("Could not remove file at {:?}: {}", path, e);
            }
            return;
        }

        let str = match ron::ser::to_string(&sessions) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("Failed to serialize sessions ({:?}): {}", sessions, e);
                return;
            }
        };
        if let Err(e) = fs::write(path.clone(), str) {
            eprintln!("Failed to write to file {:?}: {}", path, e);
        }
    });
}

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    async fn ready(&self, ctx: Context, ready: Ready) {
        println!("{} is connected", ready.user.name);

        ctx.set_activity(Activity::playing("with numbers")).await;

        let res = Command::set_global_application_commands(&ctx.http, |commands| {
            commands
                .create_application_command(|c| commands::ping::register(c))
                .create_application_command(|c| commands::calc::register(c))
                .create_application_command(|c| commands::session::register(c))
        }).await;

        if let Err(e) = res {
            eprintln!("Failed to register slash commands\n{:#?}", e);
        }

        println!("Registered slash commands");
    }

    async fn interaction_create(&self, ctx: Context, interaction: Interaction) {
        if let Interaction::ApplicationCommand(command) = interaction {
            println!("Received command interaction: {:#?}", command);

            let res = match command.data.name.as_str() {
                "ping" => Ok(Some(commands::ping::run())),
                "calc" => Ok(Some(commands::calc::run(&ctx, &command, &command.data.options).await)),
                "session" => commands::session::run(&ctx, &command).await,
                _ => Ok(Some(create_embed(|e| {
                    e.color(Colour::DARK_RED).title("Unknown command")
                }))),
            };

            let embed = match res {
                Ok(embed) => match embed {
                    Some(e) => e,
                    None => return,
                },
                Err(e) => error_embed(|embed| {
                    embed.title("An internal error occurred")
                        .field("Error", e, false)
                })
            };

            if let Err(e) = command.create_interaction_response(&ctx.http, |response| {
                response.kind(InteractionResponseType::ChannelMessageWithSource)
                    .interaction_response_data(|message| {
                        message.set_embed(embed)
                    })
            }).await {
                println!("Could not respond to slash command: {}", e);
            }
        }
    }
}

fn create_embed<F: FnOnce(&mut CreateEmbed) -> &mut CreateEmbed>(f: F) -> CreateEmbed {
    let mut embed = CreateEmbed::default();
    f(&mut embed);
    embed
}

fn error_embed<F: FnOnce(&mut CreateEmbed) -> &mut CreateEmbed>(f: F) -> CreateEmbed {
    let mut embed = CreateEmbed::default();
    embed.color(Colour::RED);
    f(&mut embed);
    embed
}

fn success_embed<F: FnOnce(&mut CreateEmbed) -> &mut CreateEmbed>(f: F) -> CreateEmbed {
    let mut embed = CreateEmbed::default();
    embed.color(Colour::GOLD);
    f(&mut embed);
    embed
}

mod commands {
    use serenity::builder::CreateApplicationCommand;
    use serenity::builder::CreateEmbed;
    use serenity::client::Context;
    use serenity::model::application::interaction::application_command::CommandDataOption;
    use serenity::utils::Colour;

    use crate::create_embed;

    pub mod ping {
        use serenity::builder::CreateEmbed;

        use super::*;

        pub fn run() -> CreateEmbed {
            create_embed(|e| e.title("Pong!"))
        }

        pub fn register(command: &mut CreateApplicationCommand) -> &mut CreateApplicationCommand {
            command.name("ping").description("The ping command")
        }
    }

    pub mod calc {
        use serenity::model::prelude::command::CommandOptionType;
        use serenity::model::prelude::interaction::application_command::{ApplicationCommandInteraction, CommandDataOptionValue};

        use calculator::{Calculator, ResultData, Verbosity};

        use crate::{DataKey, save_sessions};

        use super::*;

        pub async fn run(ctx: &Context, command: &ApplicationCommandInteraction, options: &[CommandDataOption]) -> CreateEmbed {
            let mut embed = CreateEmbed::default();

            println!("{options:?}");

            if let Some(
                CommandDataOption {
                    resolved: Some(CommandDataOptionValue::String(input)),
                    ..
                }
            ) = options.get(0) {
                let raw_option = options.iter()
                    .find(|opt| {
                        opt.kind == CommandOptionType::Boolean && opt.name == "raw"
                    });
                let use_thousands_separator = match raw_option {
                    Some(CommandDataOption {
                             resolved: Some(CommandDataOptionValue::Boolean(b)),
                             ..
                         }) => !*b,
                    _ => true,
                };

                let mut data = ctx.data.write().await;
                let data = data.get_mut::<DataKey>().unwrap();

                let guild_id = command.guild_id.unwrap();
                let environment = match data.sessions.get_mut(&guild_id) {
                    Some(s) => s.get_mut(&command.channel_id),
                    None => None,
                };

                let mut calc = match environment {
                    Some((_, env)) => {
                        Calculator::with_environment(Verbosity::None, env)
                    }
                    None => Calculator::new(Verbosity::None),
                };

                match calc.calculate(input) {
                    Ok(res) => {
                        embed.description(format!("`{input}`"));
                        match res.data {
                            ResultData::Value { result, unit, format } => {
                                embed
                                    .color(Colour::GOLD)
                                    .title(format!("{}{}",
                                                   format.format(result, use_thousands_separator),
                                                   unit.unwrap_or_default()
                                    ));
                            }
                            ResultData::Boolean(b) => {
                                embed
                                    .color(if b { Colour::DARK_GREEN } else { Colour::RED })
                                    .title(if b { "True" } else { "False" });
                            }
                            _ => {}
                        }

                        if let Some(sessions) = data.sessions.get(&guild_id) {
                            save_sessions(sessions.clone(), guild_id);
                        }
                    }
                    Err(error) => {
                        embed.color(Colour::DARK_RED)
                            .title(error.error.to_string());

                        let slice_start = std::cmp::max(0, error.start as isize - 5) as usize;
                        let slice_end = std::cmp::min(input.len(), error.end + 5);
                        let slice = &input[slice_start..slice_end];

                        let mut description = "```\n".to_string() + slice;
                        description.push('\n');

                        for _ in 0..error.start - slice_start {
                            description += "\u{200b} ";
                        }
                        description.push('^');
                        for _ in 0..error.end - error.start - 1 {
                            description.push('-');
                        }

                        description.push(' ');
                        description += &(error.error.to_string() + "```");
                        embed.description(description);
                    }
                }
            } else {
                embed.color(Colour::RED).title("Expected input option");
            }

            embed
        }

        pub fn register(command: &mut CreateApplicationCommand) -> &mut CreateApplicationCommand {
            command
                .name("calc")
                .description("Calculates the input")
                .create_option(|opt| {
                    opt.name("input").description("What to calculate").required(true).kind(CommandOptionType::String)
                }).create_option(|opt| {
                opt.name("raw")
                    .description("Don't use separators for thousands")
                    .kind(CommandOptionType::Boolean)
            })
        }
    }

    pub mod session {
        use chrono::Local;
        use serenity::http::Http;
        use serenity::model::prelude::ChannelType;
        use serenity::model::prelude::command::CommandOptionType;
        use serenity::model::prelude::interaction::application_command::{ApplicationCommandInteraction, CommandDataOptionValue};
        use serenity::Result;

        use calculator::Environment;

        use crate::{DataKey, error_embed, save_sessions, Sessions, success_embed};

        use super::*;

        pub async fn run(ctx: &Context, command: &ApplicationCommandInteraction) -> Result<Option<CreateEmbed>> {
            let options = &command.data.options;
            if let Some(sub_command) = options.first() {
                if sub_command.kind == CommandOptionType::SubCommand {
                    let mut data = ctx.data.write().await;
                    let data = data.get_mut::<DataKey>().unwrap();

                    let guild_id = command.guild_id.unwrap();
                    let sessions = data.sessions.entry(guild_id)
                        .or_insert_with(Sessions::new);

                    match sub_command.name.as_str() {
                        "new" => return new(
                            sessions,
                            &ctx.http,
                            command,
                            &sub_command.options,
                        ).await.map(Some),
                        "delete" => return delete(
                            sessions,
                            &ctx.http,
                            command,
                            &sub_command.options,
                        ).await,
                        _ => {}
                    }
                }
            }

            Ok(Some(error_embed(|e| e.title("Invalid usage!"))))
        }

        async fn new(sessions: &mut Sessions, http: &Http, command: &ApplicationCommandInteraction, options: &[CommandDataOption]) -> Result<CreateEmbed> {
            if let Some(name) = options.first() {
                if let Some(CommandDataOptionValue::String(name)) = &name.resolved {
                    if name.len() > 100 {
                        return Ok(error_embed(|e| {
                            e.title("Invalid usage")
                                .description(format!("Name must not exceed 100 characters (was {} characters long)", name.len()))
                        }));
                    }

                    if sessions.iter().any(|(_, (session_name, _))| session_name == name) {
                        return Ok(error_embed(|e| {
                            e.title("New Session")
                                .description(format!("A session with the name `{}` already exists.", name))
                        }));
                    }

                    let creation_time = Local::now();

                    let embed = create_embed(|embed| {
                        embed.color(Colour::DARK_MAGENTA)
                            .title("Session")
                            .field("Name", name, false)
                            .field("Created At", creation_time.format("%B %d, %Y"), false)
                    });

                    let msg = command.channel_id.send_message(http, |m| m.set_embed(embed)).await?;
                    let thread = command.channel_id.create_public_thread(
                        http,
                        msg.id,
                        |thread| thread.name(name).kind(ChannelType::PublicThread),
                    ).await?;

                    thread.id.add_thread_member(http, command.member.as_ref().unwrap().user.id).await?;

                    sessions.insert(thread.id, (name.to_owned(), Environment::new()));
                    save_sessions(sessions.clone(), command.guild_id.unwrap());

                    return Ok(success_embed(|e| {
                        e.title("Session created")
                            .description(format!("Successfully created session `{}`", name))
                    }));
                }
            }

            Ok(error_embed(|embed| {
                embed.title("New Session").description("Expected name argument")
            }))
        }

        async fn delete(sessions: &mut Sessions, http: &Http, command: &ApplicationCommandInteraction, options: &[CommandDataOption]) -> Result<Option<CreateEmbed>> {
            let data = if let Some(name) = options.first() {
                if let Some(CommandDataOptionValue::String(name)) = &name.resolved {
                    if name.len() > 100 {
                        return Ok(Some(error_embed(|e| {
                            e.title("Invalid usage")
                                .description(format!("Name must not exceed 100 characters (was {} characters long)", name.len()))
                        })));
                    }

                    let channel = sessions.iter()
                        .find(|(_, (session_name, _))| session_name == name)
                        .map(|(id, _)| *id);

                    Some((channel, Some(name)))
                } else { None }
            } else if sessions.contains_key(&command.channel_id) {
                Some((Some(command.channel_id), None))
            } else { None };

            if let Some((channel, name)) = data {
                return match channel {
                    Some(channel) => {
                        channel.delete(http).await?;
                        sessions.remove(&channel);

                        save_sessions(sessions.clone(), command.guild_id.unwrap());

                        if let Some(name) = name {
                            Ok(Some(success_embed(|e| {
                                e.title("Session deleted")
                                    .description(format!("Successfully deleted session `{}`", name))
                            })))
                        } else {
                            Ok(None)
                        }
                    }
                    None => {
                        let description = if let Some(name) = name {
                            format!("Could not find session thread with name `{}`", name)
                        } else { "This is not a session thread".to_owned() };

                        Ok(Some(error_embed(|e| {
                            e.title("Delete Session")
                                .description(description)
                        })))
                    }
                };
            }

            Ok(Some(error_embed(|embed| {
                embed.title("Delete Session").description("Expected name argument")
            })))
        }


        pub fn register(command: &mut CreateApplicationCommand) -> &mut CreateApplicationCommand {
            command
                .name("session")
                .description("Manage sessions")
                .create_option(|opt| {
                    opt.name("new").description("Create a new session")
                        .kind(CommandOptionType::SubCommand)
                        .create_sub_option(|opt| {
                            opt.name("name").description("Session name")
                                .kind(CommandOptionType::String).required(true).max_length(100)
                        })
                })
                .create_option(|opt| {
                    opt.name("delete").description("Delete a session")
                        .kind(CommandOptionType::SubCommand)
                        .create_sub_option(|opt| {
                            opt.name("name").description("Session name")
                                .kind(CommandOptionType::String).max_length(100)
                        })
                })
        }
    }
}
