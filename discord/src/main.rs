//! Discord bot frontend for the calculator
//!
//! Requires a `.env` file containing DISCORD_TOKEN

use serenity::{async_trait, Client};
use serenity::builder::CreateEmbed;
use serenity::client::{Context, EventHandler};
use serenity::model::application::interaction::{Interaction, InteractionResponseType};
use serenity::model::gateway::{Activity, Ready};
use serenity::model::prelude::command::Command;
use serenity::prelude::GatewayIntents;
use serenity::utils::Colour;

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

    if let Err(e) = client.start().await {
        println!("Client error {:?}", e);
    }
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
        }).await;

        if let Err(e) = res {
            eprintln!("Failed to register slash commands\n{:#?}", e);
        }

        println!("Registered slash commands");
    }

    async fn interaction_create(&self, ctx: Context, interaction: Interaction) {
        if let Interaction::ApplicationCommand(command) = interaction {
            println!("Received command interaction: {:#?}", command);

            let embed = match command.data.name.as_str() {
                "ping" => commands::ping::run(),
                "calc" => commands::calc::run(&command.data.options),
                _ => create_embed(|e| {
                    e.color(Colour::DARK_RED).title("Unknown command")
                }),
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

mod commands {
    use serenity::builder::CreateApplicationCommand;
    use serenity::model::application::interaction::application_command::CommandDataOption;
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
        use serenity::builder::CreateEmbed;
        use serenity::model::prelude::command::CommandOptionType;
        use serenity::model::prelude::interaction::application_command::CommandDataOptionValue;
        use serenity::utils::Colour;
        use calculator::{Calculator, ResultData, Verbosity};
        use super::*;

        pub fn run(options: &[CommandDataOption]) -> CreateEmbed {
            let mut embed = CreateEmbed::default();

            if let Some(
                CommandDataOption {
                    resolved: Some(CommandDataOptionValue::String(input)),
                    ..
                }
            ) = options.get(0) {
                let mut calc = Calculator::new(Verbosity::None);
                match calc.calculate(input) {
                    Ok(res) => {
                        embed.description(format!("`{input}`"));
                        match res.data {
                            ResultData::Number { result, unit, format } => {
                                embed
                                    .color(Colour::GOLD)
                                    .title(format!("{}{}",
                                                   format.format(result),
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
                })
        }
    }
}
