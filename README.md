# funcially

[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/david072/funcially/CI?label=CI&style=for-the-badge)](https://github.com/david072/funcially/actions/workflows/ci.yml)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/david072/funcially?label=latest%20release&style=for-the-badge)](https://github.com/david072/funcially/releases/latest)
[![GitHub](https://img.shields.io/github/license/david072/funcially?style=for-the-badge)](https://github.com/david072/funcially/blob/main/LICENSE)

An advanced scientific calculator working with text inputs.

[Documentation](https://github.com/david072/funcially/wiki)

![Image](/media/thumbnail.png)

## Downloading

- Desktop Versions with installers available in the [Releases tab](https://github.com/david072/funcially/releases)
- Web:
    - [Stable version](https://funcially.com/app)
    - [Experimental version](https://david072.github.io/funcially) (might be buggy / unstable)

## Building

Building this project requires that [Rust](https://www.rust-lang.org/) version 1.65.0 or above is installed.
To update, run `rustup update stable`.

1. Clone the project with `git clone https://github.com/david072/funcially`
2. `cd funcially`
3. Run the CLI version with `cargo run -p cli`, the GUI version with `cargo run -p gui`

For the Discord Bot:

1. Create the file `.env` in the top level, and put `DISCORD_TOKEN=<your-bot-token>` into the
   file
2. Run `cargo run -p discord`

To pass arguments to the CLI, use `cargo run -p cli -- <args>`

## Contributing

You can contribute either by using the application and reporting issues and submitting feature requests,
or by forking the project and working on it yourself.
