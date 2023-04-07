# funcially

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/david072/funcially/ci.yml?label=CI&style=for-the-badge)](https://github.com/david072/funcially/actions/workflows/ci.yml)
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
3. Run the GUI version with `cargo run` (`cargo run -p gui`) or the CLI version with `cargo run -p cli`.

To pass arguments to the CLI, use `cargo run -p cli -- <args>`

### Codespace

This repository contains a template for a GitHub codespace environment. This allows you to compile, develop and commit <br>
on a virtual machine accessible via your browser or a supported editor, without having to download anything.

To create a codespace:
1. Click on the "Code" dropdown and choose codespace
2. Create a new codespace
3. The virtual machine will automatically be configured with Rust and trunk for building the project
4. To run the application, run `cd gui` and `trunk serve` in the terminal
5. Once it is done building, a popup appears in the bottom right hand corner with which you can open the website in the browser

You can also use the editor you chose for making commits.

## Contributing

You can contribute either by using the application and reporting issues and submitting feature requests,
or by forking the project and working on it yourself.

Take a look at our [Contributing Guidelines](https://github.com/david072/funcially/blob/main/CONTRIBUTING.md) for more detail about contributing!
