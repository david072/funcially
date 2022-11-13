# Contributing Guidelines

Thank your for investing your time to contribute to this project!

Read our [Code of Conduct](https://github.com/david072/funcially/blob/main/CODE_OF_CONDUCT.md) to keep our community approachable and respectable.

## Discussion

For asking questions, sharing screenshots, etc. you can use [GitHub Discussions](https://github.com/david072/funcially/discussions).

## Issues

[Issues](https://github.com/david072/funcially/issues) are not for asking questions (use [Discussions](https://github.com/david072/funcially/discussions) for that).
Instead, use them for [Bug reports](https://github.com/david072/funcially/issues/new?assignees=&labels=bug&template=bug_report.md&title=) or 
[Feature requests](https://github.com/david072/funcially/issues/new?assignees=&labels=enhancement&template=feature_request.md&title=). 

Please be sure to fill out the template as much as possible. 

Before creating your issue, check that there is not already a similiar issue. If so, you can comment on it and provide additional information.

## Making a PR

Before starting to work, please file an issue (or find an existing one) and announce that you are going to work on it. This avoids people doing duplicate work.
Ask for feedback before you start working on something non-trivial!

There are a number of tests in the repository which you can run using `cargo test`.

When you have something that works, feel free to open a draft pull request. That way you can get some feedback early. Then, when you feel like the PR is ready to go,
do a self-review and then open it for review.

Do not worry about many small commits. Smaller commits make it easier to understand what was changed and allows reverting them if needed. They will be squashed
once merged.

Make sure to not include any build artifacts (.msi files, etc.). If there are any files missing in the `.gitignore`, feel free to add them and include it in your PR.
You do not need to make a separate PR for this.

## Coding conventions

This project mostly follows Clippy's style hints.

- Use `TODO` or `FIXME` appropriately
- Avoid panics
- Add blank lines around all `fn`, `struct`, `enum`, `impl`, etc.
- `// Comment like this` and not `//like this`
- Use meaningful variable and function names
- Only use abbreviations when their meaning is obvious in the scenario
- Write idiomatic Rust
