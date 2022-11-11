fn main() {
    #[cfg(windows)]
    if !std::env::var("TARGET").unwrap_or_default().contains("wasm32") {
        embed_resource::compile("./installers/wix/funcially.rc");
    }
}
