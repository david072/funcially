fn main() {
    #[cfg(windows)]
    embed_resource::compile("./installers/wix/axioma.rc");
}
