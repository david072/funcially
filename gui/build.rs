fn main() {
    #[cfg(windows)]
    embed_resource::compile("./installer/wix/axioma.rc");
}
