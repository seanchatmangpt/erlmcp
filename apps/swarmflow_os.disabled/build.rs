fn main() {
    // macOS: resolve enif_* symbols at NIF load time (was rustler build script)
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        println!("cargo:rustc-link-arg=-Wl,-undefined,dynamic_lookup");
    }
}
