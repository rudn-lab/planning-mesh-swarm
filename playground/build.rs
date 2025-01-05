fn main() {
    // Scan the .rs files in the src directory.
    // For each of them, if they contain the substring `pub `,
    // then print out the path and line, and panic.

    let mut should_panic = false;
    scan_directory("src", &mut should_panic);

    if should_panic {
        println!("---");
        println!("The above lines contain unqualified pub items, which should not be used.");
        println!("Please use pub(crate) instead.");
        println!("---");
        std::process::exit(1);
    }
}

fn scan_directory(path: &str, should_panic: &mut bool) {
    for entry in std::fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            scan_directory(path.to_str().unwrap(), should_panic);
        } else if let Some(extension) = path.extension() {
            if extension == "rs" {
                let contents = std::fs::read_to_string(&path).unwrap();
                for (line_number, line) in contents.lines().enumerate() {
                    if line.contains("pub ") {
                        println!("{}:{}", path.to_str().unwrap(), line_number + 1);
                        *should_panic = true;
                    }
                }
            }
        }
    }
}
