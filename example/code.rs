#![feature(iter_intersperse)]

macro_rules! identity_macro {
    ($($t:tt)*) => {
        $($t)*
    };
}

identity_macro! {
    const TEST: u32 = {
        (1.0 as u32) * 3
    };
}

fn main() {
    for i in 0..5 {
        println!("{}: {}", i, "2".parse::<u32>().unwrap() + TEST);
    }
}
