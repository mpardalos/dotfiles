#!/bin/bash

if command -v eww; then
    echo 'eww is already installed'
    exit 0
fi

if ! command -v cargo; then
    echo 'Error: cargo is not available'
    exit 1
fi

cd $(mktemp -d)
git clone https://github.com/elkowar/eww
cd eww
cargo build --release --no-default-features --features=wayland
chmod +x target/release/eww
cp target/release/eww ~/.local/bin/eww
