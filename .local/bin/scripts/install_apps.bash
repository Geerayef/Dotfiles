#!/usr/bin/env bash

MY_APPS="hyprland hypridle hyprlock hyprpaper wezterm foot fish starship neovim python-pynvim fd ripgrep fzf bat eza git curl wget clang cmake brightnessctl playerctl pipewire wireplumber handlr-regex pandoc-cli md-tui syncthing rclone grim slurp wl-clipboard cliphist tlp tlp_rdw"
printf "~~~~~ Using %s.\n" "$SHELL"
printf "~~~~~ Installing %s\n" "$MY_APPS"
if [[ -x "$(command -v dnf)" ]]; then
  MY_APPS="$MY_APPS clang-tools-extra"
  sudo dnf upgrade && sudo dnf install "$MY_APPS"
elif [[ -x "$(command -v apt)" ]]; then
  sudo apt upgrade && sudo apt install "$MY_APPS"
elif [[ -x "$(command -v pacman)" ]]; then
  sudo pacman -Syu --needed "$MY_APPS"
fi
printf "~~~~~ Done.\n"
