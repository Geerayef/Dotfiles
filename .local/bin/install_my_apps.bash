#!/usr/bin/env bash

MY_APPS="hyprland hypridle hyprlock alacritty wezterm fish starship neovim fd ripgrep fzf bat eza git curl clang brightnessctl playerctl pipewire wireplumber handlr-regex"
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
