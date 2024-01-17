#!/usr/bin/env bash

MY_APPS="alacritty wezterm foot fish zsh starship neovim fd ripgrep fzf git curl clang bat eza brightnessctl pipewire wireplumber"
echo "~~~~~ Using $SHELL"

printf "~~~~~ Installing %s\n" "$MY_APPS"
if [[ -x "$(command -v dnf)" ]]; then
  MY_APPS="${MY_APPS} clang-tools-extra"
  sudo dnf upgrade && sudo dnf install $MY_APPS
elif [[ -x "$(command -v apt)" ]]; then
  sudo apt upgrade && sudo apt install $MY_APPS
elif [[ -x "$(command -v pacman)" ]]; then
  sudo pacman -Syu --needed $MY_APPS
fi

printf "~~~~~ Done.\n"
