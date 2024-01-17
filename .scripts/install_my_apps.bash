#!/usr/bin/env bash

# Alternatives
# brightnessctl: brillo
MY_APPS='alacritty wezterm fish zsh starship neovim fd ripgrep fzf git curl clang bat eza brightnessctl pipewire wireplumber'
MY_SHELL=$SHELL
echo "~~~~~ Using $MY_SHELL"

if [[ -x "$(command -v dnf)" ]]; then
  echo "~~~~~ Installing $MY_APPS\n"
  MY_APPS="${MY_APPS} clang-tools-extra"
  sudo dnf install "$MY_APPS"
elif [[ -x "$(command -v apt)" ]]; then
  echo "~~~~~ Installing $MY_APPS\n"
  sudo apt install "$MY_APPS"
elif [[ -x "$(command -v pacman)" ]]; then
  echo "~~~~~ Installing $MY_APPS\n"
  sudo pacman -S install "$MY_APPS"
fi

echo "~~~~~ Done.\n"
