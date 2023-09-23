#!/bin/env bash

MY_APPS='alacritty wezterm fish zsh starship neovim fd ripgrep fzf git curl clang bat exa'
MY_SHELL=$(echo $SHELL)
echo "~~~~~ Using $MY_SHELL"

if [[ -x "$(command -v dnf)" ]]; then
  echo "~~~~~ Installing alacritty wezterm zsh starship neovim fd ripgrep fzf git curl\n"
  MY_APPS="${MY_APPS} clang-tools-extra"
  sudo dnf install $MY_APPS
elif [[ -x "$(command -v apt)" ]]; then
  echo "~~~~~ Installing alacritty wezterm zsh starship neovim fd ripgrep fzf git curl\n"
  sudo apt install $MY_APPS
elif [[ -x "$(command -v pacman)" ]]; then
  echo "~~~~~ Installing alacritty wezterm zsh starship neovim fd ripgrep fzf git curl\n"
  sudo pacman -S install $MY_APPS
fi

echo "~~~~~ Done.\n"
