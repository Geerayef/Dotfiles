#!/usr/bin/env bash

AUR_APPS="hyprshade brave-bin nvimpager clock-tui"
MY_APPS="hyprland hypridle hyprlock hyprpaper hyprlang wezterm foot fish starship neovim python-pynvim fd ripgrep fzf bat eza git curl wget clang cmake brightnessctl playerctl pipewire wireplumber handlr-regex pandoc-cli md-tui syncthing rclone grim slurp wl-clipboard cliphist tlp tlp_rdw"
printf "~~~~~ Install apps.\n"
if [[ -x "$(command -v pacman)" ]]; then
  printf "~~~~~ [INFO] Installing %s\n" "$MY_APPS"
  sudo pacman -Syu --needed "$MY_APPS"
else
  printf "~~~~~ [ERROR] pacman is not present on this system.\n"
fi
if [[ -x "$(command -v paru)" ]]; then
  printf "~~~~~ [INFO] Installing %s\n" "$AUR_APPS"
  paru "$AUR_APPS"
elif [[ -x "$(command -v yay)" ]]; then
  printf "~~~~~ [INFO] Installing %s\n" "$AUR_APPS"
  yay "$AUR_APPS"
else
  printf "~~~~~ [ERROR] paru and yay are not present on this system.\n"
fi
printf "~~~~~ Done.\n"
