#!/usr/bin/env bash

main() {
  local aur="hyprshade brave-bin nvimpager clock-tui"
  local app="hyprland hypridle hyprlock hyprpaper hyprlang wezterm foot fish starship neovim python-pynvim fd ripgrep fzf bat eza git curl wget clang cmake brightnessctl playerctl pipewire wireplumber handlr-regex pandoc-cli md-tui glow syncthing rclone grim slurp wl-clipboard cliphist tlp tlp_rdw"
  printf "~~~~~ [ ArchLinux ] Install my apps.\n"
  if [[ -x "$(command -v pacman)" ]]; then
    printf "~~~~~ [INFO] Installing via 'pacman': %s.\n" "$app"
    sudo pacman -Syu --needed "$app"
  else
    printf "~~~~~ [ERROR] 'pacman' is not present on this system.\n"
  fi
  if [[ -x "$(command -v paru)" ]]; then
    printf "~~~~~ [INFO] (AUR) Installing via 'paru': %s\n" "$aur"
    paru "$aur"
  elif [[ -x "$(command -v yay)" ]]; then
    printf "~~~~~ [ERROR] 'paru' is not present on this system.\n"
    printf "~~~~~ [INFO] (AUR) Installing via 'yay': %s\n" "$aur"
    yay "$aur"
  else
    printf "~~~~~ [ERROR] 'yay' is not present on this system.\n"
  fi
  printf "~~~~~ Done.\n"
}

main
