#!/usr/bin/env bash

# Disable "Quote to prevent splitting or globbing." error so that Bash will
# split the `app` and `aur` strings at a space.
# This ensures that each individual word is passed as one parameter to the respective command.

main() {
  local aur="hyprshade brave-bin"
  local app="hyprland hypridle hyprlock hyprpaper hyprlang xdg-desktop-portal-hyprland wezterm foot fish starship neovim python-pynvim fd ripgrep fzf bat eza git curl wget clang cmake brightnessctl playerctl pipewire wireplumber mako ufw most handlr-regex pandoc-cli md-tui glow syncthing rclone grim slurp wl-clipboard cliphist tlp tlp-rdw nwg-look"
  printf "~~~~~ [ ArchLinux ] Install my apps.\n"
  if [[ -x "$(command -v pacman)" ]]; then
    printf "~~~~~ [INFO] Installing via 'pacman': %s.\n" "$app"
    # shellcheck disable=SC2086
    sudo pacman -Syu --needed $app
  else
    printf "~~~~~ [INFO] 'pacman' is not present on this system.\n"
  fi
  if [[ -x "$(command -v paru)" ]]; then
    printf "~~~~~ [INFO] (AUR) Installing via 'paru': %s\n" "$aur"
    # shellcheck disable=SC2086
    paru -Syu --needed $aur
  elif [[ -x "$(command -v yay)" ]]; then
    printf "~~~~~ [INFO] 'paru' is not present on this system.\n"
    printf "~~~~~ [INFO] (AUR) Installing via 'yay': %s\n" "$aur"
    # shellcheck disable=SC2086
    yay -Syu --needed $aur
  else
    printf "~~~~~ [INFO] 'yay' is not present on this system.\n"
  fi
  printf "~~~~~ Done.\n"
}

main
