#!/usr/bin/env bash

# Disable "Quote to prevent splitting or globbing." error so that Bash will
# split the `app` and `aur` strings at a space.
# This ensures that each individual word is passed as one parameter to the respective command.

# shellcheck shell=bash

THIS
THIS="@$(basename "$0")"

main() {
  if [[ -d $BASHDOTDIR ]]; then
    # shellcheck disable=1091
    . "$BASHDOTDIR/functions/notify.bash"
  else
    # shellcheck disable=1090
    . "${XDG_CONFIG_HOME:-$HOME/.config}bash/functions/notify.bash"
  fi
  local aur="hyprlang-git hyprutils-git hyprshade xdg-desktop-portal-hyprland-git brave-bin librewolf-bin"
  local app="hyprland hypridle hyprlock hyprpaper hyprcursor wezterm foot fish starship neovim python-pynvim fd ripgrep fzf bat eza git curl wget clang cmake brightnessctl playerctl pipewire wireplumber mako ufw most handlr-regex pandoc-cli glow syncthing rclone grim slurp wl-clipboard cliphist tlp tlp-rdw nwg-look tealdeer refind ruff pacman-contrib"
  if [[ -x "$(command -v pacman)" ]]; then
    notify "INFO" "${THIS}: Installing via 'pacman':"
    echo "$app" | tr ' ' '\n'
    # shellcheck disable=SC2086
    sudo pacman -Syu --needed $app
  else
    notify "ERROR" "${THIS}: 'pacman' is not present on this system."
  fi
  if [[ -x "$(command -v paru)" ]]; then
    notify "INFO" "${THIS}: (AUR) Installing via 'paru': ${aur}"
    # shellcheck disable=SC2086
    paru -Syu --needed $aur
  elif [[ -x "$(command -v yay)" ]]; then
    notify "ERROR" "${THIS}: 'paru' is not present on this system."
    notify "INFO" "${THIS}: (AUR) Installing via 'yay': ${aur}"
    # shellcheck disable=SC2086
    yay -Syu --needed $aur
  else
    notify "ERROR" "${THIS}: 'yay' is not present on this system."
  fi
}

main
