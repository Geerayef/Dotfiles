#!/usr/bin/env bash

# Disable "Quote to prevent splitting or globbing." error so that Bash will
# split the `app` and `aur` strings at a space.
# This ensures that each individual word is passed as one parameter to the respective command.

# shellcheck shell=bash

main() {
  local loc
  loc="@$(basename "$0")"
  if [[ -d $BASHDOTDIR ]]; then
    # shellcheck disable=1091
    . "$BASHDOTDIR/functions/notify.bash"
  else
    # shellcheck disable=1090
    . "${XDG_CONFIG_HOME:-$HOME/.config}/bash/functions/notify.bash"
  fi
  local aur="hyprlang-git hyprutils-git xdg-desktop-portal-hyprland-git hyprpolkitagent-git hyprshade brave-bin librewolf-bin moar-bin wezterm-git"
  local app="intel-ucode refind pacman-contrib bluez bluez-utils hyprland hypridle hyprlock hyprpaper hyprcursor foot fish starship neovim python-pynvim fd ripgrep fzf bat eza btop btrfs-progs git curl wget brightnessctl playerctl pipewire wireplumber mako ufw handlr-regex pandoc-cli nwg-look zathura mupdf glow syncthing rclone grim slurp wl-clipboard cliphist tlp tlp-rdw tealdeer ruff clang clangd bear ccls cmake stylua luacheck lua-language-server shellcheck shfmt"
  if [[ -x "$(command -v pacman)" ]]; then
    notify "INFO" "${loc}: Installing via 'pacman':"
    echo "$app" | tr ' ' '\n'
    # shellcheck disable=SC2086
    sudo pacman -Syu --needed $app
  else
    notify "ERROR" "${loc}: 'pacman' is not present on this system."
  fi
  if [[ -x "$(command -v paru)" ]]; then
    notify "INFO" "${loc}: (AUR) Installing via 'paru': ${aur}"
    # shellcheck disable=SC2086
    paru -Syu --needed $aur
  elif [[ -x "$(command -v yay)" ]]; then
    notify "ERROR" "${loc}: 'paru' is not present on this system."
    notify "INFO" "${loc}: (AUR) Installing via 'yay': ${aur}"
    # shellcheck disable=SC2086
    yay -Syu --needed $aur
  else
    notify "ERROR" "${loc}: 'yay' is not present on this system."
  fi
}

main
