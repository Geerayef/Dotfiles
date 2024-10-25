#!/usr/bin/env bash

getopt() {
  local option="$1"
  local default="$2"
  local value
  value="$(tmux show-option -gqv "$option")"
  if [ -n "$value" ]; then
    echo "$value"
  else
    echo "$default"
  fi
}

setopt() {
  local option=$1
  local value=$2
  tmux set-option -gq "$option" "$value"
}

setw() {
  local option=$1
  local value=$2
  tmux set-window-option -gq "$option" "$value"
}

main() {
  local stamux_dir
  stamux_dir="$HOME/.config/tmux/plugins/stamuxline"
  local theme
  theme="$(getopt "@stamux" "kanagawa")"
  source "${stamux_dir}/${theme}.tmuxtheme"
  # status
  setopt status "on"
  setopt status-bg "${bg}"
  setopt status-justify "left"
  setopt status-left-length "100"
  setopt status-right-length "100"
  # messages
  setopt message-style "fg=${b5},bg=${r0},align=centre"
  setopt message-command-style "fg=${b5},bg=${r0},align=centre"
  # panes
  setopt pane-border-style "fg=${r0}"
  setopt pane-active-border-style "fg=${r5}"
  # windows
  setw window-status-activity-style "bold"
  setw window-status-bell-style "bold"
  setw window-status-separator ""
  setw window-status-style "fg=${fg},bg=${bg},none"
  # clock
  setopt clock-mode-style 24
  # statusline
  setopt status-left "#[fg=$fg]     #{?client_prefix,#[bg=$b3],#[bg=$bg]}#{?client_prefix,#[fg=$bg],#[fg=$b3]}#S#[fg=$fg,bg=$bg] | "
  setopt status-right "#[fg=$fg,bg=$bg] 󱑂 %R#[fg=$fg,bg=$bg]    "
  setw window-status-format "#[fg=$r5,bg=$r0,italics] #I #[fg=$r5,bg=$bg,italics] #W #[fg=$fg,bg=$bg]"
  setw window-status-current-format "#[fg=$bg,bg=$b3,bold] #I #[fg=$b3,bg=$r0,bold] #W #[fg=$fg,bg=$bg]"
  setw clock-mode-colour "${b5}"
  setw mode-style "fg=${b5} bg=${r0} bold"
}

main "$@"
