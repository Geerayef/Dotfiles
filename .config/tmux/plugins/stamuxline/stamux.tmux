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
  setopt message-style "fg=${cyan},bg=${gray},align=centre"
  setopt message-command-style "fg=${cyan},bg=${gray},align=centre"
  # panes
  setopt pane-border-style "fg=${gray}"
  setopt pane-active-border-style "fg=${blue}"
  # windows
  setw window-status-activity-style "bold"
  setw window-status-bell-style "bold"
  setw window-status-separator ""
  setw window-status-style "fg=${fg},bg=${bg},none"
  # clock
  setopt clock-mode-style 24
  # statusline
  setopt status-left "#[fg=$fg]     #{?client_prefix,#[bg=$blue],#[bg=$bg]}#{?client_prefix,#[fg=$bg],#[fg=$yellow]}#S #[fg=$fg,bg=$bg]| "
  setopt status-right "#[fg=$fg,bg=$bg] | 󱑂 %R#[fg=$fg,bg=$bg]    "
  setw window-status-format "#[fg=$blue,bg=$gray,italics] #I #[fg=$blue,bg=$bg,italics] #W #[fg=$fg,bg=$bg]"
  setw window-status-current-format "#[fg=$bg,bg=$yellow,bold] #I #[fg=$yellow,bg=$gray,bold] #W #[fg=$fg,bg=$bg]"
  setw clock-mode-colour "${cyan}"
  setw mode-style "fg=${cyan} bg=${gray} bold"
}

main "$@"
