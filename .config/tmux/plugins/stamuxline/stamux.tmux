#!/usr/bin/env bash

get-tmux-option() {
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

seto() {
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
  theme="$(get-tmux-option "@stamux_color" "kanagawa")"
  source "${stamux_dir}/${theme}.tmuxtheme"
  # status
  seto status "on"
  seto status-bg "${bg}"
  seto status-justify "left"
  seto status-left-length "100"
  seto status-right-length "100"
  # messages
  seto message-style "fg=${cyan},bg=${gray},align=centre"
  seto message-command-style "fg=${cyan},bg=${gray},align=centre"
  # panes
  seto pane-border-style "fg=${gray}"
  seto pane-active-border-style "fg=${blue}"
  # windows
  setw window-status-activity-style "bold"
  setw window-status-bell-style "bold"
  setw window-status-separator ""
  setw window-status-style "fg=${fg},bg=${bg},none"
  # clock
  seto clock-mode-style 24
  # statusline
  seto status-left "#[fg=$fg]     #{?client_prefix,#[bg=$blue],#[bg=$bg]}#{?client_prefix,#[fg=$bg],#[fg=$yellow]} #S #[fg=$fg,bg=$bg]  | "
  seto status-right "#[fg=$fg,bg=$bg] | 󱑂 %R#[fg=$fg,bg=$bg]    "
  setw window-status-format "#[fg=$blue,bg=$gray,italics] #I #[fg=$blue,bg=$bg,italics] #W #[fg=$fg,bg=$bg]"
  setw window-status-current-format "#[fg=$bg,bg=$yellow,bold] #I #[fg=$yellow,bg=$gray,bold] #W #[fg=$fg,bg=$bg]"
  setw clock-mode-colour "${cyan}"
  setw mode-style "fg=${cyan} bg=${gray} bold"
}

main "$@"
