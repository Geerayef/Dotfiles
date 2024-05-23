#!/usr/bin/env bash

main() {
  local stamux_dir
  stamux_dir="$HOME/.config/tmux/plugins/stamuxline"

  get-tmux-option() {
    local option value default
    option="$1"
    default="$2"
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

  local theme
  theme="$(get-tmux-option "@stamux_color" "kanagawa")"
  source "${stamux_dir}/${theme}.tmuxtheme"

  # status
  seto status "on"
  seto status-bg "${thm_bg}"
  seto status-justify "left"
  seto status-left-length "100"
  seto status-right-length "100"
  # messages
  seto message-style "fg=${thm_cyan},bg=${thm_gray},align=centre"
  seto message-command-style "fg=${thm_cyan},bg=${thm_gray},align=centre"
  # panes
  seto pane-border-style "fg=${thm_gray}"
  seto pane-active-border-style "fg=${thm_blue}"
  # windows
  setw window-status-activity-style "bold"
  setw window-status-bell-style "bold"
  setw window-status-separator ""
  setw window-status-style "fg=${thm_fg},bg=${thm_bg},none"
  # clock
  seto clock-mode-style 24

  seto status-left "#[fg=$thm_fg]|   #{?client_prefix,#[bg=$thm_blue],#[bg=$thm_bg]}#{?client_prefix,#[fg=$thm_bg],#[fg=$thm_yellow,bold]} #S #[fg=$thm_fg,bg=$thm_bg] | "
  seto status-right "#[fg=$thm_fg,bg=$thm_bg] | 󱑂  %R#[fg=$thm_fg,bg=$thm_bg] |"
  setw window-status-format "#[fg=$thm_blue,bg=$thm_gray,italics] #I #[fg=$thm_blue,bg=$thm_bg,italics] #W #[fg=$thm_fg,bg=$thm_bg]"
  setw window-status-current-format "#[fg=$thm_bg,bg=$thm_yellow,bold] #I #[fg=$thm_yellow,bg=$thm_gray,bold] #W #[fg=$thm_fg,bg=$thm_bg]"
  setw clock-mode-colour "${thm_cyan}"
  setw mode-style "fg=${thm_cyan} bg=${thm_gray} bold"
}

main "$@"
