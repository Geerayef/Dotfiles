# ~  General, platform independent aliases

alias L="clear"
alias nv="nvim"
alias code="$(which codium)"
alias dtfls="/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME"

alias l.="ls -A | egrep '^\.'"

# List installed desktops
alias xd="ls /usr/share/xsessions"
alias xdw="ls /usr/share/wayland-sessions"

# ~  Aliases: apt

alias as="sudo apt search"
alias ash="sudo apt show"
alias al="sudo apt list"
alias ai="sudo apt install"
alias ari="sudo apt reinstall"
alias ad="sudo apt update"
alias ag="sudo apt upgrade"
alias auu="sudo apt update && sudo apt upgrade"
alias ar="sudo apt remove"
alias aar="sudo apt autoremove"

# ~  Aliases: dnf

alias dnfu="sudo dnf update --refresh"
