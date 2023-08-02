# ~  General, platform independent aliases

alias C="clear"
alias nv="nvim"
alias codi="$(which codium)"

alias dtfls="/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME"

alias l.="ls -A | egrep '^\.'"
alias la="ls -a"
alias ll="ls -la"

# List installed desktops
alias xd="ls /usr/share/xsessions"
alias xdw="ls /usr/share/wayland-sessions"

# ~  Aliases: apt

alias ad="sudo apt update"
alias ag="sudo apt upgrade"
alias auu="sudo apt update && sudo apt upgrade"
alias ai="sudo apt install"
alias ari="sudo apt reinstall"
alias ar="sudo apt remove"
alias aar="sudo apt autoremove"
alias ap="sudo apt purge"
alias ac="sudo apt clean"
alias aac="sudo apt autoclean"
alias as="sudo apt search"
alias ash="sudo apt show"
alias al="sudo apt list"

# apt-get
alias agd="sudo apt-get update"
alias agg="sudo apt-get upgrade"
alias aguu="sudo apt-get update && sudo apt-get upgrade"
alias agi="sudo apt-get install"
alias agri="sudo apt-get reinstall"
alias agr="sudo apt-get remove"
alias agp="sudo apt-get purge"
alias agar="sudo apt-get autoremove"
alias agssfy="sudo apt-get satisfy"
alias agc="sudo apt-get clean"
alias agac="sudo apt-get autoclean"
alias agchk="sudo apt-get check"
alias agsrc="sudo apt-get source"
alias agdl="sudo apt-get download"

# ~  Aliases: dnf

alias dnfu="sudo dnf upgrade --refresh"
alias dnfi="sudo dnf install"
alias dnfgi="sudo dnf groupinstall"
alias dnfr="sudo dnf remove"
alias dnfar="sudo dnf autoremove"
alias dnfgr="sudo dnf groupremove"
alias dnfc="sudo dnf clean all"
alias dnfs="sudo dnf search"
alias dnfl="sudo dnf list"
alias dfnli="sudo dnf list installed"
alias dnfgl="sudo dnf grouplist"