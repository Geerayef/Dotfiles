# ~  General

alias C="clear"
alias grep="grep --color=always"
alias tlmgr="/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode"
if test -e "$(command -v eza)"
    alias ls="eza -a --color=always --icons=always --group-directories-first"
    alias la="eza -aG --color=always --icons=always --group-directories-first"
    alias ll="eza -la --color=always --icons=always --group-directories-first"
    alias lT="eza -aT -L 2 --color=always --icons=always --group-directories-first"
    alias lt="eza -aT -L 1 --color=always --icons=always --group-directories-first"
    alias l.="eza -af --color=always --icons=always --group-directories-first"
else if test -e "$(command -v exa)"
    alias ls="exa -a --color=always --icons=always --group-directories-first"
    alias la="exa -aG --color=always --icons=always --group-directories-first"
    alias ll="exa -la --color=always --icons=always --group-directories-first"
    alias lT="exa -aT -L 2 --color=always --icons=always --group-directories-first"
    alias lt="exa -aT -L 1 --color=always --icons=always --group-directories-first"
    alias l.="exa -af --color=always --icons=always --group-directories-first"
else
    alias ls="ls -a --color=always --group-directories-first"
    alias l.="ls -A | grep -E '^\.' --group-directories-first"
    alias la="ls -a --color=always --group-directories-first"
    alias ll="ls -la --color=always --group-directories-first"
end

alias lidx="ls -al /usr/share/xsessions"
alias lidw="ls -al /usr/share/wayland-sessions"

# ~  Apps

alias v="nvim"
alias nv="$HOME/software/neovim/nvim-linux64/bin/nvim"
if test -e "$(command -v codium)"
    alias codi="$(which codium)"
end

# ~  Git

alias dot="/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME"
alias gs="git status ."
alias ga="git add"
alias ga.="git add ."
alias grs="git restore --staged"
alias gr="git restore"
alias gst="git stash"
alias gstp="git stash pop"
alias gc="git commit -m"
alias gf="git fetch"
alias gm="git merge"
alias gpl="git pull --rebase"
alias gps="git push --set-upstream"
alias gch="git checkout"
alias gb="git branch"
alias gl='git log --pretty="%h » ¦ « %aN » ¦ « %s » ¦ « %aD"'

# ~  Package managers

if test $SYSTEM_PACKAGE_MANAGER = apt
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
else if test $SYSTEM_PACKAGE_MANAGER = dnf
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
    alias dnfh="dnf help"
else if test $SYSTEM_PACKAGE_MANAGER = pacman
    alias pi="sudo pacman -Sy --needed"
    alias pu="sudo pacman -Syu"
    alias pr="sudo pacman -Rs"
    alias pq="sudo pacman -Q"
    alias ps="sudo pacman -Sys"
else
    echo "Please set the SYSTEM_PACKAGE_MANAGER environment variable to the name of your system package manager."
end
