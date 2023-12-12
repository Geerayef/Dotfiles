# ~  General

if [[ -r "$BASHDOTDIR/settings/alias.sh" ]] ; then
    source "$BASHDOTDIR/settings/alias.sh"
else
    alias C="clear"
    if [[ -e "$(command -v eza)" ]] ; then
        alias ls="eza -a --color=always --icons=always --group-directories-first"
        alias la="eza -aG --color=always --icons=always --group-directories-first"
        alias ll="eza -la --color=always --icons=always --group-directories-first"
        alias lT="eza -aT -L 2 --color=always --icons=always --group-directories-first"
        alias lt="eza -aT -L 1 --color=always --icons=always --group-directories-first"
        alias l.='eza -a --color=always --icons=always --group-directories-first | grep -E "^\."'
    elif [[ -e "$(command -v exa)" ]] ; then
        alias ls="exa -a --color=always --icons=always --group-directories-first"
        alias la="exa -aG --color=always --icons=always --group-directories-first"
        alias ll="exa -la --color=always --icons=always --group-directories-first"
        alias lT="exa -aT -L 2 --color=always --icons=always --group-directories-first"
        alias lt="exa -aT -L 1 --color=always --icons=always --group-directories-first"
        alias l.='exa -a --color=always --icons=always --group-directories-first | grep -E "^\."'
    else
        alias l.="ls -A | grep -E '^\.' --group-directories-first"
        alias la="ls -a --group-directories-first"
        alias ll="ls -la --group-directories-first"
    fi

    alias grep="grep --color=always"
    # List installed desktops
    alias lid="ls -al /usr/share/xsessions"
    alias lidw="ls -al /usr/share/wayland-sessions"

    # ~  Apps

    alias nv="nvim"
    [[ -e "$(command -v codium)" ]] && alias codi="$(which codium)"

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
    alias gl="git log"

    # ~  Package managers

    if [[ "$SYSTEM_PACKAGE_MANAGER" = apt ]] ; then
        # ~  apt
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
    elif [[ "$SYSTEM_PACKAGE_MANAGER" = dnf ]] ; then
        # ~  dnf
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
    elif [[ "$SYSTEM_PACKAGE_MANAGER" = pacman ]] ; then
        # ~  pacman
        alias pi="sudo pacman -S --needed"
        alias pu="sudo pacman -Syu"
        alias pr="sudo pacman -R"
        alias pq="sudo pacman -Q"
        alias ps="sudo pacman -Ss"
    else
        echo "Please set the SYSTEM_PACKAGE_MANAGER environment variable to the name of the package manager used by the system. [ e.g. apt / dnf / pacman ]"
    fi
fi
