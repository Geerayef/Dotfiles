#!/usr/bin/env bash

# NOTE: Needs "$NNN_FIFO" to work
# Previews are shown in: tmux / wezterm / $NNN_TERMINAL (additional arguments are supported)
# If "NNN_PREVIEWIMGPROG" else "iTerm2 image protocol"

# ~  Configurable environment variables

NNN_SPLIT=${NNN_SPLIT:-}
NNN_TERMINAL=${NNN_TERMINAL:-}
NNN_SPLITSIZE=${NNN_SPLITSIZE:-50}
TMPDIR=${TMPDIR:-/tmp}
ENVVARS=(
    "NNN_SCOPE=${NNN_SCOPE:-0}"
    "NNN_PISTOL=${NNN_PISTOL:-0}"
    "NNN_ICONLOOKUP=${NNN_ICONLOOKUP:-0}"
    "NNN_PAGER=${NNN_PAGER:-less -P?n -R -C}"
    "NNN_BATTHEME=${NNN_BATTHEME:-ansi}"
    "NNN_BATSTYLE=${NNN_BATSTYLE:-numbers}"
    "NNN_PREVIEWWIDTH=${NNN_PREVIEWWIDTH:-1920}"
    "NNN_PREVIEWHEIGHT=${NNN_PREVIEWHEIGHT:-1080}"
    "NNN_PREVIEWDIR=${NNN_PREVIEWDIR:-$TMPDIR/nnn/previews}"
    "NNN_PREVIEWIMGPROG=${NNN_PREVIEWIMGPROG:-}"
    "NNN_PREVIEWVIDEO=${NNN_PREVIEWVIDEO:-}"
)

# ~  Non-configurable environment variables

NNN_PARENT=${NNN_FIFO#*.}
[ "$NNN_PARENT" -eq "$NNN_PARENT" ] 2>/dev/null || NNN_PARENT=""
ENVVARS+=(
    "PWD=$PWD"
    "PATH=$PATH"
    "NNN_FIFO=$NNN_FIFO"
    "FIFOPID=$TMPDIR/nnn-preview-tui-fifopid.$NNN_PARENT"
    "FIFOPATH=$TMPDIR/nnn-preview-tui-fifo.$NNN_PARENT"
    "PREVIEWPID=$TMPDIR/nnn-preview-tui-previewpid.$NNN_PARENT"
    "CURSEL=$TMPDIR/nnn-preview-tui-selection.$NNN_PARENT"
    "FIFO_UEBERZUG=$TMPDIR/nnn-preview-tui-ueberzug-fifo.$NNN_PARENT"
    "POSOFFSET=$TMPDIR/nnn-preview-tui-posoffset"
)

trap '' PIPE
exists() { type "$1" >/dev/null 2>&1 ;}
pkill() { command pkill "$@" >/dev/null 2>&1 ;}
prompt() { clear; printf "%b" "$@"; cfg=$(stty -g); stty raw -echo; head -c 1; stty "$cfg" ;}
pidkill() {
    if [ -f "$1" ]; then
        PID="$(cat "$1" 2>/dev/null)" || return 1
        kill "$PID" >/dev/null 2>&1
        RET=$?
        wait "$PID" 2>/dev/null
        return $RET
    fi
    return 1
}

start_preview() {
    if [ -e "${TMUX%%,*}" ] && tmux -V | grep -q '[ -][3456789]\.'; then
        NNN_TERMINAL=tmux
        exists mpv && tmux display -p '#{client_termfeatures}' | grep -q 'sixel' && ENVVARS+=("NNN_PREVIEWVIDEO=sixel")
    elif [ -n "$WEZTERM_PANE" ]; then
        NNN_TERMINAL=wezterm
        exists mpv && ENVVARS+=("NNN_PREVIEWVIDEO=kitty")
    else
        NNN_TERMINAL="${NNN_TERMINAL:-alacritty}"
    fi

    if [ -z "$NNN_SPLIT" ] && [ $(($(tput lines) * 2)) -gt "$(tput cols)" ]; then
        NNN_SPLIT='h'
    elif [ "$NNN_SPLIT" != 'h' ]; then
        NNN_SPLIT='v'
    fi

    ENVVARS+=("NNN_TERMINAL=$NNN_TERMINAL" "NNN_SPLIT=$NNN_SPLIT" "QLPATH=$2" "PREVIEW_MODE=1")
    case "$NNN_TERMINAL" in
        tmux)
            # tmux splits are inverted
            ENVVARS=("${ENVVARS[@]/#/-e}")
            if [ "$NNN_SPLIT" = "v" ]; then split="h"; else split="v"; fi
            tmux split-window -l"$NNN_SPLITSIZE"% "${ENVVARS[@]}" -d"$split" -p"$NNN_SPLITSIZE" "$0" "$1"
            ;;
        wezterm)
            export "${ENVVARS[@]}"
            if [ "$NNN_SPLIT" = "v" ]; then split="--horizontal"; else split="--bottom"; fi
            wezterm cli split-pane --cwd "$PWD" $split --percent "$NNN_SPLITSIZE" "$0" "$1" >/dev/null
            wezterm cli activate-pane-direction Prev
            ;;
        alacritty)
            alacritty "$1" >/dev/null
            ;;
    esac
}

toggle_preview() {
    export "${ENVVARS[@]}"
    if pidkill "$FIFOPID"; then
        [ -p "$NNN_PPIPE" ] && printf "0" > "$NNN_PPIPE"
        pidkill "$PREVIEWPID"
        if [ -n "$QLPATH" ] && stat "$1"; then
            f="$(wslpath -w "$1")" && "$QLPATH" "$f" &
        fi
    else
        [ -p "$NNN_PPIPE" ] && printf "1" > "$NNN_PPIPE"
        start_preview "$1" "$QLPATH"
    fi
}

fifo_pager() {
    cmd="$1"
    shift
    # We use a FIFO to access $NNN_PAGER PID in jobs control
    mkfifo "$FIFOPATH" || return
    $NNN_PAGER < "$FIFOPATH" &
    printf "%s" "$!" > "$PREVIEWPID"
    (
        exec > "$FIFOPATH"
        if [ "$cmd" = "pager" ]; then
            if exists bat; then
                bat --terminal-width="$cols" "$@" &
            else
                $NNN_PAGER "$@" &
            fi
        else
            "$cmd" "$@" &
        fi
    )
    rm "$FIFOPATH"
}

# Binary file: show file info inside the pager
print_bin_info() {
    printf -- "-------- \033[1;31mBinary file\033[0m --------\n"
    if exists mediainfo; then
        mediainfo "$1"
    else
        file -b "$1"
    fi
}

handle_mime() {
    case "$2" in
        image/jpeg)
            image_preview "$cols" "$lines" "$1"
            ;;
        image/gif)
            generate_preview "$cols" "$lines" "$1" "gif"
            ;;
        image/vnd.djvu)
            generate_preview "$cols" "$lines" "$1" "djvu"
            ;;
        image/*)
            generate_preview "$cols" "$lines" "$1" "image"
            ;;
        video/*)
            generate_preview "$cols" "$lines" "$1" "video"
            ;;
        audio/*)
            generate_preview "$cols" "$lines" "$1" "audio"
            ;;
        application/font*|application/*opentype|font/*)
            generate_preview "$cols" "$lines" "$1" "font"
            ;;
        */*office*|*/*document*|*/*msword|*/*ms-excel)
            generate_preview "$cols" "$lines" "$1" "office"
            ;;
        application/zip)
            fifo_pager unzip -l "$1"
            ;;
        text/troff)
            if exists man; then
                fifo_pager man -Pcat -l "$1"
            else
                fifo_pager pager "$1"
            fi
            ;;
        *)
            handle_ext "$1" "$3" "$4"
            ;;
    esac
}

handle_ext() {
    case "$2" in
        epub) generate_preview "$cols" "$lines" "$1" "epub" ;;
        pdf) generate_preview "$cols" "$lines" "$1" "pdf" ;;
        gz|bz2) fifo_pager tar -tvf "$1" ;;
        md)
            if exists glow; then
                fifo_pager glow -s dark "$1"
            elif exists lowdown; then
                fifo_pager lowdown -Tterm "$1"
            else
                fifo_pager pager "$1"
            fi
            ;;
        htm|html|xhtml)
            if exists w3m; then
                fifo_pager w3m "$1"
            elif exists lynx; then
                fifo_pager lynx "$1"
            elif exists elinks; then
                fifo_pager elinks "$1"
            else
                fifo_pager pager "$1"
            fi
            ;;
        7z|a|ace|alz|arc|arj|bz|cab|cpio|deb|jar|lha|lz|lzh|lzma|lzo|rar|rpm|rz|t7z|tar|tbz|tbz2|tgz|tlz|txz|tZ|tzo|war|xpi|xz|Z)
            if exists atool; then
                fifo_pager atool -l "$1"
            elif exists bsdtar; then
                fifo_pager bsdtar -tvf "$1"
            fi ;;
        *)
            if [ "$3" = "bin" ]; then
                fifo_pager print_bin_info "$1"
            else
                fifo_pager pager "$1"
            fi
            ;;
    esac
}

preview_file() {
    clear
    # Detecting the exact type of the file: the encoding, mime type, and extension in lowercase.
    encoding="$(file -bL --mime-encoding -- "$1")"
    mimetype="$(file -bL --mime-type -- "$1")"
    ext="${1##*.}"
    [ -n "$ext" ] && ext="$(printf "%s" "${ext}" | tr '[:upper:]' '[:lower:]')"
    lines=$(tput lines)
    cols=$(tput cols)

    # Otherwise, falling back to the defaults.
    if [ -d "$1" ]; then
        cd "$1" || return
        if [ "$NNN_ICONLOOKUP" -ne 0 ] && [ -f "$(dirname "$0")"/.iconlookup ]; then
            [ "$NNN_SPLIT" = v ] && BSTR="\n"
            # shellcheck disable=SC2012
            ls -F --group-directories-first | head -n "$((lines - 3))" | "$(dirname "$0")"/.iconlookup -l "$cols" -B "$BSTR" -b " "
        elif exists eza; then
            eza -laT -L 1 --color=always --icons=always --group-directories-first --no-permissions --no-user
        elif exists exa; then
            exa -laT -L 1 --colour=always --icons=always --group-directories-first --no-permissions --no-user
        elif exists tree; then
            fifo_pager tree --filelimit "$(find . -maxdepth 1 | wc -l)" -L 3 -C -F --dirsfirst --noreport
        else
            fifo_pager ls -laF --group-directories-first --color=always
        fi
        cd ..
    elif [ "${encoding#*)}" = "binary" ]; then
        handle_mime "$1" "$mimetype" "$ext" "bin"
    else
        handle_mime "$1" "$mimetype" "$ext"
    fi
}

generate_preview() {
    if [ -n "$QLPATH" ] && stat "$3"; then
        f="$(wslpath -w "$3")" && "$QLPATH" "$f" &
    elif [ -n "$NNN_PREVIEWVIDEO" ] && [[ "$4" == +(gif|video) ]]; then
        [ "$4" = "video" ] && args=(--start=10% --length=4) || args=()
        video_preview "$1" "$2" "$3" "${args[@]}" && return
    elif [ ! -f "$NNN_PREVIEWDIR/$3.jpg" ] || [ -n "$(find -L "$3" -newer "$NNN_PREVIEWDIR/$3.jpg")" ]; then
        mkdir -p "$NNN_PREVIEWDIR/${3%/*}"
        case $4 in
            audio) ffmpeg -i "$3" -filter_complex "scale=iw*min(1\,min($NNN_PREVIEWWIDTH/iw\,ih)):-1" "$NNN_PREVIEWDIR/$3.jpg" -y ;;
            epub) gnome-epub-thumbnailer "$3" "$NNN_PREVIEWDIR/$3.jpg" ;;
            font) fontpreview -i "$3" -o "$NNN_PREVIEWDIR/$3.jpg" ;;
            gif) 
                if [ -n "$NNN_PREVIEWVIDEO" ]; then
                    video_preview "$1" "$2" "$3" && return
                else
                    image_preview "$1" "$2" "$3" && return
                fi
                ;;
            image)
                if exists convert; then
                    convert "$3" -flatten -resize "$NNN_PREVIEWWIDTH"x"$NNN_PREVIEWHEIGHT"\> "$NNN_PREVIEWDIR/$3.jpg"
                else
                    image_preview "$1" "$2" "$3" && return
                fi
                ;;
            office)
                libreoffice --convert-to jpg "$3" --outdir "$NNN_PREVIEWDIR/${3%/*}"
                filename="$(printf "%s" "${3##*/}" | cut -d. -f1)"
                mv "$NNN_PREVIEWDIR/${3%/*}/$filename.jpg" "$NNN_PREVIEWDIR/$3.jpg" ;;
            pdf) pdftoppm -jpeg -f 1 -singlefile "$3" "$NNN_PREVIEWDIR/$3" ;;
            djvu) ddjvu -format=ppm -page=1 "$3" "$NNN_PREVIEWDIR/$3.jpg" ;;
            video) video_preview "$1" "$2" "$3" && return ;;
        esac
    fi
    if [ -f "$NNN_PREVIEWDIR/$3.jpg" ]; then
        image_preview "$1" "$2" "$NNN_PREVIEWDIR/$3.jpg"
    else
        fifo_pager print_bin_info "$3"
    fi
} >/dev/null 2>&1

image_preview() {
    clear
    exec >/dev/tty
    if [ "$NNN_TERMINAL" = "wezterm" ] && [[ "$NNN_PREVIEWIMGPROG" == +(|imgcat) ]]; then
        wezterm imgcat "$3" &
    elif exists catimg && [[ "$NNN_PREVIEWIMGPROG" == +(|catimg) ]]; then
        catimg "$3" &
    elif exists img2sixel && [[ "$NNN_PREVIEWIMGPROG" == +(|img2sixel) ]]; then
        img2sixel -g "$3" &
    else
        fifo_pager print_bin_info "$3" && return
    fi
    printf "%s" "$!" > "$PREVIEWPID"
}

video_preview() {
    clear
    exec >/dev/tty
    if [ -n "$NNN_PREVIEWVIDEO" ]; then
        mpv --no-config --really-quiet --vo="$NNN_PREVIEWVIDEO" --profile=sw-fast --loop-file --no-audio "$4" "$3" &
    else
        ffmpegthumbnailer -m -s0 -i "$3" -o "$NNN_PREVIEWDIR/$3.jpg" || rm "$NNN_PREVIEWDIR/$3.jpg" &
    fi
    printf "%s" "$!" > "$PREVIEWPID"
}

winch_handler() {
    clear
    pidkill "$PREVIEWPID"
    preview_file "$(cat "$CURSEL")"
}

preview_fifo() {
    while read -r selection; do
        if [ -n "$selection" ]; then
            pidkill "$PREVIEWPID"
            [ "$selection" = "close" ] && break
            preview_file "$selection"
            printf "%s" "$selection" > "$CURSEL"
        fi
    done < "$NNN_FIFO"
    # Make sure potential preview by winch_handler is killed
    sleep 0.1
    pkill -P "$$"
}

if [ "$PREVIEW_MODE" -eq 1 ] 2>/dev/null; then
    preview_file "$PWD/$1"
    preview_fifo & WAITPID=$!
    printf "%s" "$!" > "$FIFOPID"
    printf "%s" "$PWD/$1" > "$CURSEL"
    trap 'winch_handler' WINCH
    trap 'rm "$PREVIEWPID" "$CURSEL" "$FIFO_UEBERZUG" "$FIFOPID" "$POSOFFSET" 2>/dev/null' INT HUP EXIT
    while kill -s 0 $WAITPID; do
        wait $WAITPID 2>/dev/null
    done
    exit 0
else
    if [ ! -r "$NNN_FIFO" ]; then
        prompt "No FIFO available! (\$NNN_FIFO='$NNN_FIFO')\nPlease read Usage in '$0'."
    else
        toggle_preview "$1" &
    fi
fi
