#!/usr/bin/env fish

function notify -d "Log messages to stdio."
    set -l severity $argv[1]
    set -l message $argv[2]
    printf "~~~~~ [%s] %s\n" $severity $message
end

function create_dir -d "Create directory at given path, unless it already exists."
    set -l dir_destination $argv[1]
    set -l elevated $argv[2]
    if test $elevated = true
        test -d $dir_destination || sudo mkdir $dir_destination
    else
        test -d $dir_destination || mkdir $dir_destination
    end
    notify DONE "Create $dir_destination."
end

function download -d "Download fonts."
    set -l dir_download $argv[1]
    set -l url_git_nerdfonts $argv[2]
    set -l fonts $argv[3..]
    for font in $fonts
        notify INFO "Downloading $font."
        curl --proto '=https' -\# -sSLZO --output-dir $dir_download "$url_git_nerdfonts/$font.tar.xz"
    end
    notify DONE "Download."
end

function extract -d "Extract fonts."
    set -l dir_download $argv[1]
    set -l fonts $argv[2..]
    notify INFO "Extracting."
    for font in $fonts
        switch $font
            case NerdFontsSymbolsOnly
                tar -xf "$dir_download/NerdFontsSymbolsOnly.tar.xz" -C $dir_download
            case '*'
                tar -xf "$dir_download/$font.tar.xz" -C $dir_download
        end
    end
    notify DONE "Extraction."
end

function move -d "Move fonts from download to destination directory."
    set -l dir_download $argv[1]
    set -l dir_destination $argv[2]
    set -l fonts $argv[3..]
    create_dir $dir_destination true
    for font in $fonts
        switch $font
            case IosevkaTerm
                notify INFO "IosevkaTerm will be groupped with Iosevka."
            case NerdFontsSymbolsOnly
                set -l dir_font "$dir_destination/SymbolsNF"
                create_dir $dir_font true
                sudo mv "$dir_download"/SymbolsNerdFont*.ttf $dir_font
                notify DONE "Move $font to $dir_font"
            case '*'
                set -l dir_font "$dir_destination/$font"NF
                create_dir $dir_font true
                sudo mv "$dir_download/$font"*.ttf $dir_font
                notify DONE "Move $font to $dir_font"
        end
    end
end

function main
    printf "~~~~~ NerdFonts setup.\n"
    printf "~~~~~ GitHub: ryanoasis/nerd-fonts.\n\n"
    set -l fonts Iosevka IosevkaTerm FiraCode JetBrainsMono ZedMono NerdFontsSymbolsOnly
    set -l dir_download "$HOME/Downloads/Fonts"
    set -l dir_destination /usr/share/fonts/TTF
    set -l url_git_nerdfonts "https://github.com/ryanoasis/nerd-fonts/releases/latest/download"
    create_dir $dir_download false
    download $dir_download $url_git_nerdfonts $fonts
    extract $dir_download $fonts
    move $dir_download $dir_destination $fonts
    notify INFO "Caching fonts."
    fc-cache -rv
    notify DONE "Fonts are set."
end

main
