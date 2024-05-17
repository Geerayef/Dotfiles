#!/usr/bin/env bash

## @messages: string -- Message to notify.
## @severity: string -- Message severity.
notify() {
  local messages=$1
  local severity=$2
  printf "~~~~~ [%s] %s" "$messages" "$severity"
}

## @dir_destination: <string:path> -- Path to the destination directory to which fonts will be moved.
create_path() {
  local dir_destination=$1
  [[ -d $dir_destination ]] || sudo mkdir "$dir_destination"
  printf "~~~~~ [DONE] Create %s.\n" "$dir_destination"
}

## @dir_download: <string:path> -- Path to a directory where fonts will be downloaded.
## @url_git_nerdfonts: <string:url> -- URL of the Git repository hosting font archives.
## @fonts: ref <array<string>> -- Reference to array of font names to download.
download() {
  local dir_download=$1
  local url_git_nerdfonts=$2
  local -n fonts=$3
  for name in "${fonts[@]}"; do
    printf "~~~~~ [INFO] Downloading %s\n" "$name"
    curl --proto '=https' -# -sSLZO --output-dir "$dir_download" "$url_git_nerdfonts/${name}.tar.xz"
  done
  printf "~~~~~ [DONE] Download.\n"
}

## @dir_download: <string:path> -- Path to a directory where fonts will be downloaded.
## @fonts: ref <array<string>> -- Reference to array of font names to download.
extract() {
  local dir_download=$1
  local -n fonts=$2
  printf "~~~~~ [INFO] Extracting...\n"
  for name in "${fonts[@]}"; do
    tar -xf "$dir_download/${name}.tar.xz" -C "$dir_download"
  done
  printf "~~~~~ [DONE] Extract.\n"
}

## @dir_download: <string:path> -- Path to a directory where fonts will be downloaded.
## @fonts: ref <array<string>> -- Reference to array of font names to download.
move() {
  local dir_download=$1
  local -n fonts=$2
  for name in "${fonts[@]}"; do
    case $name in
      "IosevkaTerm")
        printf "~~~~~ [INFO] %s will be groupped with Iosevka.\n" "$name"
        ;;
      *)
        local dir_destination="/usr/share/fonts/TTF/${name}NF"
        create_path "$dir_destination"
        sudo mv "$dir_download"/"$name"*.ttf "$dir_destination"
        printf "~~~~~ [DONE] Move %s to %s\n" "$name" "$dir_destination"
        ;;
    esac
  done
}

# ------------------------------------------------------------------------------------------------ #

main() {
  printf "~~~~~ NerdFonts setup.\n"
  printf "~~~~~ GitHub: ryanoasis/nerd-fonts\n"

  local url_git_nerdfonts="https://github.com/ryanoasis/nerd-fonts/releases/latest/download"
  local dir_download_fonts="$HOME/Downloads/Fonts"
  local font_names=("Iosevka" "IosevkaTerm" "FiraCode" "JetBrainsMono" "ZedMono")

  [[ -d $dir_download_fonts ]] || mkdir "$dir_download_fonts"
  printf "~~~~~ [DONE] Create %s.\n" "$dir_download_fonts"

  download "$dir_download_fonts" "$url_git_nerdfonts" font_names
  extract "$dir_download_fonts" font_names
  move "$dir_download_fonts" font_names

  printf "~~~~~ [INFO] Caching fonts...\n"
  fc-cache -vr
  printf "\n~~~~~ [DONE] Fonts are set."
}

main
