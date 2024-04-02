#!/usr/bin/env bash

## @download_dir: <string:path> -- Path to a system directory to which fonts will be downloaded.
## @nerdfonts_git_url: <string:url> -- URL of the Git repository hosting font archives.
download_fonts() {
  local download_dir=$1
  local nerdfonts_git_url=$2
  printf "~~~~~ [INFO] Downloading Fira Code to %s\n" "$download_dir"
  curl --proto '=https' -# -sSLZO --output-dir "$download_dir" "$nerdfonts_git_url/FiraCode.tar.xz"
  printf "~~~~~ [INFO] Downloading JetBrainsMono to %s\n" "$download_dir"
  curl --proto '=https' -# -sSLZO --output-dir "$download_dir" "$nerdfonts_git_url/JetBrainsMono.tar.xz"
  printf "~~~~~ [INFO] Downloading Iosevka to %s\n" "$download_dir"
  curl --proto '=https' -# -sSLZO --output-dir "$download_dir" "$nerdfonts_git_url/Iosevka.tar.xz"
  printf "~~~~~ [INFO] Downloading IosevkaTerm to %s\n" "$download_dir"
  curl --proto '=https' -# -sSLZO --output-dir "$download_dir" "$nerdfonts_git_url/IosevkaTerm.tar.xz"
  printf "~~~~~ Done: Download.\n"
}

## @download_dir: <string:path> -- Path to a system directory to which fonts will be downloaded.
extract_fonts() {
  local download_dir=$1
  printf "~~~~~ [INFO] Extracting...\n"
  tar -xf "$download_dir/FiraCode.tar.xz" -C "$download_dir"
  tar -xf "$download_dir/JetBrainsMono.tar.xz" -C "$download_dir"
  tar -xf "$download_dir/Iosevka.tar.xz" -C "$download_dir"
  tar -xf "$download_dir/IosevkaTerm.tar.xz" -C "$download_dir"
  printf "~~~~~ Done: Extract.\n"
}

## @download_dir: <string:path> -- Path to a system directory to which fonts will be downloaded.
move_fonts() {
  local download_dir=$1
  # Fira Code
  if [[ -d /usr/share/fonts/FiraCodeNF ]]; then
    sudo mv "$download_dir"/FiraCode*.ttf /usr/share/fonts/FiraCodeNF
    printf "~~~~~ [INFO] Moved FiraCode to /usr/share/fonts/FiraCodeNF\n"
  else
    sudo mkdir /usr/share/fonts/FiraCodeNF
    printf "~~~~~ [INFO] Created /usr/share/fonts/FiraCodeNF"
    sudo mv "$download_dir"/FiraCode*.ttf /usr/share/fonts/FiraCodeNF
    printf "~~~~~ [INFO] Moved FiraCode to /usr/share/fonts/FiraCodeNF\n"
  fi
  # Jet Brains Mono
  if [[ -d /usr/share/fonts/JetBrainsNF ]]; then
    sudo mv "$download_dir"/JetBrainsMono*.ttf /usr/share/fonts/JetBrainsNF
    printf "~~~~~ [INFO] Moved JetBrains to /usr/share/fonts/JetBrainsNF\n"
  else
    sudo mkdir /usr/share/fonts/JetBrainsNF
    printf "~~~~~ [INFO] Created /usr/share/fonts/JetBrainsNF"
    sudo mv "$download_dir"/JetBrainsMono*.ttf /usr/share/fonts/JetBrainsNF
    printf "~~~~~ [INFO] Moved JetBrains to /usr/share/fonts/JetBrainsNF\n"
  fi
  # Iosevka
  if [[ -d /usr/share/fonts/IosevkaNF ]]; then
    sudo mv "$download_dir"/IosevkaNerdFont*.ttf /usr/share/fonts/IosevkaNF
    printf "~~~~~ [INFO] Moved Iosevka to /usr/share/fonts/IosevkaNF\n"
  else
    sudo mkdir /usr/share/fonts/IosevkaNF
    printf "~~~~~ [INFO] Created /usr/share/fonts/IosevkaNF"
    sudo mv "$download_dir"/IosevkaNerdFont*.ttf /usr/share/fonts/IosevkaNF
    printf "~~~~~ [INFO] Moved Iosevka to /usr/share/fonts/IosevkaNF\n"
  fi
  # Iosevka Term
  if [[ -d /usr/share/fonts/IosevkaNF/Term ]]; then
    sudo mv "$download_dir"/IosevkaTermNerdFont*.ttf /usr/share/fonts/IosevkaNF/Term
    printf "~~~~~ [INFO] Moved IosevkaTerm to /usr/share/fonts/IosevkaNF/Term\n"
  else
    sudo mkdir /usr/share/fonts/IosevkaNF/Term
    printf "~~~~~ [INFO] Created /usr/share/fonts/IosevkaNF/Term"
    sudo mv "$download_dir"/IosevkaTermNerdFont*.ttf /usr/share/fonts/IosevkaNF/Term
    printf "~~~~~ [INFO] Moved IosevkaTerm to /usr/share/fonts/IosevkaNF/Term\n"
  fi
}

# ------------------------------------------------------------------------------------------------ #

main() {
  printf "~~~~~ NerdFonts setup.\n"
  printf "~~~~~ GitHub: ryanoasis/nerd-fonts\n"
  # ~  Variables
  local NERDFONTS_GIT_URL="https://github.com/ryanoasis/nerd-fonts/releases/latest/download"
  local DIR_FONTS_DOWNLOAD="$HOME/Downloads/Fonts"
  if [[ -d $HOME/Downloads/Fonts ]]; then
    printf "~~~~~ [INFO] Found ~/Downloads/Fonts. Continuing\n"
  else
    mkdir -p "$DIR_FONTS_DOWNLOAD"
    printf "~~~~~ [INFO] Created ~/Downloads/Fonts.\n"
  fi
  # Main functions
  download_fonts "$DIR_FONTS_DOWNLOAD" $NERDFONTS_GIT_URL
  extract_fonts "$DIR_FONTS_DOWNLOAD"
  move_fonts "$DIR_FONTS_DOWNLOAD"
  # Re-cache system fonts
  fc-cache -v
  printf "\n~~~~~ Done. Fonts are set."
}

main
