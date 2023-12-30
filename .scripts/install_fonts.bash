#!/usr/bin/env bash

echo "~~~~~ Fonts setup."
printf "~~~~~ GitHub: ryanoasis/nerd-fonts\n"

# -------------------------------------------------------------------------------- #

# ~  Variables

DOWNLOAD_FONT_DIR=$HOME/Downloads/Fonts
if [[ -d $HOME/Downloads/Fonts ]] ; then
  printf "~~~~~ Found ~/Downloads/Fonts/. Continue\n"
else
  mkdir -p "$DOWNLOAD_FONT_DIR"
  printf "~~~~~ Created ~/Downloads/Fonts/\n"
fi

# -------------------------------------------------------------------------------- #

# ~  Downloading

printf "~~~~~ Downloading Fira Code to ~/Downloads/Fonts\n"

curl --proto '=https' -# -sSLZO --output-dir "$DOWNLOAD_FONT_DIR" 'https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FiraCode.tar.xz'

printf "~~~~~ Downloading JetBrainsMono to ~/Downloads/Fonts\n"

curl --proto '=https' -# -sSLZO --output-dir "$DOWNLOAD_FONT_DIR" 'https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.tar.xz'

printf "~~~~~ Downloading Hasklug to ~/Downloads/Fonts\n"

curl --proto '=https' -# -sSLZO --output-dir "$DOWNLOAD_FONT_DIR" 'https://github.com/ryanoasis/nerd-fonts/releases/latest/download/Hasklig.tar.xz'

printf "~~~~~ Downloading Iosevka to ~/Downloads/Fonts\n"

curl --proto '=https' -# -sSLZO --output-dir "$DOWNLOAD_FONT_DIR" 'https://github.com/ryanoasis/nerd-fonts/releases/latest/download/Iosevka.tar.xz'

printf "~~~~~ Done: Download.\n"

# -------------------------------------------------------------------------------- #

printf "~~~~~ Uncompressing...\n"
tar -xf "$DOWNLOAD_FONT_DIR/FiraCode.tar.xz"      -C "$DOWNLOAD_FONT_DIR"
tar -xf "$DOWNLOAD_FONT_DIR/JetBrainsMono.tar.xz" -C "$DOWNLOAD_FONT_DIR"
tar -xf "$DOWNLOAD_FONT_DIR/Hasklig.tar.xz"       -C "$DOWNLOAD_FONT_DIR"
tar -xf "$DOWNLOAD_FONT_DIR/Iosevka.tar.xz"       -C "$DOWNLOAD_FONT_DIR"
printf "~~~~~ Done: Uncompress.\n"

if [[ -d /usr/share/fonts/JetBrainsNF ]] ; then
  sudo mv "$DOWNLOAD_FONT_DIR/JetBrainsMono*.ttf" /usr/share/fonts/JetBrainsNF
  printf "~~~~~ Move JetBrains to /usr/share/fonts/JetBrainsNF\n"
else
  sudo mkdir /usr/share/fonts/JetBrainsNF
  printf "~~~~~ Created /usr/share/fonts/JetBrainsNF"
  sudo mv "$DOWNLOAD_FONT_DIR/JetBrainsMono*.ttf" /usr/share/fonts/JetBrainsNF
  printf "~~~~~ Moved JetBrains to /usr/share/fonts/JetBrainsNF\n"
fi

if [[ -d /usr/share/fonts/FiraCodeNF ]] ; then
  sudo mv "$DOWNLOAD_FONT_DIR/FiraCode*.ttf" /usr/share/fonts/FiraCodeNF
  printf "~~~~~ Moved FiraCode to /usr/share/fonts/FiraCodeNF\n"
else
  sudo mkdir /usr/share/fonts/FiraCodeNF
  printf "~~~~~ Created /usr/share/fonts/FiraCodeNF"
  sudo mv "$DOWNLOAD_FONT_DIR/FiraCode*.ttf" /usr/share/fonts/FiraCodeNF
  printf "~~~~~ Moved FiraCode to /usr/share/fonts/FiraCodeNF\n"
fi

[[ ! -d /usr/share/fonts/TTF ]] && sudo mkdir /usr/share/fonts/TTF
printf "~~~~~ Created /usr/share/fonts/TTF"

if [[ -d /usr/share/fonts/TTF/Hasklug ]] ; then
  sudo mv "$DOWNLOAD_FONT_DIR/Hasklug*.otf" /usr/share/fonts/TTF/Hasklug
  printf "~~~~~ Moved Hasklug to /usr/share/fonts/TTF/Hasklug\n"
else
  sudo mkdir /usr/share/fonts/TTF/Hasklug
  printf "~~~~~ Created /usr/share/fonts/TTF/Hasklug"
  sudo mv "$DOWNLOAD_FONT_DIR/Hasklug*.otf" /usr/share/fonts/TTF/Hasklug
  printf "~~~~~ Moved Hasklug to /usr/share/fonts/TTF/Hasklug\n"
fi

if [[ -d /usr/share/fonts/TTF/Iosevka ]] ; then
  sudo mv "$DOWNLOAD_FONT_DIR/Iosevka*.ttf" /usr/share/fonts/TTF/Iosevka
  printf "~~~~~ Moved Iosevka to /usr/share/fonts/TTF/Iosevka\n"
else
  sudo mkdir /usr/share/fonts/TTF/Iosevka
  printf "~~~~~ Created /usr/share/fonts/TTF/Iosevka"
  sudo mv "$DOWNLOAD_FONT_DIR/Iosevka*.ttf" /usr/share/fonts/TTF/Iosevka
  printf "~~~~~ Moved Iosevka to /usr/share/fonts/TTF/Iosevka\n"
fi

fc-cache -v
printf "\n~~~~~ Fonts are set."
