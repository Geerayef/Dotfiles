#!/usr/bin/env bash

echo "~~~~~ Fonts setup"
echo "~~~~~ Using GitHub: ryanoasis/nerd-fonts\n"

DOWNLOAD_FONT_DIR=$HOME/Downloads/Fonts
if [ -d $HOME/Downloads/Fonts ] ; then
  echo "~~~~~ Found ~/Downloads/Fonts/. Continue\n"
else
  mkdir -p $DOWNLOAD_FONT_DIR
  echo "~~~~~ Created ~/Downloads/Fonts/\n"
fi

echo "~~~~~ Downloading Fira Code, JetBrains to ~/Downloads/Fonts\n"

curl --proto '=https' -# -sSLZO --output-dir $DOWNLOAD_FONT_DIR 'https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.zip'

curl --proto '=https' -# -sSLZO --output-dir $DOWNLOAD_FONT_DIR 'https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FiraCode.zip'
echo "~~~~~ Done: Download\n"

echo "~~~~~ Unzipping...\n"
unzip -d $DOWNLOAD_FONT_DIR $DOWNLOAD_FONT_DIR/JetBrainsMono.zip
unzip -d $DOWNLOAD_FONT_DIR $DOWNLOAD_FONT_DIR/FiraCode.zip
echo "~~~~~ Done: Unzip\n"

if [ -d /usr/share/fonts/JetBrainsNF ] ; then
  sudo mv $DOWNLOAD_FONT_DIR/JetBrainsMonoNerdFont*.ttf /usr/share/fonts/JetBrainsNF
  echo "~~~~~ Move JetBrains to /usr/share/fonts/JetBrainsNF\n"
else
  sudo mkdir /usr/share/fonts/JetBrainsNF
  echo "~~~~~ Created /usr/share/fonts/JetBrainsNF"
  sudo mv $DOWNLOAD_FONT_DIR/JetBrainsMonoNerdFont*.ttf /usr/share/fonts/JetBrainsNF
  echo "~~~~~ Moved JetBrains to /usr/share/fonts/JetBrainsNF\n"
fi

if [ -d /usr/share/fonts/FiraCodeNF ] ; then
  sudo mv $DOWNLOAD_FONT_DIR/FiraCodeNerdFont*.ttf /usr/share/fonts/FiraCodeNF
  echo "~~~~~ Moved FiraCode to /usr/share/fonts/FiraCodeNF\n"
else
  sudo mkdir /usr/share/fonts/FiraCodeNF
  echo "~~~~~ Created /usr/share/fonts/FiraCodeNF"
  sudo mv $DOWNLOAD_FONT_DIR/FiraCodeNerdFont*.ttf /usr/share/fonts/FiraCodeNF
  echo "~~~~~ Moved FiraCode to /usr/share/fonts/FiraCodeNF\n"
fi

fc-cache -v
echo "\n~~~~~ Fonts are set."
