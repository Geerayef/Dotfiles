#!/usr/bin/env fish

set -l NVIM_PRERELEASE_DIR $HOME/software/neovim

if not test -e $NVIM_PRERELEASE_DIR
    mkdir -p $NVIM_PRERELEASE_DIR
end

curl -SLZO -# --output-dir $NVIM_PRERELEASE_DIR https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.tar.gz
tar -C $NVIM_PRERELEASE_DIR -xzf $NVIM_PRERELEASE_DIR/nvim-linux64.tar.gz
