#!/usr/bin/env fish

set NVIM_PRERELEASE_DIR $HOME/Software/neovim/

# curl -sSLZO -# --output-dir $NVIM_PRERELEASE_DIR https://github.com/neovim/neovim/releases/latest/download/nvim-linux64.tar.gz
curl -SLZO -# --output-dir $NVIM_PRERELEASE_DIR https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.tar.gz
tar -C $NVIM_PRERELEASE_DIR -xzf $NVIM_PRERELEASE_DIR/nvim-linux64.tar.gz
