vim9script

g:mapleader = " "

set shortmess+=Is
syntax on
filetype plugin indent on

# Editing
set expandtab
set tabstop=4
set softtabstop=-1
set shiftwidth=4
set smarttab
set autoindent
set smartindent
set wrap
set linebreak
set showbreak=- 
set textwidth=96
set showfulltag
set completeopt=menu,noinsert,longest,popup
set backspace=indent,eol,start
set virtualedit=block
set wildmenu
set wildmode=list:full
set wildoptions=fuzzy,pum

# UI
set termguicolors
set background=dark
set number
set relativenumber
set cursorline
set cursorlineopt=number
set showtabline=1
set notitle
set laststatus=3
set noshowcmd
set cmdheight=1
set showmatch
set switchbuf=useopen,uselast,usetab
set signcolumn
set noruler
set list
set conceallevel=2

# Behavior
set lazyredraw
set updatetime=200
set belloff=all
set splitright
set splitbelow
set scroll=10
set scrolloff=4
set sidescrolloff=4
set foldenable
set ignorecase
set smartcase
set hlsearch
set incsearch
set listchars=tab:→\ ,nbsp:␣,trail:·
set fillchars=fold:·,foldopen:,foldclose:,foldsep:\ ,diff:╱,eob:\ 
