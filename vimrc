set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'majutsushi/tagbar'
Bundle 'jiangmiao/auto-pairs'
Bundle 'tpope/vim-fugitive'

filetype plugin indent on
syntax on

set t_Co=256
colorscheme grb256
set fillchars+=vert:\ 
set ttyfast
set wildmenu
set number
set relativenumber
set encoding=utf-8
set title
set cursorline
set laststatus=2
set backspace=indent,eol,start
set hidden

" Whitespace
set nowrap
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" Searching
set incsearch
set ignorecase
set smartcase
set hlsearch

" Remember last location in file
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \ exe "normal! g`\"" |
            \ endif

" Plugins
nnoremap <silent> <F1> :NERDTreeToggle<CR>
nnoremap <silent> <F2> :TagbarToggle<CR>

let g:NERDTreeDirArrows=1
let g:NERDTreeWinSize=20

let g:tagbar_iconchars = ['▾', '▸']
let g:tagbar_width = 25
let g:tagbar_autoshow_tag = 1
