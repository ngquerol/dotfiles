" NeoBundle
if has('vim_starting')
    set nocompatible
    set runtimepath+=$HOME/.vim/bundle/neobundle.vim
endif

call neobundle#rc(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'gregsexton/MatchTag'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'othree/html5.vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'tpope/vim-surround'

filetype plugin indent on

NeoBundleCheck

syntax enable

" Options
set backspace=indent,eol,start
set hidden
set incsearch
set wrapscan
set number
set showmatch
set showcmd
set mouse=a
set nobackup
set noswapfile
set scrolloff=3
set noerrorbells

set autoindent
set expandtab
set shiftround
set shiftwidth=4
set smarttab

set gdefault
set ignorecase
set smartcase

set encoding=utf8
set termencoding=utf-8

set wildmenu
set wildignore=*.swp,*.bak,*.pyc,*.class,*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignorecase
set wildmode=longest,full

let g:netrw_liststyle=3
let g:netrw_banner=0

set splitright
set splitbelow
set ttyfast
set lazyredraw
set clipboard^=unnamedplus
set showbreak=↪\ 
set linebreak

set conceallevel=2
set concealcursor=nc

colorscheme Tomorrow-Night

" GUI
if has('gui_running')
    set guifont=Inconsolata\ 13

    set guioptions-=r
    set guioptions-=L
    set guioptions-=T
    set guioptions-=m
    set guioptions+=c
    set guioptions-=e

    set lines=45 columns=90
    set guicursor+=a:blinkon0

    colorscheme Tomorrow-Night-Eighties
endif

" Leader key
let mapleader="\<Space>"

" Keybindings
inoremap jj <ESC>
noremap <silent><Leader>ev :tabedit $MYVIMRC<CR>
noremap <silent><Leader>p :set paste!<CR>
nnoremap gp :bp<CR>
nnoremap gn :bn<CR>
nnoremap gl :ls<CR>
nnoremap gb :ls<CR>:b

" Go back to last edited place when opening a file
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

" Plugins
runtime macros/matchit.vim

hi Conceal guibg=#303030 gui=bold ctermbg=235 cterm=bold
let g:javascript_conceal = 1

nnoremap <silent><Leader>r :CtrlPMRU<CR>
nnoremap <silent><Leader>o :CtrlP<CR>
let g:ctrlp_lazy_update = 1
let g:ctrlp_by_filename = 1
if executable("ag")
    set grepprg=ag\ --smart-case\ --nogroup\ --nocolor
    let g:ctrlp_user_command = 'ag %s --files-with-matches --nocolor -g ""'
endif
