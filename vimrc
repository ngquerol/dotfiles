filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'SirVer/ultisnips'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'honza/vim-snippets'
Plugin 'jeetsukumaran/vim-buffergator'
Plugin 'jiangmiao/auto-pairs'
Plugin 'ChrisKempson/Vim-Tomorrow-Theme'
Plugin 'kien/ctrlp.vim'
Plugin 'mattn/emmet-vim'
Plugin 'othree/html5.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'

call vundle#end()

filetype plugin indent on
syntax on

" Options
set backspace=indent,eol,start
set hidden
set incsearch
set wrapscan
set showmatch
set showcmd
set mouse=a
set nobackup
set noswapfile
set scrolloff=5
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

set splitright
set splitbelow
set ttyfast
set lazyredraw
set clipboard^=unnamedplus
set showbreak=↪\ 
set linebreak
set laststatus=2
set statusline=%<[%n]\ %F\ %m%r%y\ %{exists('g:loaded_fugitive')?fugitive#statusline():''}\ %=%-10.(%l,%c%V%)\ %P

set t_Co=256
colorscheme Tomorrow-Night-Eighties

" Leader key
let mapleader="\<Space>"

" Keybindings
inoremap jj <ESC>
noremap <silent><Leader>ev :tabedit $MYVIMRC<CR>
noremap <silent><Leader>n :set nu!<CR>

" Go back to last edited place when opening a file
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

" Plugins
runtime macros/matchit.vim

let g:buffergator_split_size=5
let g:buffergator_viewport_split_policy="B"
let g:buffergator_sort_regime="mru"
let g:buffergator_suppress_keymaps=1
let g:buffergator_autoexpand_on_split=0
nnoremap <silent><Leader>f :BuffergatorToggle<CR>

noremap <silent><Leader>r :CtrlPMRU<CR>
nnoremap <silent><Leader>o :CtrlP<CR>
let g:ctrlp_use_caching=0
let g:ctrlp_working_path_mode='a'
if executable("ag")
    set grepprg=ag\ --smart-case\ --nogroup\ --nocolor
    let g:ctrlp_user_command = 'ag %s --files-with-matches --nocolor -g ""'
endif

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
