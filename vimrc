set nocompatible

if has('vim_starting')
    set runtimepath+=$HOME/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'bling/vim-airline'
NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'othree/html5.vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'tpope/vim-surround'
NeoBundle 'Shougo/vimproc', {
            \ 'build' : {
            \     'unix' : 'make -f make_unix.mak',
            \    },
            \ }
NeoBundle 'xolox/vim-notes', { 'depends' :
            \ [ 'xolox/vim-misc' ]
            \ }

filetype plugin indent on

NeoBundleCheck

syntax on

set laststatus=2
set noshowmode
set backspace=indent,eol,start
set hidden
set number
set gdefault
set incsearch
set showmatch
set mouse=a
set ignorecase
set smartcase
set encoding=utf8
set termencoding=utf-8
set nobackup
set noswapfile
set t_Co=256
set scrolloff=3
set expandtab
set tabstop=4
set shiftwidth=4
set autoindent
set smartindent
set noerrorbells
set splitright
set splitbelow
set ttyfast
set wildmenu
set wildignorecase
set wildmode=longest,full
set cursorline
set clipboard=unnamedplus " share clipboard with Xorg
set wrapscan
set autochdir
set linebreak
set shortmess+=I
set conceallevel=2
set concealcursor=nc

let g:netrw_liststyle=3
let g:netrw_banner=0

let mapleader=","

" Sane colors for Conceal
autocmd ColorScheme * hi Conceal guibg=NONE gui=bold ctermbg=NONE cterm=bold
colorscheme Tomorrow-Night

" Haaardcoooore
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>
imap <Up> <nop>
imap <Down> <nop>
imap <Left> <nop>
imap <Right> <nop>

inoremap jj <ESC>
noremap <silent><leader>ev :tabedit $MYVIMRC<CR>
noremap <silent><leader>p :set paste!<CR>

autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

let g:airline_powerline_fonts = 1
let g:bufferline_solo_highlight = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:javascript_conceal=1
