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
set backspace=2
set hidden
set number
set gdefault
set incsearch
set showmatch
set showcmd
set mouse=a
set ignorecase
set smartcase
set encoding=utf8
set termencoding=utf-8
set nobackup
set noswapfile
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
set lazyredraw
set wildmenu
set wildignorecase
set wildmode=longest,full
set clipboard^=unnamed
set showbreak=↪\ 
set wrapscan
set linebreak
set conceallevel=2
set concealcursor=nc
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,*.pdf,*.bak,*.beam
set suffixes+=.old

let g:netrw_liststyle=3
let g:netrw_banner=0

colorscheme Tomorrow-Night

" GUI
if has('gui_running')
    set guifont=Dejavu\ Sans\ Mono\ 10.5

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

" Haaardcoooore
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>
imap <Up> <nop>
imap <Down> <nop>
imap <Left> <nop>
imap <Right> <nop>

" Leader key
let mapleader="\<Space>"

inoremap jj <ESC>
noremap <silent><Leader>ev :tabedit $MYVIMRC<CR>
noremap <silent><Leader>p :set paste!<CR>
noremap <silent><Leader>l :ls<CR>

" Return to last edited place in file
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

" Plugins
hi Conceal guibg=#303030 gui=bold ctermbg=235 cterm=bold
let g:javascript_conceal = 1

let g:ctrlp_map = "<Leader>o"
let g:ctrlp_lazy_update = 1
if executable("ag")
    set grepprg=ag\ --smart-case\ --nogroup\ --nocolor
    let g:ctrlp_user_command = 'ag %s --files-with-matches --nocolor -g ""'
endif
