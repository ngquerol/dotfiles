set nocompatible

" NeoBundle
if has('vim_starting')
    if has('win32') || has('win64')
        set runtimepath+=$VIM/vimfiles/bundle/neobundle.vim/
        call neobundle#rc(expand('$VIM/vimfiles/bundle/'))
    else
        set runtimepath+=$HOME/.vim/bundle/neobundle.vim/
        call neobundle#rc(expand('~/.vim/bundle/'))
    endif
endif

NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'bling/vim-airline'
NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'jiangmiao/auto-pairs'
NeoBundle 'junegunn/vim-easy-align'
NeoBundle 'kien/ctrlp.vim'
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

filetype plugin indent on

NeoBundleCheck

syntax enable

" Options
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
set clipboard=unnamed
set wrapscan
set autochdir
set linebreak
set conceallevel=2
set concealcursor=nc
set wildignore=.svn,CVS,.git,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,*.pdf,*.bak,*.beam
set suffixes+=.old

let g:netrw_liststyle=3
let g:netrw_banner=0

" Sane colors for Conceal
autocmd ColorScheme * hi Conceal guibg=#303030 gui=bold ctermbg=235 cterm=bold
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

" Leader key
let mapleader=","

inoremap jj <ESC>
noremap <silent><Leader>ev :tabedit $MYVIMRC<CR>
noremap <silent><Leader>p :set paste!<CR>

vnoremap <silent><Enter> :EasyAlign<Enter>

" Return to last edited place in file
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

" Plugins
let g:airline_powerline_fonts = 0
let g:airline_right_sep = ''
let g:airline_left_sep = ''
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#buffer_min_count = 2
let g:javascript_conceal = 1
