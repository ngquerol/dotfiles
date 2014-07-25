filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'Shougo/neocomplete.vim'
Plugin 'Shougo/neosnippet-snippets'
Plugin 'Shougo/neosnippet.vim'
Plugin 'bling/vim-airline'
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
Plugin 'othree/html5.vim'
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
set noshowmode
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
let &showbreak='↪ '
set linebreak
set laststatus=2

set t_Co=256
colorscheme Tomorrow-Night

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

noremap <silent><Leader>r :CtrlPMRU<CR>
nnoremap <silent><Leader>o :CtrlP<CR>
let g:ctrlp_use_caching=0
let g:ctrlp_open_new_file='r'
let g:ctrlp_open_multiple_files='2vjr'
if executable("ag")
    set grepprg=ag\ --smart-case\ --nogroup\ --nocolor
    let g:ctrlp_user_command='ag %s --files-with-matches --nocolor -g ""'
endif

let g:neocomplete#enable_at_startup=1
let g:neocomplete#enable_smart_case=1
let g:neocomplete#max_list=20

set conceallevel=2 concealcursor=i

" <CR>: close popup and save indent.
inoremap <silent><CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
    " For no inserting <CR> key.
    return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction

imap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ? "\<C-n>" : "\<TAB>")
smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
imap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
smap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
