set nocompatible 

if has('autocmd')
    filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
    syntax enable
endif

" Use :help 'option' to see the documentation for the given option.

set autoindent
set expandtab
set shiftwidth=4
set softtabstop=4
set backspace=indent,eol,start
set complete-=i
set smarttab

set nrformats-=octal

if !has('nvim') && &ttimeoutlen == -1
    set ttimeout
    set ttimeoutlen=100
endif

set incsearch
" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
    nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

set laststatus=2
set ruler
set wildmenu

if !&scrolloff
    set scrolloff=1
endif
if !&sidescrolloff
    set sidescrolloff=5
endif
set display+=lastline

if &encoding ==# 'latin1' && has('gui_running')
    set encoding=utf-8
endif

if &listchars ==# 'eol:$'
    set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

if v:version > 703 || v:version == 703 && has("patch541")
    set formatoptions+=j " Delete comment character when joining commented lines
endif

if has('path_extra')
    setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if &shell =~# 'fish$' && (v:version < 704 || v:version == 704 && !has('patch276'))
    set shell=/usr/bin/env\ bash
endif

set autoread

if &history < 1000
    set history=1000
endif
if &tabpagemax < 50
    set tabpagemax=50
endif
if !empty(&viminfo)
    set viminfo^=!
endif
set sessionoptions-=options
set viewoptions-=options

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^Eterm'
    set t_Co=16
endif

set background=dark

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
    runtime! macros/matchit.vim
endif

" Jump to the last known cursor position, if applicable.
if has("autocmd")
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

if empty(mapcheck('<C-U>', 'i'))
    inoremap <C-U> <C-G>u<C-U>
endif
if empty(mapcheck('<C-W>', 'i'))
    inoremap <C-W> <C-G>u<C-W>
endif

" vim:set ft=vim et sw=2:
