" Plugins
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'rking/ag.vim'
Plugin 'sjl/gundo.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'tpope/vim-sleuth'
Plugin 'tomasr/molokai'
call vundle#end()
filetype plugin indent on

" Syntax Hilighting
syntax enable
colorscheme molokai
set background=dark
let g:indent_guides_auto_colors=0
augroup indentgroup
autocmd!
autocmd VimEnter * :IndentGuidesEnable
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=234 ctermfg=59
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=235 ctermfg=59
augroup END

" Line Size Marker
set textwidth=80
set colorcolumn=+1
highlight ColorColumn ctermbg=235

" UI Layout
set number
set cursorline
set wildmenu
set showmatch
set laststatus=2
set statusline=%<%t\ %y%h%m%r%=%-14.(%l,%c%V%)\ %P

" Show Whitespace
set list
set listchars=tab:»-,trail:·

" Indent Options
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent

" Search Options
set incsearch
set hlsearch
set ignorecase
set smartcase

" Backup Options
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup

" Miscellaneous Options
set hidden
set ttyfast
set matchpairs+=<:>
command! W w ! sudo tee % > /dev/null

" CtrlP Options
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_user_command = 'ag %s -al --nocolor -g ""'
nnoremap <silent> <c-o> :CtrlP<cr>

" Leader Bindings
let mapleader=","
nnoremap <silent> <leader>u :GundoToggle<cr>
nnoremap <silent> <leader>e :split $MYVIMRC<cr>
nnoremap <silent> <leader>r :source $MYVIMRC<cr>

" Custom Rebindings
nnoremap <tab> :nohlsearch<cr>:<bs>
nnoremap n nzz
inoremap jk <esc>
noremap ; :
noremap : ;
noremap Y y$

" Disable Arrow Keys
noremap  <up>    <nop>
noremap  <down>  <nop>
noremap  <left>  <nop>
noremap  <right> <nop>
inoremap <up>    <nop>
inoremap <down>  <nop>
inoremap <left>  <nop>
inoremap <right> <nop>
