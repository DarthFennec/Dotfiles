" Plugins
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'sjl/gundo.vim'
Plugin 'dyng/ctrlsf.vim'
Plugin 'ciaranm/detectindent'
Plugin 'Townk/vim-autoclose'
Plugin 'vim-scripts/closetag.vim'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'tomasr/molokai'
call vundle#end()
filetype plugin indent on

" Syntax Hilighting
syntax enable
set background=dark
colorscheme molokai
let g:indent_guides_auto_colors=0
augroup indentgroup
autocmd!
autocmd BufRead * :DetectIndent
autocmd VimEnter * :IndentGuidesEnable
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=234 ctermfg=59
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=235 ctermfg=59
autocmd BufNewFile,BufRead *.gradle set filetype=groovy
autocmd bufreadpre COMMIT_EDITMSG setlocal textwidth=72
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
set fillchars+=vert:\ 
set statusline=%<%t\ %y%h%m%r%=%-14.(%l,%c%V%)\ %P

" Show Whitespace
set list
set listchars=tab:▶\ ,trail:·

" Indent Options
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent

" Search Options
set incsearch
set hlsearch
set ignorecase
set smartcase

" Miscellaneous Options
set hidden
set ttyfast
set nojoinspaces
set showtabline=0
set matchpairs+=<:>
set directory=~/.vim/.swap//
command! W w ! sudo tee % > /dev/null

" CtrlP Options
let g:ctrlp_user_command =
  \ ['.git', 'cd %s && git ls-files -co --exclude-standard --full-name',
  \ 'ag %s -l --nocolor -g ""']

" Leader Bindings
let mapleader=","
nnoremap <leader>s :CtrlSF 
nnoremap <silent> <leader>f :CtrlP<cr>
nnoremap <silent> <leader>b :CtrlPBuffer<cr>
nnoremap <silent> <leader>a :CtrlSFOpen<cr>
nnoremap <silent> <leader>u :GundoToggle<cr>
nnoremap <silent> <leader>i :DetectIndent<cr>
nnoremap <silent> <leader>w /[^\x00-\x7f]<cr>
nnoremap <silent> <leader>c :split $MYVIMRC<cr>
nnoremap <silent> <leader>r :source $MYVIMRC<cr>

" Custom Rebindings
nnoremap <tab> :nohlsearch<cr>:<bs>
nnoremap n nzz
nnoremap N Nzz
nnoremap H H0
nnoremap M M0
nnoremap L L0
inoremap jk <esc>
inoremap <c-g> <esc>
noremap <c-g> <esc>
noremap ; :
noremap <space> ;
noremap Y y$
noremap <c-j> <c-d>0
noremap <c-k> <c-u>0
