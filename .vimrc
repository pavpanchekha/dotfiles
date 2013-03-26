" UTF8 is the way to go
set encoding=utf8

" Manage buffers
set hidden

" Modelines
set modeline

" Tabbing and folding defaults
set autoindent shiftwidth=4 tabstop=4 softtabstop=4 expandtab foldmethod=indent

" Some filetype-specific preferences
filetype plugin indent on
autocmd FileType c        set cindent 
autocmd FileType cpp      set cindent
autocmd FileType lisp     set nocindent
autocmd FileType java     set cindent
autocmd FileType python   set nocindent
autocmd FileType python   set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class,with
autocmd FileType tex,rst  set tw=72 spell
autocmd FileType plaintex set tw=72 spell
autocmd FileType xml      set foldmethod=indent ts=2 shiftwidth=2 softtabstop=2
autocmd FileType xhtml    set foldmethod=indent ts=2 shiftwidth=2 softtabstop=2
autocmd FileType html     set foldmethod=indent ts=2 shiftwidth=2 softtabstop=2
autocmd FileType txt      set tw=72 spell
autocmd FileType rst      set tw=72 spell

" Searching
set incsearch ignorecase smartcase

" Make backspace make sense in insert mode
set backspace=indent,eol,start

" Where am I in the file?
set ruler number showmode

" Some backups and directory defaults
set backup backupdir=/tmp directory=/tmp 

" Choice menu --- complete up to ambiguity
set wildmenu wildmode=list:longest

" Miscellaneous
set foldlevel=0 clipboard+=unnamed visualbell title tags=tags;/

" Shut up, Vim
set shortmess=atI

" Always 3 lines around cursor
set scrolloff=3

" Change directory to current file
autocmd BufEnter * lcd %:p:h

" No compiled files in directory listings
let g:explHideFiles='^\.,.*\.(py[co]|hi|o|out)$'

" Spell check toggle
function! ToggleSpell()
    if !exists("b:spell")
        setlocal spell spelllang=en_us
        let b:spell = 1
    else
        setlocal nospell
        unlet b:spell
    endif
endfunction
nmap <F4> :call ToggleSpell()<CR>
imap <F4> <Esc>:call ToggleSpell()<CR>a

" Autocomplete
imap <c-space> <c-x><c-o>

" My compile commands
nmap <F5> :! sh -c "~/bin/run %"<CR><CR>
nmap <F6> :! sh -c "~/bin/run -c %"<CR><CR>

runtime macros/matchit.vim

" Colors and syntax
set background=light
syntax on
colorscheme delek

" Comma is leader
let mapleader = ","

" Toolbars and other GUI stuff
set guioptions=acgimR
