" Behavior
set hidden backspace=indent,eol,start encoding=utf8
set clipboard+=unnamed shortmess=atI 
set backup backupdir=/tmp directory=/tmp 
autocmd BufEnter * lcd %:p:h

" Display
set scrolloff=3 ruler number
syntax on

" Tabbing, folding, searching
set autoindent shiftwidth=4 tabstop=4 softtabstop=4 expandtab
set foldmethod=indent foldlevel=0
set incsearch ignorecase smartcase

" My compile commands
nmap <F5> :! sh -c "~/bin/run %"<CR><CR>
nmap <F6> :! sh -c "~/bin/run -c %"<CR><CR>
