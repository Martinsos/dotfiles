set nocompatible " Don't be compatible with vi.
syntax on
filetype on
filetype plugin on

set ttyfast " Smoother/faster redrawing.

" Save ~(backup) and swap files into this directories.
set backupdir=~/vimtmp//,. " For backup files.
set directory=~/vimtmp//,. " For swap files.

" ESC in insert mode.
inoremap fd <esc>
" ESC in command mode.
cnoremap fd <C-C>

set visualbell " Blink cursor on error instead of beeping.

set tabstop=2     " Visual size of real tab, measured in spaces.
set softtabstop=2
set shiftwidth=2  " Visual size of indent, measured in spaces.
set expandtab     " Insert spaces instead of tab.
set smarttab

filetype indent on

set wrap " Wrap lines.
set showbreak=>\

set encoding=utf-8

set listchars=tab:>-,trail:~,extends:>,precedes:<
set list

set showmatch " Highlight matching parentheses.

set ruler " Show line and column number of the cursor on the right side of statusline.

set wildmode=longest,list

set ignorecase " Ignore case when searching.
set smartcase " When searching try to be smart about cases.
set hlsearch " Highlight search results.

