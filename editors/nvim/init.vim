let mapleader = "\<Space>"

call plug#begin()

Plug 'chriskempson/base16-vim'

Plug 'editorconfig/editorconfig-vim'

Plug 'stephpy/vim-yaml'
Plug 'toml-lang/toml'
Plug 'rust-lang/rust.vim'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

call plug#end()

let g:LanguageClient_serverCommands = {
\ 'rust': ['rust-analyzer'],
\ }

let g:deoplete#enable_at_startup = 1

nmap <leader>w :w<CR>
nnoremap <leader><leader> <c-^>
noremap <leader>s :Rg<cr>
noremap <leader>p :Files<cr>
noremap <leader>; :Buffers<cr>
vnoremap <C-h> :nohlsearch<cr>
nnoremap <C-h> :nohlsearch<cr>

nnoremap <left> :bp<cr>
nnoremap <right> :bn<cr>

let $FZF_DEFAULT_COMMAND = 'rg --files --hidden'

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

if executable('rg')
  set grepprg=rg\ --no-heading\ --vimgrep
  set grepformat=%f:%l:%c:%m
endif

filetype plugin on
set omnifunc=syntaxcomplete#Complete
set completeopt+=menuone,noselect,noinsert

set cmdheight=2
set number          " show line number
set signcolumn=yes  " always show sign column

set mouse=a         " enable mouse usage
set tabstop=8       " 
set expandtab
set shiftwidth=4
set autoindent
set smartindent

let base16colorspace=256
colorscheme base16-atelier-dune-light

if has('nvim')
    runtime! plugin/python_setup.vim
endif
