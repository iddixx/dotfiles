au VimLeave * set guicursor=a:ver100 " beam cursor

syntax on
filetype on
filetype indent on
set number         
set relativenumber 
set cursorline     
set t_Co=256
set showcmd!
set cmdheight=1
set laststatus=2
set virtualedit=all
set undofile
set undodir=~/.vim/undo

colorscheme zaibatsu
call plug#begin()
Plug 'tpope/vim-surround'
Plug 'tommcdo/vim-exchange'
call plug#end()

"------- Colemak-DH mappings -------
" Up/down/left/right
nnoremap n h
vnoremap n h
onoremap n h
xnoremap n h

nnoremap u gk
vnoremap u gk
onoremap u gk
xnoremap u gk

nnoremap e gj
vnoremap e gj
onoremap e gj
xnoremap e gj

nnoremap gu k
vnoremap gu k
onoremap gu k
xnoremap gu k

nnoremap ge j
vnoremap ge j
onoremap ge j
xnoremap ge j

nnoremap i l
vnoremap i l
onoremap i l
xnoremap i l

nnoremap N H
onoremap N H
xnoremap N H

nnoremap U K
onoremap U K
xnoremap U K

nnoremap E J
onoremap E J
xnoremap E J

nnoremap I L
onoremap I L
xnoremap I L

nnoremap <C-u> <C-y>
onoremap <C-u> <C-y>
xnoremap <C-u> <C-y>

nnoremap <C-e> <C-e>
onoremap <C-e> <C-e>
xnoremap <C-e> <C-e>

" Word left/right
nnoremap a b
vnoremap a b
onoremap a b
xnoremap a b

nnoremap A B
vnoremap A B
onoremap A B
xnoremap A B

" Braces & Brackets movement
nnoremap x %
onoremap x %
xnoremap x %

" End of word left/right
nnoremap N ge
onoremap N ge
xnoremap N ge

nnoremap <C-n> gE
onoremap <C-n> gE
xnoremap <C-n> gE

nnoremap I e
onoremap I e
xnoremap I e

nnoremap <C-i> E
onoremap <C-i> E
xnoremap <C-i> E

" Move visual replace from 'r' to 'R'
onoremap R r
vnoremap R r

" Copy/paste/delete/cut
nnoremap y "+y
onoremap y "+y
xnoremap y "+y
vnoremap y "+y

nnoremap p "+p
onoremap p "+p
xnoremap p "+p
vnoremap p "+p

nnoremap d "_d
onoremap d "_d
xnoremap d "_d
vnoremap d "_d

nnoremap c "+c
onoremap c "+c
xnoremap c "+c
vnoremap c "+c

nnoremap dc "_x
onoremap dc "_x
xnoremap dc "_x
vnoremap dc "_x

nnoremap dC "_X
onoremap dC "_X
xnoremap dC "_X
vnoremap dC "_X

nnoremap dd "_dd
onoremap dd "_dd
xnoremap dd "_dd
vnoremap dd "_dd

" fixes                 
nnoremap v v
onoremap v v
xnoremap v v
vnoremap v v

nnoremap ci "+ci
onoremap ci "+ci
xnoremap ci "+ci

nnoremap cc "+cc
onoremap cc "+cc
xnoremap cc "+cc

nnoremap yi "+yi
onoremap yi "+yi
xnoremap yi "+yi

nnoremap yy "+yy
onoremap yy "+yy
xnoremap yy "+yy

nnoremap vi vi
onoremap vi vi
xnoremap vi vi

nnoremap di "_di
onoremap di "_di
xnoremap di "_di
vnoremap di "_di

nnoremap ca "+ca
onoremap ca "+ca
xnoremap ca "+ca
vnoremap ca "+ca

nnoremap ya "+ya
onoremap ya "+ya
xnoremap ya "+ya
vnoremap ya "+ya

nnoremap va va
onoremap va va
xnoremap va va

nnoremap da "_da
onoremap da "_da
xnoremap da "_da
vnoremap da "_da

nnoremap gd gd
onoremap gd gd
xnoremap gd gd

onoremap i i

onoremap a a

" Undo/redo
nnoremap l u

nnoremap gz U

nnoremap L <C-r>

" insert/append (T)
nnoremap j i

nnoremap J I

nnoremap b a

nnoremap B A

" Insert in Visual mode
vnoremap J I

vnoremap B A

" Search
nnoremap k n
onoremap k n
xnoremap k n

nnoremap K N
onoremap K N
xnoremap K N

" Window navigation
nnoremap <C-w>n <C-w>h

nnoremap <C-w>u <C-w>k

nnoremap <C-w>e <C-w>j

nnoremap <C-w>i <C-w>l

nnoremap <C-w>N <C-w>H

nnoremap <C-w>U <C-w>K

nnoremap <C-w>E <C-w>J

nnoremap <C-w>I <C-w>L

tnoremap <C-w>t <C-\><C-n>
inoremap <C-w>t <C-\><C-n>

"other
" Note: Disable <C-z> (suspend Vim)
nnoremap <C-z> <Nop>

nnoremap ; q:

nnoremap g\| g~
vnoremap g\| g~
onoremap g\| g~
xnoremap g\| g~

nnoremap g0 g0

vnoremap > >gv

vnoremap < <gv>
