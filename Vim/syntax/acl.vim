" Author: Samuele Giraudo
" Creation: feb. 2022
" Modifications: feb. 2022, mar. 2022, may. 2022, aug. 2022, sep. 2022, oct. 2022,
" nov. 2022, dec. 2022, apr. 2023, jul. 2023

" Syntax file of the Aclove language.
" This file has to be at ~/.vim/syntax/acl.vim

if exists("b:current_syntax")
    finish
endif

" Turns off spell checking.
set nospell

" Comments.
syn region Comment start="{" end="}"

" Structure keyword and symbols.
syn keyword Delimiter let
syn keyword Delimiter in
syn keyword Delimiter put
syn match Delimiter  "="
syn match Delimiter "!"

" Symbol operators and related things.
syn match Operator "\^"
syn match Operator "+"
syn match Operator "-"

" Other symbols.
syn match Label "("
syn match Label ")"
syn match Label "\["
syn match Label "\]"
syn match Label "<"
syn match Label ">"
syn match Label ":"
syn match Label ";"
syn match Label "->"
syn match Label "@"

" Variable names.
syn match Character "%\%([a-zA-Z0-9\-\_\./]\+\)"

" Constant names.
syn match Function "\'\%([a-zA-Z0-9\-\_\./]\+\)"

" Misc.
syn match Delimiter "/"
syn match Label "\.\."

