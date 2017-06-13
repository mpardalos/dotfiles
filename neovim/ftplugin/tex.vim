" Latex-Box mappings {{{
imap <buffer> [[     \begin{
imap <buffer> ]]     <Plug>LatexCloseCurEnv
nmap <buffer> <F5>   <Plug>LatexChangeEnv
vmap <buffer> <F7>   <Plug>LatexWrapSelection
vmap <buffer> <S-F7> <Plug>LatexEnvWrapSelection
imap <buffer> ((     \eqref{
" }}}

" Text expansion (Inserting macros & envs with fewer keystrokes) {{{{
" Symbols {{{
inoremap <buffer> ;eq \equiv
inoremap <buffer> ;ex \exists
inoremap <buffer> ;fa \forall
inoremap <buffer> => \Rightarrow
inoremap <buffer> ;la \Leftarrow
inoremap <buffer> ;pi \pi
" }}}

" Commands {{{
inoremap <buffer> ;sec \section{}<CR><++><Esc>kf}i
inoremap <buffer> ;ssec \subsection{}<CR><++><Esc>kf}i
inoremap <buffer> ;sssec \subsubsection{}<CR><++><Esc>kf}i
inoremap <buffer> ;pm \pmod{}<++><Esc>T{i
inoremap <buffer> ;em \emph{}<++><Esc>T{i
inoremap <buffer> ;bf \textbf{}<++><Esc>T{i
inoremap <buffer> ;fr \frac{}{<++>}<++><Esc>2T{i
inoremap <buffer> ;ref \autoref{}<++><Esc>T{i
inoremap <buffer> ;cit \autocite{}<++><Esc>T{i
" }}}

" Environments {{{
inoremap <buffer> ;ea <CR>\begin{align*}<CR><CR>\end{align*}<CR><++><Esc>kki<Tab>
inoremap <buffer> ;ega <CR>\begin{gather*}<CR><CR>\end{gather*}<CR><++><Esc>kki<Tab>
inoremap <buffer> ;e <CR>\begin{gather*}<CR><CR>\end{gather*}<CR><++><Esc>kki<Tab>
inoremap <buffer> ;m $$<++><Esc>F$i
inoremap <buffer> ;ol <CR>\begin{enumerate}<CR><CR>\end{enumerate}<CR><++><Esc>2kA\item<Space>
inoremap <buffer> ;ul <CR>\begin{itemize}<CR><CR>\end{itemize}<CR><++><Esc>2kA\item<Space>
" }}}
" }}}

let g:LatexBox_latexmk_preview_continuously = 1
let g:tex_flavor='latex'
let g:LatexBox_Folding='1'
let g:LatexBox_custom_indent='0'
let g:LatexBox_quickfix=3

nnoremap <buffer> <localleader>wc :!texcount %<CR>

