imap <buffer> [[     \begin{
imap <buffer> ]]     <Plug>LatexCloseCurEnv
nmap <buffer> <F5>   <Plug>LatexChangeEnv
vmap <buffer> <F7>   <Plug>LatexWrapSelection
vmap <buffer> <S-F7> <Plug>LatexEnvWrapSelection
imap <buffer> ((     \eqref{

nnoremap <buffer> <localleader>wc :!texcount %<CR>

" Symbols
inoremap <buffer> ;eq \equiv
inoremap <buffer> ;ex \exists
inoremap <buffer> ;fa \forall
inoremap <buffer> ;ra \Rightarrow
inoremap <buffer> ;la \Leftarrow
inoremap <buffer> ;pi \pi

" Commands
inoremap <buffer> ;sec \section{}<CR><++><Esc>T{i
inoremap <buffer> ;ssec \subsection{}<CR><++><Esc>T{i
inoremap <buffer> ;sssec \subsubsection{}<CR><++><Esc>T{i
inoremap <buffer> ;pm \pmod{}<++><Esc>T{i
inoremap <buffer> ;em \emph{}<++><Esc>T{i
inoremap <buffer> ;bf \textbf{}<++><Esc>T{i
inoremap <buffer> ;fr \frac{}{<++>}<++><Esc>2T{i
inoremap <buffer> ;ref \autoref{}<++><Esc>T{i
inoremap <buffer> ;cit \autocite{}<++><Esc>T{i

" Environments
inoremap <buffer> ;ea <CR>\begin{align*}<CR><CR>\end{align*}<CR><++><Esc>kki<Tab>
inoremap <buffer> ;ega <CR>\begin{gather*}<CR><CR>\end{gather*}<CR><++><Esc>kki<Tab>
inoremap <buffer> ;e <CR>\begin{gather*}<CR><CR>\end{gather*}<CR><++><Esc>kki<Tab>
inoremap <buffer> ;m $$<++><Esc>F$i
inoremap <buffer> ;ol <CR>\begin{enumerate}<CR><CR>\end{enumerate}<CR><++><Esc>2kA\item<Space>
inoremap <buffer> ;ul <CR>\begin{itemize}<CR><CR>\end{itemize}<CR><++><Esc>2kA\item<Space>


autocmd bufwritepost *.tex :Latexmk


