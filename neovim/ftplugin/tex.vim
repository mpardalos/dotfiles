imap <buffer> [[     \begin{
imap <buffer> ]]     <Plug>LatexCloseCurEnv
nmap <buffer> <F5>   <Plug>LatexChangeEnv
vmap <buffer> <F7>   <Plug>LatexWrapSelection
vmap <buffer> <S-F7> <Plug>LatexEnvWrapSelection
imap <buffer> ((     \eqref{

nnoremap <buffer> <localleader>wc :!texcount %<CR>

" Symbols
inoremap ;eq \equiv
inoremap ;ex \exists
inoremap ;fa \forall
inoremap ;ra \Rightarrow
inoremap ;la \Leftarrow
inoremap ;pi \pi

" Commands
inoremap ;sec \section{}<CR><++><Esc>T{i
inoremap ;ssec \subsection{}<CR><++><Esc>T{i
inoremap ;sssec \subsubsection{}<CR><++><Esc>T{i
inoremap ;pm \pmod{}<++><Esc>T{i
inoremap ;em \emph{}<++><Esc>T{i
inoremap ;bf \textbf{}<++><Esc>T{i
inoremap ;fr \frac{}{<++>}<++><Esc>2T{i
inoremap ;ref \autoref{}<++><Esc>T{i
inoremap ;cit \autocite{}<++><Esc>T{i


" Environments
inoremap ;ea <CR>\begin{align*}<CR><CR>\end{align*}<CR><++><Esc>kki<Tab>
inoremap ;ega <CR>\begin{gather*}<CR><CR>\end{gather*}<CR><++><Esc>kki<Tab>
inoremap ;e <CR>\begin{gather*}<CR><CR>\end{gather*}<CR><++><Esc>kki<Tab>
inoremap ;m $$<++><Esc>F$i
inoremap ;ol <CR>\begin{enumerate}<CR><CR>\end{enumerate}<CR><++><Esc>2kA\item<Space>
inoremap ;ul <CR>\begin{itemize}<CR><CR>\end{itemize}<CR><++><Esc>2kA\item<Space>


autocmd bufwritepost *.tex :Latexmk


