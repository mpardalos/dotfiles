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
inoremap ;sec \section{}(<>)<Esc>T{i
inoremap ;ssec \subsection{}(<>)<Esc>T{i
inoremap ;sssec \subsubsection{}(<>)<Esc>T{i
inoremap ;pm \pmod{}(<>)<Esc>T{i
inoremap ;em \emph{}(<>)<Esc>T{i
inoremap ;bf \textbf{}(<>)<Esc>T{i
inoremap ;ref \autoref{}(<>)<Esc>T{i
inoremap ;cit \autocite{}(<>)<Esc>T{i


" Environments
inoremap ;ea <Enter>\begin{align*}<Enter><Enter>\end{align*}<Enter>(<>)<Esc>kki<Tab>
inoremap ;ega <Enter>\begin{gather*}<Enter><Enter>\end{gather*}<Enter>(<>)<Esc>kki<Tab>
inoremap ;e <Enter>\begin{gather*}<Enter><Enter>\end{gather*}<Enter>(<>)<Esc>kki<Tab>
inoremap ;m $$(<>)<Esc>F$i
inoremap ;ol <Enter>\begin{enumerate}<Enter><Enter>\end{enumerate}<Enter>(<>)<Esc>2kA\item<Space>
inoremap ;ul <Enter>\begin{itemize}<Enter><Enter>\end{itemize}<Enter>(<>)<Esc>2kA\item<Space>


autocmd bufwritepost *.tex :Latexmk


