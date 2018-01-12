" std::cout, std::cin, etc.
inoremap ;in std::cin >>
inoremap ;out std::cout <<
inoremap ;end << std::endl;

" Basic new file
inoremap ;main #include <iostream><CR><CR>int main() {<CR>return 0;<CR>}<Esc>kO

" if
inoremap ;if if () {<CR><++><CR>}<++><Esc>kk0f)i
inoremap ;elif <space>else if () {<CR><++><CR>}<++><Esc>kk0f)i
inoremap ;else <space>else {<CR><CR>}<++><Esc>ki<Tab>

" loops
inoremap ;while while () {<CR><++><CR>}<Esc>kk0f)i
inoremap ;for for(;<++>;<++>) {<CR><++><CR>}<Esc>kk0f(a
