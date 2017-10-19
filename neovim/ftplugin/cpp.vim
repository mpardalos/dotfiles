" std::cout, std::cin, etc.
inoremap ;in std::cin<space>>><space>
inoremap ;out std::cout<space><<<space>
inoremap ;end <<<space>std::endl;

" Basic new file
inoremap ;main #include <iostream><CR><CR>int main() {<CR>return 0;<CR>}<Esc>kO

