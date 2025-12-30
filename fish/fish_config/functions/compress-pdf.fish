function compress-pdf
    if test (count $argv) -lt 1
        echo "Usage: compress-pdf <input.pdf> [output.pdf]"
        return 1
    end

    set input $argv[1]
    if test (count $argv) -ge 2
        set output $argv[2]
    else
        set output (basename $input .pdf)_compressed.pdf
    end

    gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -sOutputFile=$output $input
    echo "Compressed $input to $output"
    ls -lh $input $output
end

