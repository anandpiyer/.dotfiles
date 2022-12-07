# Generate pdf using pdflatex (-pdf)
$pdf_mode = 1;

# Use bibtex if a .bib file exists
$bibtex_use = 1;

$pdflatex = 'pdflatex -shell-escape -synctex=1 %O %S';
$pdf_previewer = 'open -a skim';
$clean_ext = "bbl rel out synctex.gz log %R-blx.bib %R.synctex.gz";
