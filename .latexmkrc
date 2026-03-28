$pdf = 1;
$pdf_previewer = 'open -a Skim.app %O %S';

# Allow packages like minted to invoke external helpers such as pygmentize.
$pdflatex = 'pdflatex -shell-escape %O %S';
$xelatex = 'xelatex -shell-escape %O %S';
$lualatex = 'lualatex -shell-escape %O %S';

# TeX processes do not always inherit the interactive shell PATH on macOS.
$ENV{PATH} = join(':',
  '/Users/pavpan/bin',
  '/opt/homebrew/bin',
  '/Users/pavpan/.cargo/bin',
  ($ENV{PATH} // ''),
);
