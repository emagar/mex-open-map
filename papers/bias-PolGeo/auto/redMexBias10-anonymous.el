(TeX-add-style-hook
 "redMexBias10-anonymous"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "letter" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "letterpaper" "right=1.25in" "left=1.25in" "top=1in" "bottom=1in") ("fontenc" "T1") ("graphicx" "pdftex") ("hyperref" "pdftex" "hidelinks") ("natbib" "longnamesfirst" "sort") ("helvet" "scaled=.90") ("endfloat" "nomarkers" "nolists")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "geometry"
    "setspace"
    "fontenc"
    "amsmath"
    "amssymb"
    "url"
    "graphicx"
    "hyperref"
    "tikz"
    "natbib"
    "mathptmx"
    "helvet"
    "courier"
    "rotating"
    "dcolumn"
    "arydshln"
    "listings"
    "endfloat"
    "endnotes")
   (TeX-add-symbols
    "mc"
    "listofendnotes"
    "footnote"
    "enotesize")
   (LaTeX-add-labels
    "fn:cites"
    "S:bias"
    "E:kingBi"
    "E:cubeLaw"
    "F:lambdaRhoEx"
    "E:kingMulti"
    "T:3bias"
    "F:singleYrSeatsVotes"
    "F:malapp"
    "S:results"
    "F:posterior_s0s3"
    "T:GKBbreakdown"
    "T:swRatios")
   (LaTeX-add-bibliographies
    "../bib/redMex"))
 :latex)

