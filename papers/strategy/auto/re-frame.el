(TeX-add-style-hook
 "re-frame"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("graphicx" "pdftex")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "setspace"
    "tikz"
    "multicol")
   (LaTeX-add-labels
    "sec:org060bea7"
    "sec:orge5b6785"
    "sec:org19a52f1"
    "sec:orgcf57d36"
    "sec:org4ecee07"
    "sec:orgf4b1276"
    "sec:orgbf17ac6"
    "sec:orgd1100ce"
    "sec:org04bf28f"
    "sec:orgc56c7da"
    "sec:org4b5bb81"
    "sec:org6b04b12"))
 :latex)

