(TeX-add-style-hook "redMexPartyStrategy"
 (lambda ()
    (LaTeX-add-bibliographies
     "../bib/magar")
    (LaTeX-add-labels
     "T:counterprops"
     "F:propsAndCost"
     "T:simIndex"
     "T:volatMarginsd0"
     "F:malmgnat")
    (TeX-add-symbols
     "mc")
    (TeX-run-style-hooks
     "amsmath"
     "arydshln"
     "natbib"
     "sort"
     "longnamesfirst"
     "graphicx"
     "pdftex"
     "url"
     "hyperref"
     "setspace"
     ""
     "geometry"
     "bottom=1in"
     "top=1in"
     "left=1.25in"
     "right=1.25in"
     "letterpaper"
     "latex2e"
     "art12"
     "article"
     "12pt"
     "letter")))

