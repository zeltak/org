(TeX-add-style-hook
 "CV.kloog"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt" "draft=true")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("babel" "american") ("geometry" "margin=20mm" "paperwidth=210mm" "paperheight=297mm") ("biblatex" "backend=biber" "style=authoryear" "url=false" "doi=true" "eprint=false")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (TeX-run-style-hooks
    "latex2e"
    "bold_author_hack"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "fixltx2e"
    "graphicx"
    "longtable"
    "float"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "marvosym"
    "wasysym"
    "amssymb"
    "hyperref"
    "babel"
    "csquotes"
    "lastpage"
    "geometry"
    "biblatex"
    "helvet"
    "paralist"
    "fancyhdr"
    "titlesec")
   (TeX-add-symbols
    "itemize"
    "description"
    "enumerate")
   (LaTeX-add-labels
    "sec-1"
    "sec-1-1"
    "sec-1-2"
    "sec-2"
    "sec-3"
    "sec-4"
    "sec-4-1"
    "sec-4-2"
    "sec-4-3"
    "sec-4-4"
    "sec-5"
    "sec-5-1"
    "sec-5-2"
    "sec-6"
    "sec-7"
    "sec-7-1"
    "sec-7-2"
    "sec-7-3"
    "sec-7-4"
    "sec-8"
    "sec-8-1"
    "sec-8-2"
    "sec-8-3"
    "sec-9"
    "sec-9-1"
    "sec-10"
    "sec-10-1"
    "sec-10-2"
    "sec-11"
    "sec-11-1"
    "sec-12")
   (LaTeX-add-bibliographies
    "/home/zeltak/org/files/Uni/papers/kloog_2015")))

