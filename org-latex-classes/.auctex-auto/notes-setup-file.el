(TeX-add-style-hook
 "notes-setup-file"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("csquotes" "autostyle") ("xcolor" "svgnames") ("nowidow" "all") ("footmisc" "hang" "flushmargin" "stable" "multiple") ("geometry" "margin=1.25in") ("tcolorbox" "most")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "titling"
    "bookmark"
    "unicode-math"
    "titletoc"
    "csquotes"
    "fontspec"
    "xcolor"
    "enumitem"
    "enumerate"
    "fancyhdr"
    "lastpage"
    "float"
    "nowidow"
    "xparse"
    "parskip"
    "sectsty"
    "footmisc"
    "manyfoot"
    "relsize"
    "breakcites"
    "biblatex"
    "geometry"
    "setspace"
    "tcolorbox")
   (TeX-add-symbols
    '("subparagraph" 1)
    '("paragraph" 1)
    "oldsection"
    "oldparagraph"
    "oldsubparagraph")
   (LaTeX-add-environments
    '("quote" LaTeX-env-args ["argument"] 0))
   (LaTeX-add-xparse-macros
    '("\\RenewDocumentCommand{\\section}{s o m}" "section" "s o m" "Renew"))
   (LaTeX-add-xcolor-definecolors
    "block-gray"))
 :latex)

