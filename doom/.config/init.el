;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       (vertico +orderless +icons)

       :ui
       doom
       doom-dashboard
       hl-todo
       hydra
       indent-guides
       modeline
       nav-flash
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       vi-tilde-fringe
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       snippets
       word-wrap

       :emacs
       dired
       electric
       ibuffer
       undo
       vc

       :term
       vterm

       :checkers
       (syntax +childframe)
       (spell +flyspell)
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       biblio
       (debugger +lsp)
       direnv
       (eval +overlay)
       gist
       (lookup +docsets +dictionary)
       lsp
       (magit +forge)
       tree-sitter       ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)

       :lang
       data
       emacs-lisp
       (go +lsp +treesitter)
       (json +lsp)
       (javascript +lsp)
       (latex +latexmk +cdlatex +lsp)
       markdown
       (org +jupyter)
       (python +lsp +pyright)
       rest
       sh
       web
       (yaml +lsp)

       :config
       ;;literate
       (default +bindings +smartparens))
