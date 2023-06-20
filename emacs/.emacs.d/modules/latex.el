(use-package auctex
  :defer t
  :ensure t)

(setq LaTeX-electric-left-right-brace t)
(setq electric-pair-preserve-balance nil)

;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(server-start); start emacs in server mode so that skim can talk to it

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode))

(setq cdlatex-env-alist
      '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
	("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
	("proof" "\\begin{proof}\nAUTOLABEL\n?\n\\end{proof}\n" nil)))

(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
		    ;; set condition!
		    :cond #'texmathp ; expand only while in math
		    "supp" "\\supp"
		    "On" "O(n)"
		    "O1" "O(1)"
		    "Olog" "O(\\log n)"
		    "Olon" "O(n \\log n)"
		    ;; bind to functions!
		    "Sum" (lambda () (interactive)
			    (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
		    "Span" (lambda () (interactive)
			     (yas-expand-snippet "\\Span($1)$0"))
                    "abs" (lambda () (interactive)
                             (yas-expand-snippet "\left|$1\\right|$0"))

		    ;; add accent snippets
		    :cond #'laas-object-on-left-condition
		    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))
