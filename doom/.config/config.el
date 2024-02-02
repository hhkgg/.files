;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Howard Kang"
      user-mail-address "kanghoward00@gmail.com"
      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-font (font-spec :family "DejaVu Sans Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Roboto" :size 7)
      doom-serif-font (font-spec :family "Libre Baskerville")
      ;; doom-theme 'doom-city-lights
      doom-theme 'modus-operandi-deuteranopia
      display-line-numbers-type nil
      load-prefer-newer t
      +zen-text-scale 1
      ;; writeroom-extra-line-spacing 0.3
      ;;
      ;; lsp-ui-sideline-enable nil
      ;; lsp-enable-symbol-highlighting nil
      search-highlight t
      search-whitespace-regexp ".*?"
      org-directory "~/.org/"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-habit-show-habits-only-for-today t
      set-mark-command-repeat-pop t
      org-src-fontify-natively t)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-p") 'replace-string)

(map! :leader
      "X" #'org-gtd-capture)

(use-package! conda
  :config
  ;; (conda-env-initialize-interactive-shells)
  ;; (conda-env-initialize-eshell)
  ;; (conda-env-autoactivate-mode t)
  (setq conda-anaconda-home (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/"))
  (setq conda-env-subdirectory "envs")
  (setq conda-env-home-directory (expand-file-name "~/.conda/"))
  (unless (getenv "CONDA_DEFAULT_ENV")
    (conda-env-activate "base")))

;; buffer prompt after split
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; splash screen
(setq fancy-splash-image (concat doom-user-dir "doom-vapourwave.png"))

;; latex add texbin to emacs path variable
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2023/bin/universal-darwin"))
(setq exec-path (append exec-path '("/usr/local/texlive/2023/bin/universal-darwin")))

;; org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))


;; (after! org
;;   (setq org-agenda-files '("~/.org/gtd/org-gtd-tasks.org")))
;;
;;

;; (add-hook! '+doom-dashboard-mode-hook 'read-only-mode-hook)

(defun my/organize-hooks ()
  (when (org-gtd-organize-type-member-p '(project-heading))
    (org-set-tags-command)))


(use-package! org-gtd
  :after org
  :init
  ;; (setq stag-org-gtd-directory "~/.org/gtd")
  (setq org-gtd-update-ack "3.0.0")

  :custom
  (org-gtd-directory "~/.org/gtd")
  (org-agenda-property-position 'next-line)
  (org-edna-use-inheritance t)
  (org-gtd-organize-hooks '(my/organize-hooks))
  :config
  (org-edna-mode)
  (require 'org-gtd)
  (defun my/org-gtd-engage ()
    "Display `org-agenda' customized by org-gtd."
    (interactive)
    (org-gtd-core-prepare-agenda-buffers)
    (with-org-gtd-context
        (let* ((project-format-prefix
                (format " %%i %%-%d:(org-gtd-agenda--prefix-format) "
                        org-gtd-engage-prefix-width))
               (org-agenda-custom-commands
                `(("g" "scheduled today and all next items"
                   ((agenda ""
                            ((org-agenda-span 14)
                             (org-agenda-start-day nil)
                             (org-agenda-skip-additional-timestamps-same-entry t)))
                    (todo org-gtd-next
                          ((org-agenda-overriding-header "Next items")
                           (org-agenda-prefix-format
                            '((todo . ,project-format-prefix)))
                           (org-agenda-sorting-strategy '(time-up priority-down))
                           ))
                    (tags "PRIORITY=\"A\""
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                           (org-agenda-overriding-header "High-priority")
                           ))
                    (tags "mat159"
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                           (org-agenda-overriding-header "MAT159")
                           (org-agenda-sorting-strategy '(priority-down))))
                    (tags "mat240"
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                           (org-agenda-overriding-header "MAT240")
                           (org-agenda-sorting-strategy '(priority-down))))
                    (tags "csc148"
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                           (org-agenda-overriding-header "CSC148")
                           (org-agenda-sorting-strategy '(priority-down))))

                    (alltodo ""
                             ((org-agenda-sorting-strategy '(priority-down))))
                    ))))
               )
          (org-agenda nil "g")
          (goto-char (point-min)))))

  (map! :leader
        :prefix ("d" . "org-gtd")
        ;; "X" #'org-gtd-capture
        "c" #'org-gtd-clarify-item
        "o" #'org-gtd-organize
        "p" #'org-gtd-process-inbox
        "e" #'my/org-gtd-engage
        ;; "e" #'org-gtd-engage
        ;; "C-c d n" #'org-gtd-show-all-next
        ;; "C-c d s" #'org-gtd-show-stuck-projects
        )
  )


(after! org-roam
  :init
  (map! :leader
        :prefix ("n" . "notes")
        "r c" #'completion-at-point)
  :config
  (setq org-roam-directory (file-truename "~/.org/org-roam"))
  (setq org-roam-complete-everywhere t)
  (org-roam-db-autosync-enable)
  :custom
  (setq org-roam-capture-templates
        '(("d" "definition" plain
           "%?"
           :if-new (file+head "%<%y%m%d%h%m%s>-${slug}.org" "#+title: ${title}\n#+filetags: :mathematics:mat159:definition:")
           :unnarrowed t)
          ("t" "theorem" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :mathematics:mat159:theorem:")
           :unnarrowed t)
          ("p" "proof" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :mathematics:mat159:proof:")
           :unnarrowed t)
          ("p" "proposition" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :mathematics:mat159:proposition:")
           :unnarrowed t)
          )))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  ;; temporary fix org-roam-ui single tag bug
  (setq org-roam-database-connector 'sqlite)
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(setq jupyter-repl-echo-eval-p t)
(global-set-key (kbd "C-c C-x") 'jupyter-eval-region)

;; force refresh jupyter kernelspecs after switching conda env
(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

;; latex
(setq cdlatex-env-alist
      '(("axiom" "\\begin{axiom}\n?\n\\end{axiom}\n" nil)
	("theorem" "\\begin{thm}\n?\n\\end{thm}\n" nil)
	("proof" "\\begin{prooof}\n?\n\\end{prooof}\n" nil)
	("alphenum" "\\begin{enumerate}[\label=\\alph*)]\nAUTOLABEL\n?\n\\end{enumerate}\n" nil)
        ))

(setq cdlatex-command-alist
      '(("axm" "Insert axiom env"   "" cdlatex-environment ("axiom") t nil)
        ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
        ))

(add-hook 'latex-mode-hook (lambda ()
                             (push '(?$ . ("$" . "$")) evil-surround-pairs-alist)))

(use-package! laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
    "ftn" (lambda () (interactive)
	    (yas-expand-snippet "\\footnote{$1} $0"))

    "defn" (lambda () (interactive)
	     (yas-expand-snippet "\\begin{definition}{\\textbf{$1}} \\newline \n$0\n \\end{definition}"))

    "soln" (lambda () (interactive)
	     (yas-expand-snippet "\\begin{solution} \n$0\n \\end{solution}"))

    "exm" (lambda () (interactive)
	    (yas-expand-snippet "\\begin{example} \n$0\n \\end{example}"))

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
            (yas-expand-snippet "\\abs{$1}$0"))
    "CC" (lambda () (interactive)
	   (yas-expand-snippet "\\subseteq $0"))
    "SM" (lambda () (interactive)
	   (yas-expand-snippet "\\setminus $0"))
    "NN" (lambda () (interactive)
	   (yas-expand-snippet "\\cap $0"))
    "UU" (lambda () (interactive)
	   (yas-expand-snippet "\\cup $0"))

    "QQ" (lambda () (interactive)
	   (yas-expand-snippet "\\mathbb{Q} $0"))
    "RR" (lambda () (interactive)
	   (yas-expand-snippet "\\mathbb{R} $0"))
    "ZZ" (lambda () (interactive)
	   (yas-expand-snippet "\\mathbb{Z} $0"))
    ";a" (lambda () (interactive)
           (yas-expand-snippet "\\alpha $0"))
    ";b" (lambda () (interactive)
           (yas-expand-snippet "\\beta $0"))
    ";t" (lambda () (interactive)
           (yas-expand-snippet "\\theta $0"))

    ";p" (lambda () (interactive)
           (yas-expand-snippet "^{\\prime} $0"))

    "NM" (lambda () (interactive)
           (yas-expand-snippet "\\mathbb{N} $0"))

    "spann" (lambda () (interactive)
              (yas-expand-snippet "\\operatorname{span}($1) $0"))

    ;; "tt" (lambda () (interactive)
    ;;        (yas-expand-snippet "&\\text{$1} \\newline $0"))

    ;; "inn" (lambda () (interactive)
    ;;         (yas-expand-snippet "\\int $0"))

    "stmn" (lambda () (interactive)
	     (yas-expand-snippet "\\setminus $0"))


    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;; overriding doom package defaults
(after! cdlatex
  :config
  (setq cdlatex-use-dollar-to-ensure-math t)
  (map! :map cdlatex-mode-map
        ;; enable $ pairs
        "$" #'cdlatex-dollar
        "TAB" #'cdlatex-tab))

;; prevent switch focus on tex-command-run-all
(after! tex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (dolist (viewer (reverse +latex-viewers))
    (pcase viewer
      (`skim
       (when-let
           (app-path
            (and IS-MAC
                 (file-exists-p! (or "/Applications/Skim.app"
                                     "~/Applications/Skim.app"))))
         (add-to-list 'TeX-view-program-selection '(output-pdf "Skim"))
         (add-to-list 'TeX-view-program-list
                      (list "Skim" (format "%s/Contents/SharedSupport/displayline -r -b -g %%n %%o %%b"
                                           app-path))))))))

;; org/tex mode hooks
(add-hook! 'org-mode-hook 'laas-mode)
(add-hook! 'org-mode-hook 'org-gtd-mode)
(add-hook! 'org-mode-hook 'cdlatex-mode)
(add-hook! 'LaTeX-mode-hook 'cdlatex-mode)

(fset 'rainbow-delimiters-mode #'ignore)

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(use-package! easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)))

;; clean jupyter kernels and helper functions
(setq my/jupyter-runtime-folder (expand-file-name "/Users/howardkang/Library/Jupyter/runtime"))
(defun my/get-open-ports ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'") "\n")))

(defun my/list-jupyter-kernel-files ()
  (mapcar
   (lambda (file) (cons (car file) (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes my/jupyter-runtime-folder t ".*kernel.*json$")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

(defun my/jupyter-cleanup-kernels ()
  (interactive)
  (let* ((ports (my/get-open-ports))
         (files (my/list-jupyter-kernel-files))
         (to-delete (seq-filter
                     (lambda (file)
                       (not (member (cdr file) ports)))
                     files)))
    (when (and (length> to-delete 0)
               (y-or-n-p (format "Delete %d files?" (length to-delete))))
      (dolist (file to-delete)
        (delete-file (car file))))))

(use-package! jupyter
  :after (python conda)
  ;; emacs-jupyter will always use the Python kernel found on startup
  ;; ⇒ after switching to new environment, code still running in the old one (inconvenient)
  ;; To fix, force refresh of jupyter kernelspecs (source - https://sqrtminusone.xyz/posts/2021-05-01-org-python/)
  :config
  (defun my/jupyter-refresh-kernelspecs ()
    "Refresh Jupyter kernelspecs"
    (interactive)
    (jupyter-available-kernelspecs t)))

(map! :after python
      :leader
      :map python-mode-map
      :prefix "j"
      "a" #'jupyter-repl-associate-buffer)
