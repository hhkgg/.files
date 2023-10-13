;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Howard Kang"
      user-mail-address "kanghoward00@gmail.com"
      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-font (font-spec :family "DejaVu Sans Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Roboto" :size 7)
      doom-serif-font (font-spec :family "Libre Baskerville")
      doom-theme 'doom-city-lights
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
      org-habit-show-habits-only-for-today t)

;; (setq python-shell-virtualenv-root "/opt/homebrew/Caskroom/miniconda/base/bin/python")

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

;; (shell-command "conda init bash")
;; (shell-command "conda activate")

;; (setq python-python-command "/opt/homebrew/Caskroom/miniconda/base/bin/python")
;; (setq python-shell-interpreter "/opt/homebrew/Caskroom/miniconda/base/bin/python")

;; buffer prompt after split
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; splash screen
(setq fancy-splash-image (concat doom-user-dir "doom-vapourwave.png"))

;; global keybinds
;; (map! :prefix "SPC"
;;       "S-o" #'ace-window)


;; latex add texbin to emacs path variable
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2023/bin/universal-darwin"))
(setq exec-path (append exec-path '("/usr/local/texlive/2023/bin/universal-darwin")))

(setq set-mark-command-repeat-pop t)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))

(setq jupyter-repl-echo-eval-p t)
(global-set-key (kbd "C-c C-x") 'jupyter-eval-region)

;; force refresh jupyter kernelspecs after switching conda env
(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

;; latex
(setq cdlatex-env-alist
      '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
	("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)
	("proof" "\\begin{proof}\nAUTOLABEL\n?\n\\end{proof}\n" nil)
	("alphenum" "\\begin{enumerate}[\label=\\alph*)]\nAUTOLABEL\n?\n\\end{enumerate}\n" nil)
        ))

(setq org-src-fontify-natively t)

(use-package! laas
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

    "CC" (lambda () (interactive)
	   (yas-expand-snippet "\\subseteq $0"))
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

    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;; overriding doom package defaults
(after! cdlatex
  :config
  (setq cdlatex-use-dollar-to-ensure-math t)
  (map! :map cdlatex-mode-map
        "$" #'cdlatex-dollar
        "TAB" #'cdlatex-tab))

;; prevent switch focus on tex-command-run-all
(after! tex
  :config
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
(add-hook! 'org-mode-hook 'cdlatex-mode)
(add-hook! 'LaTeX-mode-hook 'cdlatex-mode)

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :init
  (map! "C-x m" #'mathpix-screenshot)
  :config
  (setq mathpix-screenshot-method "screencapture -i %s"
        mathpix-app-id (with-temp-buffer (insert-file-contents "./secrets/mathpix-app-id") (buffer-string))
        mathpix-app-key (with-temp-buffer (insert-file-contents "./secrets/mathpix-app-key") (buffer-string))))


;; (after! org
;;   (setq org-agenda-files '("~/.org/gtd/inbox.org"))
;;   (setq org-agenda-span 20)
;;   ;; (setq org-agenda-start-day "+20d"))

;;   (setq howardkang/org-agenda-directory "~/.org/gtd/")
;;   (setq org-capture-templates
;;         `(("i" "inbox" entry (file ,(concat howardkang/org-agenda-directory "inbox.org"))
;;            "* TODO %?")
;;           ("e" "email" entry (file+headline ,(concat howardkang/org-agenda-directory "emails.org") "Emails")
;;            "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
;;           ("l" "link" entry (file ,(concat howardkang/org-agenda-directory "inbox.org"))
;;            "* TODO %(org-cliplink-capture)" :immediate-finish t)
;;           ("c" "org-protocol-capture" entry (file ,(concat howardkang/org-agenda-directory "inbox.org"))
;;            "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t))))


(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;; (use-package! tree-sitter
;;   :hook
;;   (prog-mode . global-tree-sitter-mode))

;; (map! :after org
;;       :map org-mode-map
;;       :mode i
;;       :prefix "C"
;;       "l" #'flyspell-auto-correct-word)

(use-package! ctrlf
  :hook
  (after-init . ctrlf-mode)
  :config
  (map! :map cdlatex-mode-map
        "C-l" #'ctrlf-backward-default))

(use-package! easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)))

(setq jupyter-long-timeout 50)


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
  ;; emacs-jupyter will always juse the Python kernel found on startup
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

(org-super-agenda-mode 1)

(use-package! org-super-agenda
  :config
  (setq org-agenda-files '("~/.org/gtd/inbox.org"))
  (setq org-super-agenda-groups t)
  (setq org-super-agenda-mode t)
  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Today"  ; Optionally specify section name
           :time-grid t  ; Items that appear on the time grid
           :todo "TODAY")  ; Items that have this TODO keyword
          (:name "Important"
           ;; Single arguments given alone
           :tag "bills"
           :priority "A")
          ;; Set order of multiple groups at once
          (:order-multi (2 (:name "Shopping in town"
                            ;; Boolean AND group matches items that match all subgroups
                            :and (:tag "shopping" :tag "@town"))
                           (:name "Food-related"
                            ;; Multiple args given in list with implicit OR
                            :tag ("food" "dinner"))
                           (:name "Personal"
                            :habit t
                            :tag "personal")
                           (:name "Space-related (non-moon-or-planet-related)"
                            ;; Regexps match case-insensitively on the entire entry
                            :and (:regexp ("space" "NASA")
                                  ;; Boolean NOT also has implicit OR between selectors
                                  :not (:regexp "moon" :tag "planet")))))
          ;; Groups supply their own section names when none are given
          (:todo "WAITING" :order 8)  ; Set order of this section
          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
           ;; Show this group at the end of the agenda (since it has the
           ;; highest number). If you specified this group last, items
           ;; with these todo keywords that e.g. have priority A would be
           ;; displayed in that group instead, because items are grouped
           ;; out in the order the groups are listed.
           :order 9)
          (:priority<= "B"
           ;; Show this section after "Today" and "Important", because
           ;; their order is unspecified, defaulting to 0. Sections
           ;; are displayed lowest-number-first.
           :order 1)
          ;; After the last group, the agenda will display items that didn't
          ;; match any of these groups, with the default order position of 99
          )))

