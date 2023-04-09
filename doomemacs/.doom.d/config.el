;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq persp-auto-save-fname "autosave")
(setq persp-save-dir "/Users/hk/.emacs.d/.local/etc/workspaces/")
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Howard Kang"
      user-mail-address "kanghoward00@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;     doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-peacock)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;
;;
;;
;; Auctex configurations
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin"))
;; (setq exec-path (append exec-path '("/usr/local/bin" "/Library/TeX/texbin")))

;; Auctex configurations
;; (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
;; (setq exec-path (append exec-path '("/Library/TeX/texbin")))

;; package configs
(setq org-roam-directory (file-truename "~/org-roam"))

;; mappings
;; (map! :after python
;;       :map python-mode-map
;;       :prefix "C-x C-p"
;;       "f" #'python-pytest-file
;;       "r" #'python-pytest-repeat)

(map! :leader
      (:prefix ("m" . "org mode")
               :desc "org-mark-ring-goto"
               "e" #'org-mark-ring-goto))
(map! :leader
        :desc "toggle-line-number"
        "l" #'display-line-numbers-mode)

;; (map! :leader
;;       (:prefix ("m" . "org mode")
;;                :desc "org-toggle-latex-fragment"
;;                "l" #'org-toggle-latex-fragment))

;; yasnippet auto-expand
;; (defun my-yas-try-expanding ()
;;   (when yas-minor-mode (yas-expand)))
;; (add-hook 'post-command-hook #'my-yas-try-expanding)

;; buffer prompt after split
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; yasnippet latex-mode in org
(defun my-org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-reload-all)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'org-mode-hook #'my-org-latex-yas)
(add-hook 'org-mode-hook #'laas-mode)
;; electric mode
(add-hook 'org-mode-hook #'electric-pair-mode)

;; org-preview-latex transparent background
(with-eval-after-load 'org
  (plist-put org-format-latex-options :background "Transparent"))

;; calctex
(use-package! calctex
  :commands calctex-mode
  :init
  (add-hook 'calc-mode-hook #'calctex-mode)
  :config
  (setq calctex-additional-latex-packages "
\\usepackage[usenames]{xcolor}
\\usepackage{soul}
\\usepackage{adjustbox}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{siunitx}
\\usepackage{cancel}
\\usepackage{mathtools}
\\usepackage{mathalpha}
\\usepackage{xparse}
\\usepackage{arevmath}"
        calctex-additional-latex-macros
        (concat calctex-additional-latex-macros
                "\n\\let\\evalto\\Rightarrow"))
  (defadvice! no-messaging-a (orig-fn &rest args)
    :around #'calctex-default-dispatching-render-process
    (let ((inhibit-message t) message-log-max)
      (apply orig-fn args)))
  ;; Fix hardcoded dvichop path (whyyyyyyy)
  (let ((vendor-folder (concat (file-truename doom-local-dir)
                               "straight/"
                               (format "build-%s" emacs-version)
                               "/calctex/vendor/")))
    (setq calctex-dvichop-sty (concat vendor-folder "texd/dvichop")
          calctex-dvichop-bin (concat vendor-folder "texd/dvichop")))
  (unless (file-exists-p calctex-dvichop-bin)
    (message "CalcTeX: Building dvichop binary")
    (let ((default-directory (file-name-directory calctex-dvichop-bin)))
      (call-process "make" nil nil nil))))


(map! :map calc-mode-map
      :after calc
      :localleader
      :desc "Embedded calc (toggle)" "e" #'calc-embedded)
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Embedded calc (toggle)" "E" #'calc-embedded)
(map! :map latex-mode-map
      :after latex
      :localleader
      :desc "Embedded calc (toggle)" "e" #'calc-embedded)


(defvar calc-embedded-trail-window nil)
(defvar calc-embedded-calculator-window nil)

(defadvice! calc-embedded-with-side-pannel (&rest _)
  :after #'calc-do-embedded
  (when calc-embedded-trail-window
    (ignore-errors
      (delete-window calc-embedded-trail-window))
    (setq calc-embedded-trail-window nil))
  (when calc-embedded-calculator-window
    (ignore-errors
      (delete-window calc-embedded-calculator-window))
    (setq calc-embedded-calculator-window nil))
  (when (and calc-embedded-info
             (> (* (window-width) (window-height)) 1200))
    (let ((main-window (selected-window))
          (vertical-p (> (window-width) 80)))
      (select-window
       (setq calc-embedded-trail-window
             (if vertical-p
                 (split-window-horizontally (- (max 30 (/ (window-width) 3))))
               (split-window-vertically (- (max 8 (/ (window-height) 4)))))))
      (switch-to-buffer "*Calc Trail*")
      (select-window
       (setq calc-embedded-calculator-window
             (if vertical-p
                 (split-window-vertically -6)
               (split-window-horizontally (- (/ (window-width) 2))))))
      (switch-to-buffer "*Calculator*")
      (select-window main-window))))


(add-hook 'after-init-hook 'company-statistics-mode)


;; yasnippet auto expand
;; (defun my-yas-try-expanding-auto-snippets ()
;;   (when yas-minor-mode
;;     (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
;;       (yas-expand))))
;; (add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))


(use-package! laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    "mk" (lambda () (interactive)
                             (yas-expand-snippet "$$1$ $0"))
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\suppn"
                    "lrp" (lambda () (interactive)
                             (yas-expand-snippet "\\left( $1 \\right\)$0"))
                    "lrb" (lambda () (interactive)
                             (yas-expand-snippet "\\left[ $1 \\right]$0"))
                    "td" (lambda () (interactive)
                             (yas-expand-snippet "^{ $1}"))
                    "abs" (lambda () (interactive)
                             (yas-expand-snippet "\left|$1\\right|$0"))
                    "sq" (lambda () (interactive)
                             (yas-expand-snippet "\\sqrt{ $1}$0"))
                    "matrix" (lambda () (interactive)
                             (yas-expand-snippet "
                                                \begin{bmatrix}
                                                $1
                                                \end{bmatrix}
                                                "))
                    ";L" (lambda () (interactive)
                             (yas-expand-snippet "\\wedge "))
                    "alin" (lambda () (interactive)
                             (yas-expand-snippet "\\begin{align}$1\\end{align} $0"))
                    "dm" (lambda () (interactive)
                             (yas-expand-snippet "\\[ $1]\ $0"))
                    "bmat" (lambda () (interactive)
                             (yas-expand-snippet "\begin{bmatrix} $1\end{bmatrix} $0"))
                    ;; "rm" (lambda () (interactive)
                    ;;          (yas-expand-snippet "$ $1 $ $ $0"))

                    ))

;; plain latex class
(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-src-block-backend 't)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))

;; org mode
(setq org-agenda-files
      '("~/Library/CloudStorage/OneDrive-Personal/org/test_gtd/inbox.org"
        "~/Documents/test_gtd/projects.org"
        "~/Documents/test_gtd/repeaters.org" ))

(setq org-return-follows-link t
      org-agenda-tags-column 75
      org-deadline-warning-days 30
      org-use-speed-commands t)

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/documents/test_gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/documents/test_gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups
        '((:name "Day's Tasks"
          :time-grid t
          :transformer (--> it
                       (upcase it)
                       (propertize it 'face '(:foreground "RosyBrown1"))))
          (:name "Priority"
          :face (:background "black" :underline t)
          :not (:priority "C")
          :order 1)
          (:name "Tertiary"
                 :priority "C"
                 :order 100)
         (:auto-group t))
        )

  :config
  (org-super-agenda-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (julia . t)
   (python . t)
   (jupyter . t)))

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :init
  (map! "C-x m" #'mathpix-screenshot)
  :config
  (setq mathpix-screenshot-method "xfce4-screenshooter -r -o cat > %s"
        mathpix-app-id (with-temp-buffer (insert-file-contents "./secrets/mathpix-app-id") (buffer-string))
        mathpix-app-key (with-temp-buffer (insert-file-contents "./secrets/mathpix-app-key") (buffer-string))))


(use-package! conda
  :config
  (setq conda-anaconda-home (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/"))
  (setq conda-env-home-directory (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/"))
  (setq conda-env-subdirectory "envs"))

(unless (getenv "CONDA_DEFAULT_ENV")
  (conda-env-activate "algos"))

(use-package! jupyter
  :demand t
  :after (:all org python)
  :config (require 'ob-jupyter))

(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

;; fancy splash screen
(defvar fancy-splash-image-directory
  (expand-file-name "misc/splash-images/" doom-user-dir)
  "Directory in which to look for splash image templates.")

(defvar fancy-splash-image-template
  ;; (expand-file-name "emacs-e-template.svg" fancy-splash-image-directory)
  (expand-file-name "aperture-laboratories-template.svg" fancy-splash-image-directory)
  "Default template svg used for the splash image.
Colours are substituted as per `fancy-splash-template-colours'.")

(defvar fancy-splash-template-colours
  '(("#111112" :face default   :attr :foreground)
    ("#8b8c8d" :face shadow)
    ("#eeeeef" :face default   :attr :background)
    ("#e66100" :face highlight :attr :background)
    ("#1c71d8" :face font-lock-keyword-face)
    ("#f5c211" :face font-lock-type-face)
    ("#813d9c" :face font-lock-constant-face)
    ("#865e3c" :face font-lock-function-name-face)
    ("#2ec27e" :face font-lock-string-face)
    ("#c01c28" :face error)
    ("#000001" :face ansi-color-black)
    ("#ff0000" :face ansi-color-red)
    ("#ff00ff" :face ansi-color-magenta)
    ("#00ff00" :face ansi-color-green)
    ("#ffff00" :face ansi-color-yellow)
    ("#0000ff" :face ansi-color-blue)
    ("#00ffff" :face ansi-color-cyan)
    ("#fffffe" :face ansi-color-white))
  "Alist of colour-replacement plists.
Each plist is of the form (\"$placeholder\" :doom-color 'key :face 'face).
If the current theme is a doom theme :doom-color will be used,
otherwise the colour will be face foreground.")

(defun fancy-splash-check-buffer ()
  "Check the current SVG buffer for bad colours."
  (interactive)
  (when (eq major-mode 'image-mode)
    (xml-mode))
  (when (and (featurep 'rainbow-mode)
             (not (bound-and-true-p rainbow-mode)))
    (rainbow-mode 1))
  (let* ((colours (mapcar #'car fancy-splash-template-colours))
         (colourise-hex
          (lambda (hex)
            (propertize
             hex
             'face `((:foreground
                      ,(if (< 0.5
                              (cl-destructuring-bind (r g b) (x-color-values hex)
                                ;; Values taken from `rainbow-color-luminance'
                                (/ (+ (* .2126 r) (* .7152 g) (* .0722 b))
                                   (* 256 255 1.0))))
                           "white" "black")
                      (:background ,hex))))))
         (cn 96)
         (colour-menu-entries
          (mapcar
           (lambda (colour)
             (cl-incf cn)
             (cons cn
                   (cons
                    (substring-no-properties colour)
                    (format " (%s) %s %s"
                            (propertize (char-to-string cn)
                                        'face 'font-lock-keyword-face)
                            (funcall colourise-hex colour)
                            (propertize
                             (symbol-name
                              (plist-get
                               (cdr (assoc colour fancy-splash-template-colours))
                               :face))
                             'face 'shadow)))))
           colours))
         (colour-menu-template
          (format
           "Colour %%s is unexpected! Should this be one of the following?\n
%s
 %s to ignore
 %s to quit"
           (mapconcat
            #'cddr
            colour-menu-entries
            "\n")
           (propertize "SPC" 'face 'font-lock-keyword-face)
           (propertize "ESC" 'face 'font-lock-keyword-face)))
         (colour-menu-choice-keys
          (append (mapcar #'car colour-menu-entries)
                  (list ?\s)))
         (buf (get-buffer-create "*fancy-splash-lint-colours-popup*"))
         (good-colour-p
          (lambda (colour)
            (or (assoc colour fancy-splash-template-colours)
                ;; Check if greyscale
                (or (and (= (length colour) 4)
                         (= (aref colour 1)   ; r
                            (aref colour 2)   ; g
                            (aref colour 3))) ; b
                    (and (= (length colour) 7)
                         (string= (substring colour 1 3)       ; rr =
                                  (substring colour 3 5))      ; gg
                         (string= (substring colour 3 5)       ; gg =
                                  (substring colour 5 7))))))) ; bb
         (prompt-to-replace
          (lambda (target)
            (with-current-buffer buf
              (erase-buffer)
              (insert (format colour-menu-template
                              (funcall colourise-hex target)))
              (setq-local cursor-type nil)
              (set-buffer-modified-p nil)
              (goto-char (point-min)))
            (save-window-excursion
              (pop-to-buffer buf)
              (fit-window-to-buffer (get-buffer-window buf))
              (car (alist-get
                    (read-char-choice
                     (format "Select replacement, %s-%s or SPC: "
                             (char-to-string (caar colour-menu-entries))
                             (char-to-string (caar (last colour-menu-entries))))
                     colour-menu-choice-keys)
                    colour-menu-entries))))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#[0-9A-Fa-f]\\{6\\}\\|#[0-9A-Fa-f]\\{3\\}" nil t)
        (recenter)
        (let* ((colour (match-string 0))
               (replacement (and (not (funcall good-colour-p colour))
                                 (funcall prompt-to-replace colour))))
          (when replacement
            (replace-match replacement t t))))
      (message "Done"))))


(defvar fancy-splash-cache-dir (expand-file-name "theme-splashes/" doom-cache-dir))


(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "List of plists specifying image sizing states.
Each plist should have the following properties:
- :height, the height of the image
- :min-height, the minimum `frame-height' for image
- :padding, a `+doom-dashboard-banner-padding' (top . bottom) padding
  specification to apply
Optionally, each plist may set the following two properties:
- :template, a non-default template file
- :file, a file to use instead of template")



(defun fancy-splash-filename (theme template height)
  "Get the file name for the splash image with THEME and of HEIGHT."
  (expand-file-name (format "%s-%s-%d.svg" theme (file-name-base template) height) fancy-splash-cache-dir))




(defun fancy-splash-generate-image (template height)
  "Create a themed image from TEMPLATE of HEIGHT.
The theming is performed using `fancy-splash-template-colours'
and the current theme."
  (with-temp-buffer
    (insert-file-contents template)
    (goto-char (point-min))
    (if (re-search-forward "$height" nil t)
        (replace-match (number-to-string height) t t)
      (if (re-search-forward "height=\"100\\(?:\\.0[0-9]*\\)?\"" nil t)
          (progn
            (replace-match (format "height=\"%s\"" height) t t)
            (goto-char (point-min))
            (when (re-search-forward "\\([ \t\n]\\)width=\"[\\.0-9]+\"[ \t\n]*" nil t)
              (replace-match "\\1")))
        (warn "Warning! fancy splash template: neither $height nor height=100 not found in %s" template)))
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (let* ((replacement-colour
              (face-attribute (plist-get (cdr substitution) :face)
                              (or (plist-get (cdr substitution) :attr) :foreground)
                              nil 'default))
             (replacement-hex
              (if (string-prefix-p "#" replacement-colour)
                  replacement-colour
                (apply 'format "#%02x%02x%02x"
                       (mapcar (lambda (c) (ash c -8))
                               (color-values replacement-colour))))))
        (while (search-forward (car substitution) nil t)
          (replace-match replacement-hex nil nil))))
    (unless (file-exists-p fancy-splash-cache-dir)
      (make-directory fancy-splash-cache-dir t))
    (let ((inhibit-message t))
      (write-region nil nil (fancy-splash-filename (car custom-enabled-themes) template height)))))

(defun fancy-splash-generate-all-images ()
  "Perform `fancy-splash-generate-image' in bulk."
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image
       (or (plist-get size :template)
           fancy-splash-image-template)
       (plist-get size :height)))))

(defun fancy-splash-ensure-theme-images-exist (&optional height)
  "Ensure that the relevant images exist.
Use the image of HEIGHT to check, defaulting to the height of the first
specification in `fancy-splash-sizes'. If that file does not exist for
the current theme, `fancy-splash-generate-all-images' is called. "
  (unless (file-exists-p
           (fancy-splash-filename
            (car custom-enabled-themes)
            fancy-splash-image-template
            (or height (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-all-images)))

(defun fancy-splash-clear-cache (&optional delete-files)
  "Clear all cached fancy splash images.
Optionally delete all cache files and regenerate the currently relevant set."
  (interactive (list t))
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (let ((image-file
             (fancy-splash-filename
              (car custom-enabled-themes)
              (or (plist-get size :template)
                  fancy-splash-image-template)
              (plist-get size :height))))
        (image-flush (create-image image-file) t))))
  (message "Fancy splash image cache cleared!")
  (when delete-files
    (delete-directory fancy-splash-cache-dir t)
    (fancy-splash-generate-all-images)
    (message "Fancy splash images cache deleted!")))

(defun fancy-splash-switch-template ()
  "Switch the template used for the fancy splash image."
  (interactive)
  (let ((new (completing-read
              "Splash template: "
              (mapcar
               (lambda (template)
                 (replace-regexp-in-string "-template\\.svg$" "" template))
               (directory-files fancy-splash-image-directory nil "-template\\.svg\\'"))
              nil t)))
    (setq fancy-splash-image-template
          (expand-file-name (concat new "-template.svg") fancy-splash-image-directory))
    (fancy-splash-clear-cache)
    (message "") ; Clear message from `fancy-splash-clear-cache'.
    (setq fancy-splash--last-size nil)
    (fancy-splash-apply-appropriate-image)))


(defun fancy-splash-get-appropriate-size ()
  "Find the firt `fancy-splash-sizes' with min-height of at least frame height."
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash--last-size nil)
(setq fancy-splash--last-theme nil)

(defun fancy-splash-apply-appropriate-image (&rest _)
  "Ensure the appropriate splash image is applied to the dashboard.
This function's signature is \"&rest _\" to allow it to be used
in hooks that call functions with arguments."
  (let ((appropriate-size (fancy-splash-get-appropriate-size)))
    (unless (and (equal appropriate-size fancy-splash--last-size)
                 (equal (car custom-enabled-themes) fancy-splash--last-theme))
      (unless (plist-get appropriate-size :file)
        (fancy-splash-ensure-theme-images-exist (plist-get appropriate-size :height)))
      (setq fancy-splash-image
            (or (plist-get appropriate-size :file)
                (fancy-splash-filename (car custom-enabled-themes)
                                       fancy-splash-image-template
                                       (plist-get appropriate-size :height)))
            +doom-dashboard-banner-padding (plist-get appropriate-size :padding)
            fancy-splash--last-size appropriate-size
            fancy-splash--last-theme (car custom-enabled-themes))
      (+doom-dashboard-reload))))


(fancy-splash-apply-appropriate-image)
