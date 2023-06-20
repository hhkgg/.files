;; general.el keys
(require 'general)

;; * Global Keybindings
;; `general-define-key' acts like `global-set-key' when :keymaps is not
;; specified (because ":keymaps 'global" is the default)
;; kbd is not necessary and arbitrary amount of key def pairs are allowed
(general-define-key
 "C-s" 'counsel-grep-or-swiper)


;; Keybinds
(global-set-key (kbd "C-e") nil)
;; this isnt the issue yabai interaction for fullscreen
(global-set-key (kbd "M-f") nil)

(global-set-key (kbd "s-<backspace>") 'kill-whole-line)

;; * Mode Keybindings
;; `general-define-key' is comparable to `define-key' when :keymaps is specified
(general-define-key
 ;; NOTE: keymaps specified with :keymaps must be quoted
 :keymaps 'org-mode-map
 "C-c C-q" 'counsel-org-tag
 ;; ...
 )
;; `general-def' can be used instead for `define-key'-like syntax
(general-def org-mode-map
  "C-c C-q" 'counsel-org-tag
  ;; ...
  )

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun find-file-other-window-vertically nil
  "Edit a file in another window, split vertically."
  (interactive)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    (call-interactively 'find-file-other-window)))
(defun find-file-other-window-horizontally nil
  "Edit a file in another window, split vertically."
  (interactive)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    (call-interactively 'find-file-other-window)))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

;; use `SPC' as leader key
(general-create-definer my-spc-leader-def
  :prefix "SPC"
  :keymaps 'override
  :states '(normal visual))

(my-spc-leader-def
  "bl" 'ibuffer
  "b[" 'previous-buffer
  "b]" 'next-buffer
  ;; file modifications
  "fs" 'save-buffer
  "wc" 'quit-window
  "rr" 'reload-init-file
  "wv" 'find-file-other-window-vertically
  "wh" 'find-file-other-window-horizontally
  "o" 'ace-window
  "." 'find-file
  "ss" 'avy-goto-char-2
  "cc" 'comment-line
  "l" 'display-line-numbers-mode
  "rf" 'recentf-open-files
  "u" 'revert-buffer-no-confirm
  "fp" 'config-directory)

(defun config-directory ()
  (interactive)
  (cd "~/.emacs.d/")
  (call-interactively 'find-file))

(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))
;; (setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-frame)))

;; non-prefix keybinds

(general-evil-setup)
(general-imap "j"
              (general-key-dispatch 'self-insert-command
                "k" 'evil-normal-state))


