(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(visual-line-mode -1)
(fringe-mode '(0 . 0))
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(recentf-mode 1)
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/keybinds.el")
(load "~/.emacs.d/modules/latex.el")
(load "~/.emacs.d/modules/jupyter.el")
;; (add-to-list 'load-path "~/.emacs.d/modules/")

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-valley-dark t)
  (kaolin-treemacs-theme))

(set-frame-parameter nil 'ns-appearance 'dark)
(set-frame-parameter nil 'ns-transparent-titlebar t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck web-mode company yasnippet which-key vertico use-package modus-themes magit laas kaolin-themes ivy general evil-escape evil-easymotion evil-collection doom-modeline command-log-mode cdlatex balanced-windows auctex-latexmk all-the-icons ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
