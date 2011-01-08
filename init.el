;; Vim style keyboard moving
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-unset-key (kbd "C-x m")) ; I don't use mail
(global-unset-key (kbd "C-z")) ; suspending frame is useless with emacsclient and/or tmux


;; general programming things
(show-paren-mode 1)


;; javascript
(setq
 js-indent-level 2)

;; Autocomplete
(add-to-list 'load-path "~/.emacs2/vendor/autocomplete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs2/vendor/autocomplete/ac-dict")
(ac-config-default)

;; ido-mode, fuzzy matching on find-file, buffer switch
(ido-mode t)

;; magit
(add-to-list 'load-path "~/.emacs2/submodules/magit/")
(require 'magit)

;; textile mode
(add-to-list 'load-path "~/.emacs2/vendor/")
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile" . textile-mode))

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq auto-mode-alist
      (append
       '(("\\.textile" . textile-mode)
	 ("\\.bashrc" . sh-mode))
       auto-mode-alist))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

