;;; desires
;; it would be nice to build my tags via a key command. append-to: /path/to/file  from directory: /path/to/
;; do I want paredit?

;; Vim style keyboard moving
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-unset-key (kbd "C-x m")) ; I don't use mail
(global-unset-key (kbd "C-z")) ; suspending frame is useless with emacsclient and/or tmux

(defalias 'qrr 'query-regexp-replace)
(fset 'yes-or-no-p 'y-or-n-p)  ;; only type `y` instead of `yes`
(setq inhibit-splash-screen t) ;; no splash screen
(setq indent-tabs-mode nil)      ;; no tabs!
(setq fill-column 80) ;; M-q should fill at 80 chars, not 75

;; general programming things
(show-paren-mode 1)  ;; highlight matching parenthasis
(menu-bar-mode -1) ;; minimal chrome
(tool-bar-mode -1) ;; no toolbar
(scroll-bar-mode -1) ;; disable scroll bars
(setq-default truncate-lines 1)
(desktop-save-mode 1) ;; auto-save desktop state for a later time.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)  ;; buffernames that are foo<1>, foo<2> are hard to read. This makes them foo|dir  foo|otherdir
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;;; erc
;; by default, erc alerts you on any activity. I only want to hear
;; about mentions of nick or keyword
(setq erc-current-nick-highlight-type 'all)
(setq erc-keywords '("jlilly"))
(setq erc-track-exclude-types '("JOIN" "PART" "NICK" "MODE" "QUIT"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)

;; javascript
(setq js-indent-level 2)

(ido-mode t);; fuzzy matching on find-file, buffer switch

(setq auto-mode-alist
      (append
       '(("\\.bashrc" . sh-mode))
       auto-mode-alist))

;; tempfiles, stolen from github://defunkt/emacs
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t
      backup-directory-alist `(("." . ,user-temporary-file-directory))
      auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-")
      auto-save-file-name-transforms `((".*" ,user-temporary-file-directory)))

;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'dired-load-hook (lambda ()
			     (load "dired-x")))
(add-hook 'dired-mode-hook (lambda ()
			     (dired-omit-mode 1)))
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2)
			    (local-set-key (kbd "C-M-h") 'windmove-left)))
(add-hook 'perl-mode-hook (lambda ()
			    (local-set-key (kbd "C-M-h") 'windmove-left)))
(add-hook 'c-mode-common-hook (lambda ()
                                (local-set-key (kbd "C-M-h") 'windmove-left)))
(add-hook 'eshell-mode-hook (lambda ()
			      (local-set-key (kbd "C-M-l") 'windmove-right)))
(add-hook 'debugger-mode-hook (lambda ()
			      (local-set-key (kbd "C-M-l") 'windmove-right)))
;; fun
(defun prompt-with-default-as-region (prompt)
  "Prompts with the PROMPT, prefilling the value with the region
  if active"
  (let ((default (if (and transient-mark-mode mark-active)
		    (buffer-substring-no-properties (region-beginning) (region-end))
		  nil)))
    (read-string prompt default)))

(defun dictionary ()
  "Opens a web page to define the word at point."
  (interactive)
  (let ((word (prompt-with-default-as-region "word: ")))
    (browse-url (concat "http://www.google.com/search?q=define:+" word))))

(defun erc-carbon ()
  "Connects to my IRC bouncer"
  (interactive)
  (erc :server "carbon.justinlilly.com" :port 9999 :nick "justinlilly"))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

;; org mode
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DONE")))

;; scpaste
(setq scpaste-http-destination "http://caesium.justinlilly.com/pastes"
      scpaste-scp-destination "justinlilly@caesium.justinlilly.com:/var/www/blog/pastes")

;; ibuffer configs
(setq ibuffer-saved-filter-groups
   '(("default"
      ("irc" (mode . erc-mode))
      ("background" (name . "^*.**$")))))

;; history
(setq savehist-additional-variables    ;; also save...
  '(search-ring regexp-search-ring)    ;; ... my search entries
  savehist-file "~/.emacs.d/savehist") ;; keep my home clean
(savehist-mode t)                      ;; do customization before activate

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(load-file "~/.emacs.d/google_setup.el") ;; google specific configurations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(menu-bar-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-inactive ((t (:inherit mode-line :background "color-20" :foreground "white" :box (:line-width -1 :color "grey40") :weight light)))))
