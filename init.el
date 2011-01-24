;;; desires
;; it would be nice to build my tags via a key command. append-to: /path/to/file  from directory: /path/to/
;; optional java dev environment. Doesn't load at startup, but is loaded via M-x start-jde
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

;; general programming things
(show-paren-mode 1)  ;; highlight matching parenthasis
(menu-bar-mode -1) ;; minimal chrome
(if (functionp tool-bar-mode)
    (tool-bar-mode -1))
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)  ;; buffernames that are foo<1>, foo<2> are hard to read. This makes them foo|dir  foo|otherdir

;;; erc
;; by default, erc alerts you on any activity. I only want to hear
;; about mentions of nick or keyword
(setq erc-current-nick-highlight-type 'all)
(setq erc-keywords '("gencal"))
(setq erc-track-exclude-types '("JOIN" "PART" "NICK" "MODE" "QUIT"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)

;; javascript
(setq js-indent-level 2)

;; Autocomplete
(add-to-list 'load-path "~/.emacs.d/vendor/autocomplete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/autocomplete/ac-dict")
(ac-config-default)

(ido-mode t);; fuzzy matching on find-file, buffer switch

;; magit, interface to git via emacs
(add-to-list 'load-path "~/.emacs.d/submodules/magit/")
(require 'magit)

;; textile mode
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile" . textile-mode))

(setq auto-mode-alist
      (append
       '(("\\.textile" . textile-mode)
	 ("\\.bashrc" . sh-mode))
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


;;; fun bits
(defun dictionary ()
  "Opens a web page to define the word at point."
  (interactive)
  (browse-url (concat "http://www.google.com/search?q=define:+" (thing-at-point 'word))))

(defun nose (venv-name)
  "Runs nose on the current buffer using a particular virtualenv"
  ; @@@ TODO: set DJANGO_SETTINGS_MODULE somehow automatically
  (interactive "sVirtualenv: ")
  (compile (concat "DJANGO_SETTINGS_MODULE=\"dashboard.settings\" "
		   "/home/" user-login-name "/.virtualenvs/" venv-name "/bin/nosetests "
		   buffer-file-name)))


;;; epackage.el
;; One big file to boot all installed packages
;; Automatically generated. Do not edit.
(add-to-list 'load-path "~/.emacs.d/vendor/epackage/")
(load "~/.emacs.d/00conf/epackage-loader" 'noerr)

;;  M-x epackage to start package manager
(autoload 'epackage "epackage" "" t)

(autoload 'epackage-loader-file-byte-compile    "epackage" "" t)
(autoload 'epackage-loader-file-generate        "epackage" "" t)
(autoload 'epackage-cmd-autoload-package        "epackage" "" t)
(autoload 'epackage-cmd-enable-package          "epackage" "" t)
(autoload 'epackage-cmd-disable-package         "epackage" "" t)
(autoload 'epackage-cmd-activate-package        "epackage" "" t)
(autoload 'epackage-cmd-deactivate-package      "epackage" "" t)
(autoload 'epackage-cmd-clean-package           "epackage" "" t)
(autoload 'epackage-cmd-remove-package          "epackage" "" t)
(autoload 'epackage-cmd-upgrade-package         "epackage" "" t)
(autoload 'epackage-cmd-upgrade-all-packages    "epackage" "" t)
(autoload 'epackage-cmd-download-sources-list   "epackage" "" t)
(autoload 'epackage-cmd-download-package        "epackage" "" t)
(autoload 'epackage-initialize                  "epackage" "" t)
(autoload 'epackage-version                     "epackage" "" t)
(autoload 'epackage-documentation               "epackage" "" t)
