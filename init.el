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
(tool-bar-mode -1) ;; no toolbar
(scroll-bar-mode -1) ;; disable scroll bars
(setq-default truncate-lines 1)
(desktop-save-mode 1) ;; auto-save desktop state for a later time.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)  ;; buffernames that are foo<1>, foo<2> are hard to read. This makes them foo|dir  foo|otherdir
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(load-theme 'tsdh-dark)

(defun prompt-with-default-as-region (prompt)
  "Prompts with the PROMPT, prefilling the value with the region
  if active"
  (let ((default (if (and transient-mark-mode mark-active)
		    (buffer-substring-no-properties (region-beginning) (region-end))
		  nil)))
    (read-string prompt default)))

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
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2)
			    (local-set-key (kbd "C-M-h") 'windmove-left)))

;;; fun bits
(setq VENV-BASE "/home/justinlilly/.virtualenvs/")
(defun add-to-PATH (dir)
  ; taken from https://github.com/dreid/emacs-config/blob/master/dreid/paths.el
  "Add the specified path element to the Emacs PATH"
  (interactive "DEnter a directory to be added to PATH: ")
  (if (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH"
              (concat (expand-file-name dir)
                      path-separator
                      (getenv "PATH")))))

(defun dictionary ()
  "Opens a web page to define the word at point."
  (interactive)
  (let ((word (prompt-with-default-as-region "word: ")))
    (browse-url (concat "http://www.google.com/search?q=define:+" word))))

(defun virtualenv (venv-name)
  "Sets the current virtualenv"
  (interactive "sVirtualenv: ")
  (setenv "VIRTUAL_ENV" venv-name)
  (add-to-PATH (concat VENV-BASE venv-name "/bin/")))

(defun activate-dashboard ()
  (interactive)
  (virtualenv "dashboard")
  (setenv "DJANGO_SETTINGS_MODULE" "dashboard.settings"))

(defun nose ()
  "Runs nose on the current buffer using a particular virtualenv"
  (interactive)
  (if (stringp (getenv "DJANGO_SETTINGS_MODULE"))
      (compile (concat "nosetests " buffer-file-name))
      (message "DJANGO_SETTINGS_MODULE not set. Please activate something.")))

(defun erc-carbon ()
  "Connects to my IRC bouncer"
  (interactive)
  (erc :server "carbon.justinlilly.com" :port 9999 :nick "justinlilly"))

(defun im-style ()
  "Runs the IM style guide on the code."
  (interactive)
  (compile (concat "DJANGO_SETTINGS_MODULE=\"dashboard.settings\" "
		   "/home/" user-login-name "/.virtualenvs/dashboard/bin/nosetests "
		   "/home/" user-login-name "/src/dashboard/tests/test_{pep8,pyflakes}.py")))

(defun jira ()
  (interactive)
  "Opens the JIRA ticket relevant for XXX-1234"
  (browse-url
   (concat "http://jira.invitemedia.com/browse/"
	   (thing-at-point 'filename))))  ; not really filename, but the regexp works.

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
