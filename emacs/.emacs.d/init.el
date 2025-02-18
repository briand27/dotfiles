(require 'package)

;; Basic configuration setup
(setq inhibit-startup-message t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(scroll-bar-mode -1)			; Disable visible scrollbar
(tool-bar-mode -1)			; Disable the toolbar
(tooltip-mode -1)			; Disable tooltips
(set-fringe-mode 10)			; Give some breathing room
(menu-bar-mode -1)			; Disable the menu bar
(setq visible-bell t)			; Set up the visible bell
(if
 (find-font (font-spec :name "Fira Code"))
	    (set-face-attribute 'default nil :font "Fira Code" :height 140))

;; TODO
;; - [done] kill full line where cursor is
;; - repeat last command - macros?
;; - kill region
;; - [done] insert and go-to new line C-o
;; - lint and format
;; - run test file
;; - go to test file (run file searcher as separate process)
;; - cycle between markers

(defun insert-newline-after (&optional n)
  "Insert N newlines after the line on the cursor and go to the end."
  (interactive "P")
  (if (< (prefix-numeric-value n) 0)
      (progn
	(if (eq (line-number-at-pos) 1)
	    (progn
	      (move-beginning-of-line nil)
	      (newline-and-indent)
	      (setq n (+ 1 n))))
	(progn
	  (previous-line)
	  (setq n (- n)))))
  (move-end-of-line nil)
  (newline-and-indent n))

;; Doesn't work at beginning of buffer
(defun insert-newline-before (&optional n)
  "Insert N newlines before the cursor and go to the end."
  (interactive "P")
  (insert-newline-after (- (prefix-numeric-value n))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line lumbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

;; Automatically added when (use-package command-log-mode) was evaluated
;; TODO: Find out why these were added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4" default))
 '(package-selected-packages
   '(no-littering visual-fill-column org-bullets markdown-mode counsel-projectile evil-magit magit projectile all-the-icons hydra evil-collection evil general doom-themes helpful ivy-rich which-key rainbow-delimiters compat doom-modeline counsel ivy command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

;; may need to run all-the-icons-install-fonts or nerd-icons-install-fonts to work
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :config (load-theme 'doom-moonlight t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describle-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; TODO: repeat-mode. C-d l l l l should delete 4 lines and such
;; These key bindings are not recognized by clm?
;; nvm it's just some of the commands?
(use-package general
  :config
  (general-auto-unbind-keys)
  (general-define-key
   ;; Remap help to Ctrl-i like "info"
   ;; "C-i" 'help-command
   ;; Ctrl-f like "Find" with Swiper
   ;; "C-f" 'swiper
   ;; vim-like home row navigation
   ;; "C-h" 'backward-char
   ;; "C-j" 'next-line
   ;; "C-k" 'previous-line
   ;; "C-l" 'forward-char
   ;; "C-w" 'forward-word
   ;; "C-b" 'backward-word
   ;; Ctrl-s for "Save"
   ;; "C-s" 'save-buffer
   ;; Ctrl-s for "Swiper / Search"
   "C-s" 'swiper
   ;; Ctrl-d for "delete line"
   ;; "C-d" 'kill-whole-line
   ;; Ctrl-o uses vim-line o rather than open-line
   "C-o" 'insert-newline-after
   ;; I do this by mistake a lot
   "C-<return>" 'newline
   ;; C-g for "go to line"
   ;; "C-g" 'goto-line
   ;; Ctrl-Shift-f for global find
   "C-c f" 'fzf)
  ;; TODO: use hydra for "delete mode"?
  (general-create-definer delete-keys
    ;; :keymaps '(global)		; TODO: what this
    :prefix "C-d"
    :global-prefix "C-d"		; TODO: how is this different than above?
  )
  (delete-keys				; TODO: fix this
    "l" '(kill-whole-line :which-key "kill-line")
    "d" '(kill-whole-line :which-key "kill-line")
    "w" '(kill-whole-word :which-key "kill-word")))
  ;; (general-create-definer briand/project-find-keys
    ;; :prefix "C-p")
  ;; (briand/project-find-keys
   ;; "p" '(counsel-fzf :which-key "fuzzy find file")
   ;; "f" '(counsel-projectile-rg :which-key "ripgrep project")))
(defun kill-whole-word (&optional n)
  "Kill N tokens following the cursor is currently on."
  (interactive "P")
  (if (eq (char-syntax (preceding-char)) ?w)
      (backward-word))
  (kill-word n))

(use-package hydra)

(use-package fzf
  :config (setenv "FZF_DEFAULT_COMMAND" "rg --files"))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :ext t))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1))))
    ;; (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package no-littering)

;; From "Writing GNU Emacs Extensions" By Bob Glickstein

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))

(defun point-to-bottom ()
  "Put point at beginning of last visible line."
  (interactive)
  (move-to-window-line -1))

(defun line-to-top ()
  "Move current line to top of window."
  (interactive)
  (recenter 0))

(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn
        (setq buffer-read-only t)
        (message "File is a symlink"))))

(add-hook 'find-file-hooks 'read-only-if-symlink)

;; alt with lambda
;; (add-hook 'find-file-hooks
;;           '(lambda ()
;;              (if (file-symlink-p buffer-file-name)
;;                  (progn
;;                    (setq buffer-read-only t)
;;                    (message "File is a symlink")))))

;;  to remove
;; (remove-hook 'find-file-hooks 'read-only-if-symlink)

(defun visit-target-instead ()
  "Replace this buffer with a buffer visiting the link target."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (find-alternate-file target)
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))

(defun clobber-symlink ()
  "Replace symlink with a copy of the file."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (if (yes-or-no-p (format "Replace %s with %s? "
                                     buffer-file-name
                                     target))
              (progn
                (delete-file buffer-file-name)
                (write-file buffer-file-name)))
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))

(defadvice switch-to-buffer (before existing-buffer
                             activate compile)
  "When interactive, switch to existing buffers only,
unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
                      (other-buffer)
                      (null current-prefix-arg)))))

;; (defvar unscroll-to nil
;;   "Text position for next call to 'unscroll'.")

;; (defadvice scroll-up (before remember-for-unscroll
;;                       activate compile)
;;   "Remember where we started from, for 'unscroll'."
;;   (if (not (eq last-command 'scroll-up))
;;       (setq unscroll-to (point))))

;; (defun unscroll ()
;;   "Jump to location specified by 'unscroll-to'."
;;   (interactive)
;;   (goto-char unscroll-to))

;; (defvar unscroll-point nil
;;   "Cursor position for next call to 'unscroll'.")
;; (defvar unscroll-window-start nil
;;   "Window start for next call to 'unscroll'.")
;; (defvar unscroll-hscroll nil
;;   "Hscroll for next call to 'unscroll'.")

;; (defadvice scroll-up (before remember-for-unscroll
;;                       activate compile)
;;   "Remember where we started from, for 'unscroll'."
;;   (if (not (eq last-command 'scroll-up))
;;       (progn
;;         (setq unscroll-point (point))
;;         (setq unscroll-window-start (window-start)))))
;; (defun unscroll ()
;;   "Revert to 'unscroll-point' and 'unscroll-window-start'."
;;   (interactive)
;;   (goto-char unscroll-point)
;;   (set-window-start nil unscroll-window-start))

;; (defadvice scroll-up (before remember-for-unscroll
;;                       activate compile)
;;   "Remember where we started from, for 'unscroll'."
;;   (if (not (eq last-command 'scroll-up))
;;       (setq unscroll-point (point)
;;             unscroll-window-start (window-start)
;;             unscroll-hscroll (window-hscroll))))
(defun unscroll ()
  "Revert to 'unscroll-point' and 'unscroll-window-start'."
  (interactive)
  (if (not unscroll-point)
      (error "Cannot unscroll yet"))
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

(defadvice scroll-up (before remember-for-unscroll
                      activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-down (before remember-for-unscroll
                        activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-left (before remember-for-unscroll
                        activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-right (before remember-for-unscroll
                         activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defvar unscroll-point (make-marker)
  "Cursor position for next call to 'unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to 'unscroll'.")

(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn
        (set-marker unscroll-point (point))
        (set-marker unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))


;; minor mode

(defvar refill-mode nil
  "Mode variable for refill minor mode.")

(make-variable-buffer-local 'refill-mode)

(defun refill-mode (&optional arg)
  "Refill minor mode."
  (interactive "P")
  (setq refil-mode
	(if (null arg)
	    (not refill-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if refill-mode
      (add-hook 'after-change-functions 'refill nil t)
    (remove-hook 'after-change-functions 'refill t)))

(defun refill (start end len)		; start and end of new text, len of old text
  "After a text change, refill the current paragraph."
  (let ((left (if (or (zerop len) (not (before-2nd-word-p start)))
		  start
		(max (save-excursion
		       (goto-char start)
		       (beginning-of-line 0)
		       (point))
		     (save-excursion
		       (goto-char start)
		       (backward-paragraph 1)
		       (point))))))
    (if (or (and (zerop len)
		 (same-line-p start end)
		 (short-line-p end))
	    (and (eq (char-syntax (preceding-char)) ?\) )
		 (looking-at "\\s *$")))
	nil
      (save-excursion
	(fill-region left end nil nil t))))) ; end variable is unnecessary here

(defun before-2nd-word-p (pos)
  "Does pos lie before the second word on the line?"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-syntax-forward (concat "^ " (char-to-string char-syntax ?\n)))
    (skip-syntax-forward " ")
    (< pos (point))))

(defun same-line-p (start end)
  "Are start and end on the same line?"
  (save-excursion
    (goto-char start)
    (end-of-line)
    (<= end (point))))

(defun short-line-p (pos)
  "Does line containing pos stay within `fill-column`?"
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (<= (current-column) fill-column)))

;; Lot of syntax here
;; See "Chapter 8: Evaluation and Error Recovery" from "Writing GNU Emacs Extensions"
(defmacro limited-save-excursion (&rest subexprs)
  "Like save-excursion, but only restores point."
  (let ((orig-point-symbol (make-symbol "orig-point")))
    `(let ((,orig-point-symbol (point-marker)))
       (unwind-protect
           (progn ,@subexprs)
         (goto-char ,orig-point-symbol)))))

;; Stripe

(defvar stripe-username "briand")

(defun my/use-eslint-from-node-modules ()
  (let ((eslint (get-eslint-executable)))
    (when (and eslint (file-executable-p eslint))
      (setq flycheck-javascript-eslint-executable eslint))))

(defun my/use-flow-from-node-modules ()
  (let ((flow (get-flow-executable)))
    (when (and flow (file-exists-p flow))
      (setq flycheck-javascript-flow-executable flow))))

(use-package flycheck
  :init
  (setq flycheck-ruby-rubocop-executable "bundle exec rubocop")
  (setq flycheck-ruby-executable (format "/Users/%s/.rbenv/shims/ruby" stripe-username))

  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)
                        '(ruby-rubylint)
                        '(json-jsonlist)
                        '(emacs-lisp-checkdoc))))

  ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  ;; (add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules)

  ;; ;; use eslint and flow with web-mode for jsx files
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; (flycheck-add-mode 'javascript-flow 'web-mode)
  ;; (flycheck-add-next-checker 'javascript-flow '(t . javascript-eslint))
  ;; (global-flycheck-mode))

;; (use-package projectile
  ;; :init
  ;; (setq projectile-indexing-method 'alien)
  ;; (setq projectile-use-git-grep t)
  ;; (setq projectile-tags-command "/usr/local/bin/ctags --exclude=node_modules --exclude=admin --exclude=.git --exclude=frontend --exclude=home --exclude=**/*.js -Re -f \"%s\" %s")

  ;; :config
  ;; (projectile-global-mode)

  ;; :bind-keymap
  ;; ("C-c p" 'projectile-command-map))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (setq projectil-indexing-method 'alien)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package ruby-mode
  :config
  (defun my-ruby-mode-hook ()
    (set-fill-column 80)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
    (setq ruby-insert-encoding-magic-comment nil))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

;; system crafters
;; (use-package lsp-mode
  ;; :commands (lsp lsp-deferred)
  ;; :init
  ;; (setq lsp-keymap-prefix "C-c l")
  ;; :config
  ;; (lsp-enable-which-key-integration t))

;; (use-package lsp-ui :commands lsp-ui-mode)

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors_list)

;; (use-package company
  ;; :after lsp-mode
  ;; :hook (lsp-mode . company-mode)
  ;; :bind (:map company-active-map
         ;; ("<tab>" . company-complete-selection))
        ;; (:map lsp-mode-map
         ;; ("<tab>" . company-indent-or-complete-common))
  ;; :custom
  ;; (company-minimum-prefix-length 1)
  ;; (company-idle-delay 0.0))

;; java
;; (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java :ensure nil)
;; (use-package helm-lsp)
;; (use-package helm :config (helm-mode))

(use-package yaml-mode)

;; typescript
;; (use-package typescript-mode
  ;; :mode "\\.ts\\'"
  ;; :hook (typescript-mode . lsp-deferred)
  ;; :config
  ;; (setq typescript-indent-level 2))

;; Set up LSP
;; Hint: use M-. to go to a definition, and M-, to go back.
;; (use-package lsp)
;; (use-package lsp-mode :commands lsp)
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
;; (add-hook 'ruby-mode-hook #'lsp)
;; (add-hook 'enh-ruby-mode-hook #'lsp)
;; (setq lsp-prefer-flymake :none)
;; (setq lsp-log-io t)
;; (setq lsp-enable-snippet nil)

;; Decides if the buffer is Ruby and in pay server
;; (defun activate-pay-server-sorbet-p (filename mode)
  ;; (and
   ;; (string-prefix-p (expand-file-name "~/stripe/pay-server")
                    ;; filename)
   ;; (or (eq major-mode 'ruby-mode) (eq major-mode 'enh-ruby-mode))))

;; Configure the connection to Sorbet
;; (lsp-register-client
 ;; (make-lsp-client :new-connection (lsp-stdio-connection '("pay" "exec" "scripts/bin/typecheck" "--lsp" "--enable-all-experimental-lsp-features"))
                  ;; :major-modes '(ruby-mode enh-ruby-mode)
                  ;; :priority 25
                  ;; :activation-fn 'activate-pay-server-sorbet-p
                  ;; :server-id 'stripe-sorbet-lsp))

;; (use-package helm
  ;; :bind (("M-x" . helm-M-x))
  ;; :config
  ;; (require 'helm-config)
  ;; (helm-mode 1))



(use-package smooth-scroll
  :config (smooth-scroll-mode))

(use-package web-mode
  :init
  (defun web-mode-customization ()
    "Customization for web-mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (add-hook 'web-mode-hook 'web-mode-customization)

  :mode ("\\.html?\\'" "\\.erb\\'" "\\.hbs\\'"
         "\\.jsx?\\'" "\\.json\\'" "\\.s?css\\'"
         "\\.less\\'" "\\.sass\\'" "\\.tsx?\\'"))

(defun get-eslint-executable ()
  (let ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "package.json")))
    (and root
         (expand-file-name "node_modules/eslint/bin/eslint.js"
                           root))))

(defun my/use-eslint-from-node-modules ()
  (let ((eslint (get-eslint-executable)))
    (when (and eslint (file-executable-p eslint))
      (setq flycheck-javascript-eslint-executable eslint))))


(use-package projectile-ripgrep)

;; TODO: figure out how to change evil mode keybindings to not interfere with preferred emacs keybindings
;; (use-package evil
  ;; :init
  ;; (setq evil-want-integration t)
  ;; (setq evil-want-keybinding nil)
  ;; (setq evil-want-C-u-scroll nil)
  ;; (setq evil-want-C-i-jump nil)
  ;; (setq evil-search-module 'swiper)
  ;; :config
  ;; (evil-mode 1)
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;; (use-package evil-collection
  ;; :after evil
  ;; :config
  ;; (evil-collection-init))

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package tramp)

(use-package consult
  :bind
  ([remap goto-line] . consult-goto-line))

(use-package protobuf-mode)

(use-package format-all
  :commands format-all-mode
  :hook (progmode . format-all-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (ruby-mode . lsp-deferred)
  (java-mode . lsp-deferred)
  :commands
  (lsp lsp-deferred)
  :config
  (setq lsp-sorbet-as-add-on t)
  (setq lsp-sorbet-use-bundler t)
  (setq lsp-enable-which-key-integration t)
  (setq read-process-output-max (* 1024 1024)) ;; 1 MB
  (setq gc-cons-threshold 100000000)
  (setq lsp-use-plists t)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("pay" "exec" "scripts/bin/typecheck" "--lsp" "--enable-all-experimental-lsp-features"))
                    :major-modes '(ruby-mode enh-ruby-mode)
                    :priority 25
                    :activation-fn 'activate-pay-server-sorbet-p
                    :server-id 'stripe-sorbet-lsp)))

(defun activate-pay-server-sorbet-p (filename mode)
  (and
   (string-prefix-p (expand-file-name "~/stripe/pay-server")
                    filename)
   (or (eq major-mode 'ruby-mode) (eq major-mode 'enh-ruby-mode))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy)

(use-package lsp-treemacs
  :after lsp)

(use-package flycheck)

(use-package lsp-java
  :ensure t
  :custom
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.43.0/jdt-language-server-1.43.0-202412191447.tar.gz")
  ;; default java vmargs for vscode
  ;; source https://emacs-lsp.github.io/lsp-java/#eclipse-jdt-language-server
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))
  :config
  (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)

(use-package bazel)

(use-package org-roam
  :config
  (make-directory "~/org-roam")
  (setq org-roam-directory (file-truename "~/org-roam"))
  (org-roam-db-autosync-mode))
