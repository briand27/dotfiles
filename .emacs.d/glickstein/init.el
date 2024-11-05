;; From Writing GNU Emacs Extensions

(autoload 'insert-time "timestamp"
  "Insert the current time according to insert-time-format."
  t)
(autoload 'insert-date "timestamp"
  "Insert the current date according to insert-date-format."
  t)
(autoload 'update-writestamps "timestamp"
  "Find writestamps and replace them with the current time."
  nil)
(autoload 'update-modifystamps "timestamp"
  "Find modifystamps and replace them with the given time."
  nil)

(setq enable-local-variables 'query)

;; ---------------------------------------

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 280)

(load-theme 'tango-dark)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

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
  :bind (("C-f" . swiper)
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

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Messing around --------------

;; ;; TODO: Hook these up to work with Helpful describe-key
;; (general-define-key
;;  ;; Remap help to Ctrl-i like "info"
;;  "C-i" 'help-command
;;  ;; Ctrl-f like "Find" with Swiper
;;  "C-f" 'swiper
;;  ;; vim-like home row navigation
;;  "C-h" 'backward-char
;;  "C-j" 'next-line
;;  "C-k" 'previous-line
;;  "C-l" 'forward-char
;;  "C-w" 'forward-to-word
;;  "C-b" 'backward-to-word
;;  ;; Ctrl-s for "Save"
;;  "C-s" 'save-buffer
;;  ;; Ctrl-d for "delete line"
;;  "C-d" 'kill-line)

;; -----------------------------

(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes)

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

;; These key bindings are not recognized by clm?
;; nvm it's just some of the commands?
(use-package general
  :config
  (general-define-key
   ;; Remap help to Ctrl-i like "info"
   "C-i" 'help-command
   ;; Ctrl-f like "Find" with Swiper
   "C-f" 'swiper
   ;; vim-like home row navigation
   "C-h" 'backward-char
   "C-j" 'next-line
   "C-k" 'previous-line
   "C-l" 'forward-char
   "C-w" 'forward-word
   "C-b" 'backward-word
   ;; Ctrl-s for "Save"
   "C-s" 'save-buffer
   ;; Ctrl-d for "delete line"
   "C-d" 'kill-line)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (rune/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose-theme")))

;; I like vim but I think I'll set up my own keybindings
;; TODO:
;; C-j C-k C-h C-l - < > ^ v
;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-i-jump nil)
;;   :config
;;   (evil-mode 1)

;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;;   ;; Use visual line motions even outside of visual-line-mode buffers
;;   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;   (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
;;   (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :ext t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; doesn't exist? 
;; (use-package evil-magit
;;   :after magit)

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
