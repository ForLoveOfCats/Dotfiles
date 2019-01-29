;;Setup packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'load-path "~/Dotfiles/EmacsPlugins/")
(package-initialize)


;;Require external packages
(require 'projectile)
(require 'company)
(require 'expand-region)
(require 'smartparens-config)
(require 'real-auto-save)
(require 'flx-ido)
(require 'dired-sidebar)
(require 'multiple-cursors)
(require 'bind-key)
(require 'helm)
(require 'ivy)
;; (require 'helm-projectile)
(require 'highlight-indent-guides)
(require 'loop)
(require 'auto-highlight-symbol)
(require 'odin-mode)
(require 'flycheck-odin)


;;Only use one instance (used with EmacsAsEditor.sh)
(server-start)


;;Keybinds
(global-set-key (kbd "M-x") 'helm-M-x)
(defadvice mouse-set-point (before mouse-set-point-before activate) (deactivate-mark))
(global-set-key (kbd "C-z") 'undo)
;; (global-set-key (kbd "C-c C-c") 'kill-ring-save)
(require 'csharp-mode)
(define-key csharp-mode-map (kbd "C-c C-c") 'kill-ring-save) ;;Prevent csharp-mode from overriding kill-ring-save
(global-set-key (kbd "RET") 'nav/csharp-newline)
;; (bind-key* "C-v" 'yank)
(global-set-key (kbd "<backspace>") 'backward-delete-char)
(cua-mode)

(defun no-copy-kill-whole-line ()
  "kill-whole-line without copying to kill ring"
  (interactive)
  (move-beginning-of-line 1)
  (delete-region (point) (line-end-position))
  (delete-char -1)
  (next-line))
;; (global-set-key (kbd "S-<backspace>") 'no-copy-kill-whole-line)

(defun no-copy-kill-to-word ()
  "kills to the next word without copying to kill ring"
  (interactive)
  (delete-region
   (point)
   (progn
     (backward-word)
     (point))))
;; (global-set-key (kbd "C-<backspace>") 'no-copy-kill-to-word)

(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

;; (global-set-key (kbd "<f12>") 'omnisharp-go-to-definition)

(global-set-key (kbd "M-<backspace>") 'pop-global-mark)

(define-key company-active-map (kbd "<tab>") 'company-complete-common)
(define-key company-active-map (kbd "TAB") 'company-complete-common)

(define-key company-active-map (kbd "<return>") nil)
(define-key company-active-map (kbd "RET") nil)

;; (global-set-key (kbd "C-=") 'er/expand-region)

(defun create-and-move-to-newline-below ()
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-for-tab-command))
;; (global-set-key (kbd "C-<return>") 'create-and-move-to-newline-below)

(defun dup-line-below ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (pop kill-ring))
;; (global-set-key (kbd "M-S-<down>") 'dup-line-below)

(defun dup-line-above ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (previous-line)
  (move-end-of-line 1)
  (newline)
  (yank)
  (pop kill-ring))
;; (global-set-key (kbd "M-S-<up>") 'dup-line-above)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
;; (global-set-key (kbd "M-<up>") 'move-line-up)

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
;; (global-set-key (kbd "M-<down>") 'move-line-down)

;; Make escape quit anything
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (global-set-key (kbd "C-`") 'dired-sidebar-toggle-sidebar)

(defun toggle-semicolon ()
  (interactive)
  (save-excursion
	(move-end-of-line 1)
	(if (char-equal (char-before) ?\;)
		(delete-char -1)
	  (insert ?\;)
	  )
	)
  )
;; (global-set-key (kbd "C-;") 'toggle-semicolon)

;; (global-set-key (kbd "C-S-a") 'comment-region)

(defun highlight-then-select-next ()
	"Highlights current word, following presses selects next instance"
	(interactive)
	(if (region-active-p)
		(mc/mark-next-like-this 1)
	  (er/expand-region 1)
	  )
	)
;; (global-set-key (kbd "C-d") 'highlight-then-select-next)

;;Funky navigation mode
(setq nav/old-map (current-global-map))
(setq nav/is-enabled nil)

(defun er/prepare-for-more-expansions ())

(defun my/kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing)
	  )
	)
  (pop kill-ring)
  )

(defun my/kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (my/kill-thing-at-point 'word)
  )

(defun nav/is-uppercase (char)
	(seq-contains (number-sequence 65 90) char)
	)

(defun nav/is-whitespace (char)
  (if (or (= char ?\s) (= char ?\t))
	  t
	nil)
  )

(defun nav/char-is-word (char)
  (if (or (= char ?\s) (= char ?\t) (= char ?\n) (= char ?-) (= char ?_) (= char ?() (= char ?)) (= char ?{) (= char ?}) (= char ?\;) (= char ?/) (= char ?\") (= char ?\\) (= char ?.) (= char ?,))
	  t
	nil)
  )

(defun nav/right-word ()
  (interactive)
  (catch 'break
	(if (nav/is-whitespace (char-after))
		(progn
		  (loop-while t
			(right-char)
			(if (not (nav/is-whitespace (char-after)))
				(throw 'break "break")
				)
			)
		  )
	  )

	(if (and (nav/is-uppercase (char-after)) (nav/is-uppercase (char-after (+ (point) 1))))
		(progn
		  (loop-while t
			(right-char)
			(if (not (nav/is-uppercase (char-after)))
				(throw 'break "break")
				)
			)
		  )
	  )
	
	(if (nav/char-is-word (char-after))
		(progn
		  (right-char)
		  (throw 'break "break")
		  )
	  )

	(loop-while t
	  (right-char)
	  (if (or (nav/char-is-word (char-after)) (nav/is-uppercase (char-after)))
		  (throw 'break "break")
		)
	  )
	
	)
  )

(defun nav/left-word ()
  (interactive)
  (catch 'break
	(if (nav/is-whitespace (char-before))
		(progn
		  (loop-while t
			(left-char)
			(if (not (nav/is-whitespace (char-before)))
				(throw 'break "break")
			  )
			)
		  )
	  )

	(if (and (nav/is-uppercase (char-before)) (nav/is-uppercase (char-before (- (point) 1))))
		(progn
		  (loop-while t
			(left-char)
			(if (not (nav/is-uppercase (char-before)))
				(throw 'break "break")
			  )
			)
		  )
	  )

	(if (nav/char-is-word (char-before))
		(progn
		  (left-char)
		  (throw 'break "break")
		  )
	  )

	(loop-while t
	  (left-char)
	  (if (or (nav/char-is-word (char-before)) (nav/is-uppercase (char-after)))
		(throw 'break "break")
		)
	  )
	
	)
  )

(defun nav/delete-word-back ()
  (interactive)
  (if (= (point) 1)
	  nil
	(progn 
	  (activate-mark)
	  (set-mark (point))
	  (nav/left-word)
	  (backward-delete-char 1)
	  (deactivate-mark)
	  )
	)
  )

(defun nav/delete-word-forward ()
  (interactive)
  (if (= (point) 1)
	  nil
	(progn 
	  (activate-mark)
	  (set-mark (point))
	  (nav/right-word)
	  (backward-delete-char 1)
	  (deactivate-mark)
	  )
	)
  )

(defun nav/delete-to-line-start ()
  (interactive)
  (activate-mark)
  (set-mark (point))
  (beginning-of-line-text)
  (backward-delete-char 1)
  (deactivate-mark)
  )


(defun nav/delete-to-line-end ()
  (interactive)
  (activate-mark)
  (set-mark (point))
  (end-of-line)
  (backward-delete-char 1)
  (deactivate-mark)
  )

(defun nav/csharp-newline ()
  (interactive)
  (if (equal major-mode 'csharp-mode)
	  (progn
		(if (and (= (char-before) ?{) (= (char-after) ?}))
			(progn
			  (newline)
			  (indent-for-tab-command)
			  (previous-line)
			  (create-and-move-to-newline-below)
			  )
		  (newline)
		  )
		(indent-for-tab-command))
	(newline)
	)
  )

(defun nav/kill-buffer-or-window ()
  (interactive)
  (if (= (length (window-list)) 1)
	  (kill-buffer (current-buffer))
	(delete-window))
  )

(defun nav/toggle-selection ()
  (interactive)
  (if (not mark-active)
	  (progn
		(activate-mark)
		(set-mark (point))
		(message "Enabled mark")
		)
	(progn
	  (deactivate-mark)
	  (message "Disabled mark")
	  )
	)
  )

(defun nav/kill-line-or-lines ()
  (interactive)
  (if mark-active
	  (progn
		(backward-delete-char 1)
		(deactivate-mark)
		(no-copy-kill-whole-line)
		)
	(no-copy-kill-whole-line)
	)
  )

(defun nav/smart-tab ()
  (interactive)
  (if (eq major-mode (default-value 'major-mode))
	  (insert "	")
	(indent-for-tab-command)
	)
  )

(defun nav/enable ()
  (interactive)
  (setq nav/is-enabled t)
  (setq nav/old-map (current-global-map))
  (use-global-map (make-sparse-keymap))
  (set-face-foreground 'mode-line "white")
  (set-face-background 'mode-line "blue")

  (define-key csharp-mode-map ";" nil)
  (define-key csharp-mode-map "," nil)
  (define-key csharp-mode-map ";" nil)
  (define-key csharp-mode-map "/" nil)
  
  (global-set-key (kbd "w") (lambda () (interactive) (if helm-alive-p (helm-previous-line) (previous-line))))
  (global-set-key (kbd "s") (lambda () (interactive) (if helm-alive-p (helm-next-line) (next-line))))
  ;; (global-set-key (kbd "w") (lambda () (interactive) (previous-line)))
  ;; (global-set-key (kbd "s") (lambda () (interactive) (next-line)))
  (global-set-key (kbd "d") 'right-char)
  (global-set-key (kbd "a") 'left-char)

  (global-set-key (kbd "m") 'nav/toggle-selection)
  
  (global-set-key (kbd ",") 'beginning-of-line-text)
  (global-set-key (kbd "<") 'nav/delete-to-line-start)
  (global-set-key (kbd ".") 'end-of-line)
  (global-set-key (kbd ">") 'nav/delete-to-line-end)

  (global-set-key (kbd "'") 'backward-paragraph)
  (global-set-key (kbd "/") 'forward-paragraph)
  
  (global-set-key (kbd "<mouse-1>") 'mouse-set-point)
  (mouse-wheel-mode t)
  
  (global-set-key (kbd "]") 'nav/right-word)
  (global-set-key (kbd "[") 'nav/left-word)
  
  (global-set-key (kbd "h") 'describe-key)

  (global-set-key (kbd "l") (lambda () (interactive) (let ((old-pos (point))) (beginning-of-line) (kill-line) (yank) (goto-char old-pos))))
  
  (global-set-key (kbd "=") (lambda () (interactive) (er/expand-region 1)))
  (global-set-key (kbd "-") (lambda () (interactive) (er/expand-region -1)))

  (global-set-key (kbd "t") 'highlight-then-select-next)
  
  (global-set-key (kbd "RET") 'nav/csharp-newline)
  (global-set-key (kbd "n") 'create-and-move-to-newline-below)
  (global-set-key (kbd "SPC") 'self-insert-command)
  (global-set-key (kbd "<tab>") 'nav/smart-tab)
  (global-set-key (kbd ";") 'toggle-semicolon)

  (global-set-key (kbd "e") (lambda () (interactive) (comment-line 1) (previous-line) (indent-for-tab-command)))
  
  (global-set-key (kbd "z") 'undo)
  (global-set-key (kbd "g") (lambda () (interactive) (mc/keyboard-quit) (keyboard-quit)))

  (global-set-key (kbd "v") (lambda () (interactive) (yank) (indent-for-tab-command)))
  (global-set-key (kbd "c") 'kill-ring-save)

  (global-set-key (kbd "p") (lambda () (interactive) (nav/disable) (projectile-find-file)))
  (global-set-key (kbd "S-p") (lambda () (interactive) (nav/disable) (projectile-switch-project)))
  (global-set-key (kbd "o") (lambda () (interactive) (nav/disable) (projectile-grep)))
  (global-set-key (kbd "i") (lambda () (interactive) (nav/disable) (projectile-replace)))
  (global-set-key (kbd "f") (lambda () (interactive) (nav/disable) (omnisharp-helm-find-symbols)))
  (global-set-key (kbd "r") (lambda () (interactive) (nav/disable) (omnisharp-rename)))
  (global-set-key (kbd "y") 'omnisharp-go-to-definition)
  (global-set-key (kbd "u") 'omnisharp-helm-find-usages)
  (global-set-key (kbd "j") 'helm-execute-persistent-action)

  (global-set-key (kbd "<backspace>") 'backward-delete-char)
  (global-set-key (kbd "S-<backspace>") (lambda () (interactive) (backward-delete-char -1)))
  (global-set-key (kbd "\\") 'nav/delete-word-back)
  (global-set-key (kbd "|") 'nav/delete-word-forward)
  (global-set-key (kbd "k") 'nav/kill-line-or-lines)
  (global-set-key (kbd "x") 'my/kill-word-at-point)

  (global-set-key (kbd "b") (lambda () (interactive) (nav/disable) (helm-buffers-list)))
  (global-set-key (kbd "q") 'nav/kill-buffer-or-window)
  )

(defun nav/disable ()
  (interactive)
  (setq nav/is-enabled nil)
  (use-global-map nav/old-map)
  (set-face-foreground 'mode-line "black")
  (set-face-background 'mode-line "green")
  )

(defun nav/toggle ()
  (interactive)
  (if nav/is-enabled
	  (nav/disable)
	(nav/enable)
	)
  )

(bind-key* "S-SPC" 'nav/toggle)
(global-set-key (kbd "<tab>") 'nav/smart-tab)



(defun overwrite-mode ()
  (interactive))


;;Keyboard smooth scrolling
;; (setq redisplay-dont-pause t
;; scroll-margin 1
;; scroll-step 1
;; scroll-conservatively 10000
;; scroll-preserve-screen-position 1)
(setq scroll-step 1)


;;Parenthesis autocompletion and removal
(smartparens-global-mode 1)


;;Text selection settings
(transient-mark-mode 1) ;;Forces deselect when cursor move
(delete-selection-mode 1)  ;;Start typing to overwrite selection

(defadvice kill-ring-save (after keep-transient-mark-active ())
  "Override the deactivation of the mark."
  (setq deactivate-mark nil))
(ad-activate 'kill-ring-save)


;;Mouse settings
(setq mouse-wheel-progressive-speed nil)


;;Save recent files list
(recentf-mode 1)
(setq recentf-max-menu-items 25)


;;Helm fuzzy search stuff
(require 'helm-flx)
(helm-flx-mode +1)
(setq helm-M-x-fuzzy-match t)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq helm-flx-for-helm-find-files t)
(setq helm-flx-for-helm-locate t)


;;GUI stuff
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(setq initial-buffer-choice t)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)
(global-git-gutter-mode +1)
(tool-bar-mode -1)
(load-theme 'atom-one-dark t)
(global-display-line-numbers-mode)
(set-frame-font "Monospace 9" nil t)
(setq isearch-allow-scroll t)
(require 'ido)
;; (ido-mode t)
;; (ido-everywhere 1)
(flx-ido-mode 1)
;; (setq ido-everywhere t)
;; (ido-mode 1)
(ivy-mode 1)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil)
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(show-smartparens-global-mode t)
;; (setq sp-show-pair-delay 0)
;; (global-auto-highlight-symbol-mode t)
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "green")
(setq-default show-trailing-whitespace t)
(global-hl-line-mode)

(add-hook 'minibuffer-setup-hook 'eval-minibuffer-company)
(defun eval-minibuffer-company ()
  "enable company-mode during eval-expression"
  (if (eq this-command 'eval-expression)
	  (company-mode)))


;;Disable Autosave junk
(setq auto-save-default nil)
(setq make-backup-files nil)


;;Real Autosave
(add-hook 'text-mode-hook 'real-auto-save-mode)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 1) ;;Autosave every x seconds
(add-hook 'focus-out-hook (lambda () (save-some-buffers t))) ;;Save when switching buffers/selecting different app


;;Reload file if modified on disk
(global-auto-revert-mode)
(setq auto-revert-use-notify t)


;;Tabs'n'whitespace
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;;Global company-mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-require-match 'never)
(setq company-frontends '(company-tng-frontend company-pseudo-tooltip-frontend))


;GC
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 500)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))


;;C# stuff
(add-hook 'csharp-mode-hook 'omnisharp-mode)
;; (setq omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.32.1/run")
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)
(setq flycheck-checker-error-threshold 1000)
(setq omnisharp-expected-server-version "1.32.5")


(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-odin-setup))


;;Automatic stuff

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-idle-interval 0)
 '(git-gutter:update-interval 1)
 '(package-selected-packages
   (quote
	(counsel ivy d-mode zig-mode helm-flx magit helm-projectile loop highlight-indent-guides helm centered-cursor-mode bind-key multiple-cursors dired-sidebar expand-region flycheck-inline real-auto-save git-gutter projectile smartparens ace-window atom-one-dark-theme sublimity company omnisharp))))
