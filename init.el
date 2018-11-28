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
(add-to-list 'load-path "~/.emacs.d/EmacsPlugins/")
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


;;Keybinds
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c C-c") 'kill-ring-save)
(require 'csharp-mode)
(define-key csharp-mode-map (kbd "C-c C-c") 'kill-ring-save) ;;Prevent csharp-mode from overriding kill-ring-save
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "<backspace>") 'backward-delete-char)

(defun no-copy-kill-whole-line ()
  "kill-whole-line without copying to kill ring"
  (interactive)
  (move-beginning-of-line 1)
  (delete-region (point) (line-end-position))
  (delete-char -1))
(global-set-key (kbd "S-<backspace>") 'no-copy-kill-whole-line)

(defun no-copy-kill-to-word ()
  "kills to the next word without copying to kill ring"
  (interactive)
  (delete-region
   (point)
   (progn
     (backward-word)
     (point))))
(global-set-key (kbd "C-<backspace>") 'no-copy-kill-to-word)

(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

(global-set-key (kbd "<f12>") 'omnisharp-go-to-definition)

(global-set-key (kbd "M-<backspace>") 'pop-global-mark)

(define-key company-active-map (kbd "<tab>") 'company-complete-common)
(define-key company-active-map (kbd "TAB") 'company-complete-common)

(define-key company-active-map (kbd "<return>") nil)
(define-key company-active-map (kbd "RET") nil)

(global-set-key (kbd "C-=") 'er/expand-region)

(defun create-and-move-to-newline-below ()
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-for-tab-command))
(global-set-key (kbd "C-<return>") 'create-and-move-to-newline-below)

(defun dup-line-below ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (pop kill-ring))
(global-set-key (kbd "M-S-<down>") 'dup-line-below)

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
(global-set-key (kbd "M-S-<up>") 'dup-line-above)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(global-set-key (kbd "M-<up>") 'move-line-up)

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "M-<down>") 'move-line-down)

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

(global-set-key (kbd "C-`") 'dired-sidebar-toggle-sidebar)

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
(global-set-key (kbd "C-;") 'toggle-semicolon)

(global-set-key (kbd "C-S-a") 'comment-region)

(defun highlight-then-select-next ()
	"Highlights current word, following presses selects next instance"
	(interactive)
	(if (region-active-p)
		(mc/mark-next-like-this 1)
	  (er/expand-region 1)
	  )
	)
(global-set-key (kbd "C-d") 'highlight-then-select-next)


;;Keyboard smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)


;;Parenthesis autocompletion and removal
(smartparens-global-mode 1)


;;Text selection settings
(transient-mark-mode -1) ;;Forces deselect when cursor move
(delete-selection-mode 1)  ;;Start typing to overwrite selection

(defadvice kill-ring-save (after keep-transient-mark-active ())
  "Override the deactivation of the mark."
  (setq deactivate-mark nil))
(ad-activate 'kill-ring-save)


;;Mouse settings
(setq mouse-wheel-progressive-speed nil)


;;GUI stuff
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(setq initial-buffer-choice t)
(projectile-mode +1)
(global-git-gutter-mode +1)
(tool-bar-mode -1)
(load-theme 'atom-one-dark t)
(global-display-line-numbers-mode)
(set-frame-font "Monospace 9" nil t)
(setq isearch-allow-scroll t)
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
(run-with-idle-timer 1 t 'force-window-update) ;;Forces window to recalculate every second
(blink-cursor-mode 0)


;;Disable Autosave junk
(setq auto-save-default nil)
(setq make-backup-files nil)


;;Real Autosave
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 1) ;;Autosave every x seconds
(add-hook 'focus-out-hook (lambda () (save-some-buffers t))) ;;Save when switching buffers/selecting different app


;;Reload file if modified on disk
(global-auto-revert-mode)
(setq auto-revert-use-notify t)


;;Tabs'n'whitespace
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)


;;Global company-mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-require-match 'never)
(setq company-frontends '(company-tng-frontend company-pseudo-tooltip-frontend))


;;C# stuff
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(setq omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.32/run")
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)
(add-hook 'csharp-mode-hook #'flycheck-mode)


;;Automatic stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(multiple-cursors dired-sidebar expand-region flycheck-inline real-auto-save git-gutter projectile smartparens ace-window atom-one-dark-theme sublimity company omnisharp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
