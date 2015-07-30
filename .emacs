;;; Package
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))
(setq package-list
      '(evil
	evil-leader
	projectile
	key-chord
	monokai-theme))
(package-initialize)

;;; Package Maintenance
; todo: add cleanup and update
(defun update-packages () (interactive)
  (package-refresh-contents)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  (message nil))

;;; Theme
(load-theme 'monokai t)
(custom-set-faces
 '(default ((t (:family "Source Code Pro"
		:foundry "adobe"
		:slant normal
		:weight semi-bold
		:height 113
		:width normal)))))

;;; Evil Mode
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "q" 'update-packages)
(require 'evil)
(evil-mode 1)

;;; Projectile
(projectile-global-mode)

;;; Line Modes
(global-linum-mode t)
(global-hl-line-mode +1)

;;; Disable GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Better Behavior
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq require-final-newline t)
(custom-set-variables '(show-paren-mode t))

;;; Escape Sequence
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)

;;; Keybindings
(defun yank-line-rest () (interactive) (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "Y" 'yank-line-rest)
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map ":" 'evil-repeat-find-char)
