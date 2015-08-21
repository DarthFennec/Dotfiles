;;; Package
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq package-list
      '(evil
        evil-leader
        evil-numbers
        evil-surround
        evil-snipe
        evil-tabs
        evil-escape
        key-chord
        autopair
        hydra
        helm
        helm-ag
        helm-projectile
        dtrt-indent
        python-mode
        haskell-mode
        discover-my-major
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

;;; Quiet Startup
(setq inhibit-startup-screen t)

;;; Theme
(load-theme 'monokai t)

;;; Helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;;; Make Helm More Evil
(defhydra helm-like-unite ()
  ("<SPC>" helm-toggle-visible-mark)
  ("a" helm-toggle-all-marks)
  ("v" helm-execute-persistent-action)
  ("g" helm-beginning-of-buffer)
  ("h" helm-previous-source)
  ("l" helm-next-source)
  ("G" helm-end-of-buffer)
  ("j" helm-next-line)
  ("k" helm-previous-line)
  ("q" nil))

;;; Evil Mode And Leaders
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "q" 'update-packages
  "f" 'helm-command-prefix
  "r" '(lambda () (interactive) (load-file "~/.emacs")))
(require 'evil)
(evil-mode 1)
(custom-set-variables '(elscreen-display-tab nil))
(global-evil-tabs-mode t)

;;; Line Modes
(global-linum-mode t)
(column-number-mode t)
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
(setq vc-follow-symlinks t)

;;; Show Whitespace
(require 'whitespace)
(setq whitespace-style '(face lines-tail indentation trailing tab-mark))
(global-whitespace-mode)

;;; Escape Sequence
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
(key-chord-define minibuffer-local-map "jk" 'helm-like-unite/body)

;;; Keybindings
(defun yank-line-rest () (interactive) (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "Y" 'yank-line-rest)
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map ":" 'evil-repeat-find-char)

;;; Clearing Eshell
(defun eshell/cls () (let ((inhibit-read-only t)) (erase-buffer)))
