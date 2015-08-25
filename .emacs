;;; Packages
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq package-list
      '(evil
        evil-leader
        evil-numbers
        evil-surround
        evil-tabs
        autopair
        hydra
        helm
        dtrt-indent
        multi-term
        python-mode
        haskell-mode
        monokai-theme))
(package-initialize)

;;; Package Maintenance
;; todo: add cleanup and update
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

;;; Helm And Friends
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
;; Make Helm More Evil
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
  ("<RET>" nil)
  ("q" nil))

;;; Evil And Friends
;; Evil Leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
;; Evil Mode
(require 'evil)
(evil-mode 1)
;; Evil Tabs
(custom-set-variables '(elscreen-display-tab nil))
(global-evil-tabs-mode t)
;; Evil Numbers
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)
;; Evil Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;;; Line Modes
(global-linum-mode t)
(column-number-mode t)
(global-hl-line-mode +1)

;;; Disable GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Hilight Matches
(custom-set-variables '(show-paren-mode t))

;;; Better Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;;; Better Behavior
(setq vc-follow-symlinks t)
(setq require-final-newline t)

;;; Indentation
(setq-default indent-tabs-mode nil)
(custom-set-variables '(dtrt-indent-mode t))
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; Autoparing
(require 'autopair)
(setq autopair-blink nil)
(autopair-global-mode)

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
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map ":" 'evil-repeat-find-char)
(define-key evil-visual-state-map ";" 'evil-ex)
(define-key evil-visual-state-map ":" 'evil-repeat-find-char)
(defun show-file-path () (interactive) (message (buffer-file-name)))
(define-key evil-normal-state-map (kbd "C-g") 'show-file-path)
(define-key evil-visual-state-map (kbd "C-g") 'show-file-path)
(define-key evil-insert-state-map (kbd "C-g") 'show-file-path)
(define-key evil-replace-state-map (kbd "C-g") 'show-file-path)
(defun yank-line-rest () (interactive) (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "Y" 'yank-line-rest)

;;; Leader Bindings
(evil-leader/set-key
  "q" 'update-packages
  "f" 'helm-command-prefix
  "r" '(lambda () (interactive) (load-file "~/.emacs")))

;;; Backups And Autosaves
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;;; Multiterm
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;;; Clearing Eshell
(defun eshell/cls () (let ((inhibit-read-only t)) (erase-buffer)))
