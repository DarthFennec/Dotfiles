;;;; Packages

;;; Archive List
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Package List
(setq package-list
      '(evil evil-tabs evil-leader evil-numbers
        evil-surround evil-quickscope
        helm helm-ag helm-projectile
        dtrt-indent autopair multi-term hydra package-utils
        python-mode groovy-mode haskell-mode markdown-mode
        highlight-quoted highlight-numbers paren-face
        monokai-theme))

;;; Package Maintenance
(defun update-packages () (interactive)
  ;; refresh package list
  (package-refresh-contents)
  ;; clean
  ;; install
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  ;; upgrade
  (package-utils-upgrade-all-no-fetch)
  ;; clear the minibuffer
  (message nil))

;;; Initialize
(require 'package)
(package-initialize)

;;; Requires
(require 'helm)
(require 'helm-config)
(require 'evil-leader)
(require 'evil)
(require 'evil-numbers)
(require 'evil-quickscope)
(require 'evil-surround)
(require 'autopair)
(require 'whitespace)
(require 'key-chord)
(require 'multi-term)

;;;; Behavior

;;; Custom Customization File
(setq custom-file "~/.emacs-custom.el")

;;; Autosaves And Backups
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))

;;; Set Custom Variables
(custom-set-variables
 ;; Projectile Settings
 '(projectile-enable-caching t)
 '(projectile-completion-system 'helm)
 ;; Better Scrolling
 '(scroll-step 1)
 '(scroll-conservatively 10000)
 ;; Better Behavior
 '(autopair-blink nil)
 '(vc-follow-symlinks t)
 '(require-final-newline t)
 '(inhibit-startup-screen t)
 ;; Better Display
 '(show-paren-mode t)
 '(column-number-mode t)
 '(elscreen-display-tab nil)
 '(whitespace-style '(face lines-tail trailing tab-mark))
 ;; Indentation
 '(dtrt-indent-mode t)
 '(indent-tabs-mode nil)
 ;; Autosaves And Backups
 '(auto-save-list-file-prefix autosave-dir)
 '(auto-save-file-name-transforms `((".*" ,autosave-dir t)))
 '(backup-directory-alist `((".*" . ,backup-dir)))
 ;; Multiterm
 '(multi-term-program "/bin/zsh")
 ;; Helm
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\*helm" "\\*messages\\*" "\\*help\\*" "\\*backtrace\\*"
     "\\*faces\\*" "\\*completions\\*" "\\*customize")))

;;; Disable GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Helm And Friends
(helm-mode 1)
(projectile-global-mode)
(helm-projectile-on)

;;; Evil And Friends
(global-evil-leader-mode)
(evil-mode 1)
(global-evil-tabs-mode t)

;;; Highlighting And Misc
(global-paren-face-mode)
(global-evil-surround-mode 1)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'autopair-mode)
(add-hook 'text-mode-hook 'autopair-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode)
(add-hook 'text-mode-hook 'turn-on-evil-quickscope-always-mode)

;;; Load Theme
(load-theme 'monokai t)

;;;; Keybindings

;;; Custom Ex Commands
(evil-ex-define-cmd "k[ill-buffer]" 'kill-this-buffer)

;;; Persistent Window Mode
(defhydra window-mode ()
  ("s" evil-window-split)
  ("v" evil-window-vsplit)
  ("n" evil-window-new)
  ("c" evil-window-delete)
  ("o" delete-other-windows)
  ("h" evil-window-left)
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)
  ("J" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("H" evil-window-move-far-left)
  ("L" evil-window-move-far-right)
  ("w" evil-window-next)
  ("W" evil-window-prev)
  ("t" evil-window-top-left)
  ("b" evil-window-bottom-right)
  ("T" evil-tabs-current-buffer-to-tab)
  ("p" evil-window-mru)
  ("r" evil-window-rotate-downwards)
  ("R" evil-window-rotate-upwards)
  ("+" balance-windows)
  ("=" evil-window-increase-height)
  ("-" evil-window-decrease-height)
  ("." evil-window-increase-width)
  ("," evil-window-decrease-width)
  ("q" nil))

;;; Escape Sequence
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
(key-chord-define minibuffer-local-map "jk" 'helm-like-unite/body)

;;; Swap Colon And Semicolon
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map ":" 'evil-repeat-find-char)
(define-key evil-visual-state-map ";" 'evil-ex)
(define-key evil-visual-state-map ":" 'evil-repeat-find-char)
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

;;; Show Current File Path
(defun show-file-path () (interactive) (message (buffer-file-name)))
(define-key evil-normal-state-map (kbd "C-g") 'evil-show-file-info)
(define-key evil-visual-state-map (kbd "C-g") 'evil-show-file-info)
(define-key evil-motion-state-map (kbd "C-g") 'evil-show-file-info)
(define-key evil-insert-state-map (kbd "C-g") 'evil-show-file-info)
(define-key evil-replace-state-map (kbd "C-g") 'evil-show-file-info)

;;; Yank Rest Of Line
(defun yank-line-rest () (interactive) (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "Y" 'yank-line-rest)
(define-key evil-motion-state-map "Y" 'yank-line-rest)

;;; C-w In Insert Mode
(define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-replace-state-map (kbd "C-w") 'evil-window-map)

;;; Window Jumps Column Zero
(defmacro jump-and-zero (jmp)
  `(lambda () (interactive) (,jmp) (evil-beginning-of-line)))
(define-key evil-normal-state-map "H" (jump-and-zero evil-window-top))
(define-key evil-normal-state-map "M" (jump-and-zero evil-window-middle))
(define-key evil-normal-state-map "L" (jump-and-zero evil-window-bottom))
(define-key evil-visual-state-map "H" (jump-and-zero evil-window-top))
(define-key evil-visual-state-map "M" (jump-and-zero evil-window-middle))
(define-key evil-visual-state-map "L" (jump-and-zero evil-window-bottom))
(define-key evil-motion-state-map "H" (jump-and-zero evil-window-top))
(define-key evil-motion-state-map "M" (jump-and-zero evil-window-middle))
(define-key evil-motion-state-map "L" (jump-and-zero evil-window-bottom))

;;; Evil Numbers
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)

;;; Helm More Evil Motions
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-n") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-p") 'helm-delete-minibuffer-contents)

;;; Automatic Indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; Helm M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;;; Leader Bindings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "t" 'multi-term
  "q" 'update-packages
  "w" 'window-mode/body
  "e" 'helm-find-files
  "b" 'helm-buffers-list
  "s" 'helm-projectile-ag
  "f" 'helm-projectile-find-file
  "p" 'helm-projectile-switch-project
  "r" (lambda () (interactive) (load-file "~/.emacs")))
