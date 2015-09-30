;;;; TODO List

;;; evil bindings in magit
;;; evil bindings in man

;;; update function should delete old packages
;;; indentation should be highlighted properly
;;; modeline should look nicer

;;; evil insert/paste should work in term-mode
;;; evil surround should be repeatable
;;; evil ex line should use helm
;;; helm resume should work with all helm buffers
;;; electric pair should not happen in ex line
;;; recursive popwin should behave always

;;; buffer list should be cleaner (temp buffers should autoclose?)
;;; projectile open project should be less restrictive somehow?
;;; alternate way to reload config?
;;; add col 80 ruler?

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
        dtrt-indent multi-term hydra key-chord package-utils magit popwin
        python-mode groovy-mode haskell-mode markdown-mode go-mode
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
(require 'popwin)
(require 'linum)
(require 'dtrt-indent)
(require 'whitespace)
(require 'key-chord)
(require 'multi-term)
(require 'markdown-mode)

;;;; Behavior

;;; Custom Customization File
(setq custom-file "~/.emacs-custom.el")

;;; Autosaves And Backups
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))

;;; Language Specific Changes
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(setq dtrt-indent-hook-mapping-list
      (cons '(groovy-mode c/c++/java c-basic-offset)
            dtrt-indent-hook-mapping-list))

;;; Set Custom Variables
(custom-set-variables
 ;; Projectile Settings
 '(projectile-enable-caching t)
 '(projectile-completion-system 'helm)
 ;; Better Scrolling
 '(scroll-step 1)
 '(scroll-conservatively 10000)
 ;; Better Behavior
 '(vc-follow-symlinks t)
 '(require-final-newline t)
 '(inhibit-startup-screen t)
 '(evil-leader/in-all-states t)
 ;; Better Display
 '(show-paren-mode t)
 '(column-number-mode t)
 '(elscreen-display-tab nil)
 '(whitespace-style '(face lines-tail trailing tab-mark))
 ;; Indentation And Fill
 '(dtrt-indent-max-merge-deviation 0.01)
 '(dtrt-indent-mode t)
 '(indent-tabs-mode nil)
 '(fill-column 80)
 '(sentence-end-double-space nil)
 ;; Autosaves And Backups
 '(auto-save-list-file-prefix autosave-dir)
 '(auto-save-file-name-transforms `((".*" ,autosave-dir t)))
 '(backup-directory-alist `((".*" . ,backup-dir)))
 ;; Terminal
 '(multi-term-program "/bin/zsh")
 '(Man-notify-method 'pushy)
 ;; Popwin
 '(popwin:special-display-config
   '((help-mode :dedicated t)
     (Buffer-menu-mode :dedicated t)
     (messages-buffer-mode :dedicated t)
     (completion-list-mode :noselect t :dedicated t)
     (" *undo-tree*" :width 60 :position right :dedicated t)
     ("^\\*helm[- ].+\\*$" :regexp t :dedicated t)
     (magit-diff-mode :noselect t :width 80 :position right)
     (magit-status-mode :dedicated t)))
 ;; Helm
 '(helm-split-window-preferred-function 'ignore)
 '(helm-boring-buffer-regexp-list
   '("^ " "^\\*Help\\*$" "^\\*Messages\\*$" "^\\*Buffer List\\*$"
     "^\\*helm[- ].+\\*$" "^\\*magit\\(-\\w+\\)?: .+\\*$")))

;;; Hook Editing Via Term-Mode
(when (require 'term nil t)
  (defadvice term-handle-ansi-terminal-messages
    (before handle-custom-ansi-terminal-messages activate)
    (when (string-match "\eAnSiT.+\n" message)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (command-code (aref message (+ start 6)))
             (argument
              (save-match-data
                (substring message (+ start 8)
                           (string-match "\r?\n" message (+ start 8))))))
        (cond ((= command-code ?e)
               (save-excursion (find-file-other-window argument)))
              ((= command-code ?x)
               (save-excursion (find-file argument)))
              ((= command-code ?m)
               (save-excursion (man argument))))))))

;;; Auto Open As Root
(defadvice find-file (after find-file-sudo activate)
  (unless (and buffer-file-name (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(defadvice find-file-other-window (after find-file-other-window-sudo activate)
  (unless (and buffer-file-name (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; Better Popup Windows
(popwin-mode 1)

;;; Disable GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Bury Scratch Instead Of Killing
(defun kill-buffer-query-functions-maybe-bury ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil) t))
(add-hook 'kill-buffer-query-functions 'kill-buffer-query-functions-maybe-bury)

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
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'text-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode)
(add-hook 'text-mode-hook 'turn-on-evil-quickscope-always-mode)

;;; Load Theme
(load-theme 'monokai t)

;;;; Face Customizations
(set-face-attribute 'linum nil :inverse-video nil :weight 'semi-bold)
(dotimes (i 6)
  (set-face-attribute
   (intern (concat "markdown-header-face-" (number-to-string (+ i 1))))
   nil :height 'unspecified))

;;;; Keybindings

;;; Ease Bindings
(defmacro bindall (fun premaps &rest bnds)
  (let* ((maps
          (mapcar (lambda (x)
                    (case x
                      ((N) 'evil-normal-state-map) ((V) 'evil-visual-state-map)
                      ((M) 'evil-motion-state-map) ((I) 'evil-insert-state-map)
                      ((R) 'evil-replace-state-map) (t x))) premaps))
         (pairs (mapcan (lambda (x) (mapcar (lambda (y) `(,y ,x)) maps)) bnds)))
    `(progn ,@(mapcar (lambda (x) `(,fun ,(car x) ,@(cadr x))) pairs))))

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
(bindall key-chord-define (I V R) ("jk" 'evil-normal-state))
(bindall define-key (I V R) ((kbd "C-g") 'evil-normal-state))

;;; Swap Colon And Semicolon
(bindall define-key (N V M) (";" 'evil-ex) (":" 'evil-repeat-find-char))

;;; Yank Rest Of Line
(defun evil-yank-line-rest () (interactive) (evil-yank (point) (point-at-eol)))
(bindall define-key (N M) ("Y" 'evil-yank-line-rest))

;;; Window Jumps Column Zero
(defmacro jmp-zero (jmp)
  (let ((jmpr (intern (concat "evil-window-" jmp))))
    `(lambda () (interactive) (,jmpr) (evil-beginning-of-line))))
(bindall define-key (N V M)
 ("H" (jmp-zero "top")) ("M" (jmp-zero "middle")) ("L" (jmp-zero "bottom")))

;;; Evil Numbers
(bindall define-key (N V)
 ((kbd "C-a") 'evil-numbers/inc-at-pt) ((kbd "C-s") 'evil-numbers/dec-at-pt))

;;; Helm Evil Motions
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-n") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-p") 'helm-delete-minibuffer-contents)

;;; Ex And C-w Bindings Everywhere
(define-key global-map (kbd "C-;") 'evil-ex)
(define-key global-map (kbd "C-w") 'evil-window-map)
(bindall define-key (I) ((kbd "C-w") 'evil-window-map))

;;; Helm M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;;; Leader Bindings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "t" 'multi-term
  "g" 'magit-status
  "q" 'update-packages
  "w" 'window-mode/body
  "u" 'undo-tree-visualize
  "d" 'evil-show-file-info
  "a" 'helm-resume
  "e" 'helm-find-files
  "b" 'helm-buffers-list
  "s" 'helm-projectile-ag
  "f" 'helm-projectile-find-file
  "p" 'helm-projectile-switch-project
  "r" (lambda () (interactive) (load-file "~/.emacs")))
