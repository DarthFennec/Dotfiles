;;;; TODO List
;;; evil bindings in magit
;;; helm modeline should be pretty
;;; recursive popwin should behave always
;;; evil surround should be repeatable always
;;; evil insert/paste should work in term-mode
;;; helm resume should work with all helm buffers
;;; hitting o or O in a comment should continue the comment block

;;;; Packages

;;; Archive List
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Package List
(setq package-list
      '(monokai-theme
        helm helm-ag helm-projectile
        magit gitattributes-mode gitconfig-mode gitignore-mode
        evil evil-tabs evil-leader evil-numbers evil-commentary evil-space
        evil-surround evil-quickscope evil-exchange evil-visualstar
        dtrt-indent multi-term hydra key-chord package-utils popwin
        python-mode groovy-mode haskell-mode markdown-mode go-mode
        highlight-quoted highlight-numbers paren-face fill-column-indicator))

;;; Package Maintenance
(defun update-packages ()
  (interactive)
  ;; refresh package list
  (package-refresh-contents)
  ;; install
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  ;; upgrade
  (package-utils-upgrade-all-no-fetch)
  ;; clean
  (let ((curr nil)
        (from package-list)
        (to nil))
    (while from
      (setq curr (car from))
      (setq from (cdr from))
      (setq to (cons curr to))
      (dolist (dep (package-desc-reqs (cadr (assoc curr package-alist))))
        (unless (or (package-built-in-p (car dep))
                    (member (car dep) from) (member (car dep) to))
          (setq from (cons (car dep) from)))))
    (dolist (pack (mapcar #'car package-alist))
      (when (and (package-installed-p pack)
                 (not (package-built-in-p pack)) (not (member pack to)))
        (package-utils-remove-by-name pack))))
  ;; clear the minibuffer
  (message nil))

;;; Initialize
(require 'package)
(package-initialize)

;;; Add Custom Load Path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Requires
(require 'helm)
(require 'helm-config)
(require 'evil-leader)
(require 'evil)
(require 'evil-numbers)
(require 'evil-quickscope)
(require 'evil-surround)
(require 'evil-exchange)
(require 'evil-visualstar)
(require 'evil-commentary)
(require 'evil-space)
(require 'popwin)
(require 'linum)
(require 'dtrt-indent)
(require 'highlight-indent-guides)
(require 'fill-column-indicator)
(require 'whitespace)
(require 'key-chord)
(require 'magit)
(require 'woman)
(require 'man)
(require 'term)
(require 'multi-term)
(require 'markdown-mode)

;;;; Behavior

;; temporary, as Ubuntu's git version is too old
(setq magit--minimal-git "1.9.0")

;;; Custom Customization File
(setq custom-file "~/.emacs-custom.el")

(setq my-boring-buffers
      '("^ " "^\\*Help\\*$" "^\\*Messages\\*$" "^\\*Buffer List\\*$"
        "^\\*Backtrace\\*$" "^\\*Warnings\\*$" "^\\*WoMan-Log\\*$"
        "^\\*Compile-Log\\*$" "^\\*tramp/.+\\*$" "^\\*Faces\\*$"
        "^\\*helm[- ].+\\*$" "^\\*magit\\(-\\w+\\)?: .+$"))

;;; Autosaves And Backups
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))

;;; Language Specific Changes
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-to-list 'auto-mode-alist '("README\\.md$" . gfm-mode))
(add-to-list 'dtrt-indent-hook-mapping-list
             '(groovy-mode c/c++/java c-basic-offset))

(add-hook 'Man-mode-hook 'evil-motion-state)
(add-hook 'help-mode-hook 'evil-motion-state)

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
 '(mouse-avoidance-mode 'banish)
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
 '(truncate-partial-width-windows nil)
 '(sentence-end-double-space nil)
 ;; Autosaves And Backups
 '(auto-save-list-file-prefix autosave-dir)
 '(auto-save-file-name-transforms `((".*" ,autosave-dir t)))
 '(backup-directory-alist `((".*" . ,backup-dir)))
 ;; Terminal
 '(multi-term-program "/bin/zsh")
 '(Man-notify-method 'pushy)
 ;; Helm
 '(helm-split-window-preferred-function 'ignore)
 '(helm-boring-buffer-regexp-list my-boring-buffers)
 ;; Popwin
 '(popwin:special-display-config
   '((help-mode)
     (debugger-mode)
     (Buffer-menu-mode)
     (compilation-mode)
     (messages-buffer-mode)
     ("*helm-mode-completion-at-point*" :noselect t)
     ("*Warnings*")
     (" *undo-tree*" :width 60 :position right)
     ("^\\*helm[- ].+\\*$" :regexp t)
     (magit-diff-mode :noselect t :width 80 :position right)
     (magit-revision-mode :noselect t :width 80 :position right)
     (magit-status-mode))))

;;; Keep Temporary Buffers Hidden
(defadvice buffer-name (after boring-buffer-name disable)
  (when (and ad-return-value
             (not (eq ?\s (aref ad-return-value 0)))
             (some (lambda (x) (string-match-p x ad-return-value))
                   my-boring-buffers))
    (setq ad-return-value (concat " " ad-return-value))))

(defmacro make-boring-advice (funcname advname)
  `(defadvice ,funcname (around ,advname activate)
     (ad-enable-advice 'buffer-name 'after 'boring-buffer-name)
     (ad-activate 'buffer-name)
     ad-do-it
     (ad-disable-advice 'buffer-name 'after 'boring-buffer-name)
     (ad-activate 'buffer-name)))

(make-boring-advice record-window-buffer record-window-no-boring-buffer)
(make-boring-advice switch-to-prev-buffer switch-to-prev-no-boring-buffer)
(make-boring-advice switch-to-next-buffer switch-to-next-no-boring-buffer)
(make-boring-advice get-next-valid-buffer get-next-valid-no-boring-buffer)
(make-boring-advice menu-bar-update-buffers menu-bar-update-no-boring-buffers)
(make-boring-advice msb-invisible-buffer-p msb-invisible-boring-buffer-p)
(make-boring-advice mouse-buffer-menu-alist mouse-no-boring-buffer-menu-alist)

;;; Do Not Kill Scratch
(defun kill-buffer-query-functions-maybe-bury ()
  (not (string= (buffer-name (current-buffer)) "*scratch*")))
(add-hook 'kill-buffer-query-functions 'kill-buffer-query-functions-maybe-bury)

;;; Hook Editing Via Term-Mode
(defadvice term-handle-ansi-terminal-messages
    (before handle-custom-ansi-terminal-messages activate)
  (save-match-data
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
(defadvice find-file-noselect (before find-file-sudo activate)
  (unless (or (not filename) (file-writable-p filename))
    (setq filename (concat "/sudo::" (expand-file-name filename)))))

;;; Remove Dot Files From Helm Find File
(defadvice helm-ff-filter-candidate-one-by-one
    (around helm-hide-dot-files (file) activate)
  (unless (string-match-p "/\\.\\.?$" file) ad-do-it))

;;; Better Popup Windows
(popwin-mode 1)

;;; Disable GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; Helm And Friends
(helm-mode 1)
(projectile-global-mode 1)
(helm-projectile-on)

;;; Evil And Friends
(global-evil-leader-mode 1)
(evil-mode 1)
(evil-commentary-mode 1)
(evil-space-mode 1)
(global-evil-tabs-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode 1)
(evil-exchange-install)
(key-chord-mode 1)

;;; Highlighting And Misc
(global-paren-face-mode 1)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'text-mode-hook 'fci-mode)
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
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'text-mode-hook 'highlight-indent-guides-mode)
(add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode)
(add-hook 'text-mode-hook 'turn-on-evil-quickscope-always-mode)

;;; Load Theme
(load-theme 'monokai t)

;;; Face Customizations
(set-face-attribute 'linum nil :inverse-video nil :weight 'semi-bold)
(dotimes (i 6)
  (set-face-attribute
   (intern (concat "markdown-header-face-" (number-to-string (1+ i))))
   nil :height 'unspecified))

;;;; Modeline

(defvar-local my-indent-offset '(nil . nil))
(defun get-modeline-left ()
  (unless (eq major-mode (car my-indent-offset))
    (let ((offset (caddr (dtrt-indent--search-hook-mapping major-mode))))
      (setq my-indent-offset (cons major-mode offset))))
  (let* ((state (cond (defining-kbd-macro      " RECORD   ")
                      ((evil-normal-state-p)   " NORMAL   ")
                      ((evil-visual-state-p)   " VISUAL   ")
                      ((evil-insert-state-p)   " INSERT   ")
                      ((evil-replace-state-p)  " REPLACE  ")
                      ((evil-motion-state-p)   " MOTION   ")
                      ((evil-operator-state-p) " OPERATOR ")
                      (t                       " EMACS    ")))
         (buf (buffer-name))
         (mod (if (buffer-modified-p) "* " "  "))
         (ro (if buffer-read-only "[RO]" nil))
         (su (if (and buffer-file-name
                      (file-remote-p (buffer-file-name))) "[SU]" nil))
         (rosu (if (and (null ro) (null su)) "" (concat ro su " ")))
         (mmode (concat (symbol-name major-mode) " "))
         (offs (number-to-string (symbol-value (cdr my-indent-offset))))
         (tabs (if indent-tabs-mode "T" "S"))
         (indent (concat "[" offs tabs "]"))
         (coding (symbol-name (coding-system-type buffer-file-coding-system)))
         (eol (case (coding-system-eol-type buffer-file-coding-system)
                (0 "LF") (1 "CRLF") (2 "CR")))
         (encode (concat "[" coding ":" eol "]"))
         (dir (if (projectile-project-p) (projectile-project-name)
                (file-name-base (directory-file-name default-directory))))
         (proj (concat "[" dir vc-mode "]")))
    (add-text-properties 0 (length state) '(face mode-line-emphasis) state)
    (add-text-properties 0 (length buf) '(face mode-line-buffer-id) buf)
    (add-text-properties 0 1 '(face mode-line-emphasis) mod)
    (add-text-properties 0 (length mmode) '(face mode-line-emphasis) mmode)
    (set-text-properties 0 (length proj) nil proj)
    (list state buf mod rosu mmode indent encode proj)))

(defun get-modeline-right ()
  (let ((perc (format-mode-line "%p")))
    (when (string= perc "Bottom") (setq perc "Bot"))
    (when (string-match-p "[0-9]+%$" perc) (setq perc (concat perc "%")))
    (concat (format-mode-line "(%l,%c) ") perc " " elscreen-mode-line-string)))

(defun draw-modeline (lefts right)
  (let* ((lefts (reverse lefts))
         (max (- (window-width) (length (format-mode-line right))))
         (sizes (mapcar #'length lefts))
         (size (-sum sizes)))
    (while (and lefts (> size max))
      (setq size (- size (car sizes)))
      (setq sizes (cdr sizes))
      (setq lefts (cdr lefts)))
    (setq lefts (cons (make-string (- max size) ?\s) lefts))
    (setq lefts (cons right lefts))
    (reverse lefts)))

(setq-default
 mode-line-format
 '((:eval (draw-modeline (get-modeline-left) (get-modeline-right)))))

(kill-buffer "*Messages*")

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
(bindall key-chord-define (I V R) ("jk" 'evil-normal-state))
(bindall define-key (I V R) ((kbd "C-g") 'evil-normal-state))

;;; Bind Semicolon To Evil Ex
(bindall define-key (N V M) (";" 'evil-ex))

;;; Yank Rest Of Line
(defun evil-yank-line-rest () (interactive) (evil-yank (point) (point-at-eol)))
(bindall define-key (N M) ("Y" 'evil-yank-line-rest))

;;; Window Jumps Column Zero
(defmacro jmp-zero (jmp)
  `(lambda () (interactive) (,jmp) (evil-beginning-of-line)))
(bindall define-key (N V M)
         ("H" (jmp-zero evil-window-top)) ("M" (jmp-zero evil-window-middle))
         ("L" (jmp-zero evil-window-bottom)))

;;; Evil Numbers
(bindall define-key (N V)
         ((kbd "C-a") 'evil-numbers/inc-at-pt)
         ((kbd "C-s") 'evil-numbers/dec-at-pt))

;;; Man Quit Kills Buffer
(define-key Man-mode-map "q" 'Man-kill)
(define-key woman-mode-map "q" 'Man-kill)

;;; Helm Evil Motions
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-n") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-p") 'helm-delete-minibuffer-contents)

;;; Leader, Ex, And C-w Bindings Everywhere
(define-key global-map (kbd "C-,") evil-leader--default-map)
(define-key global-map (kbd "C-;") 'evil-ex)
(define-key global-map (kbd "C-w") 'evil-window-map)
(bindall define-key (I) ((kbd "C-w") 'evil-window-map))

;;; Helm M-x
(define-key global-map (kbd "M-x") 'helm-M-x)

;;; Newline Auto Comment
(evil-define-key 'insert prog-mode-map (kbd "RET") 'comment-indent-new-line)
(evil-define-key 'insert text-mode-map (kbd "RET") 'comment-indent-new-line)

;;; C-RET Fills Current Line
(defun fill-current-line ()
  (interactive)
  (fill-region (line-beginning-position) (line-end-position)))
(bindall define-key (N R I) ((kbd "<C-return>") 'fill-current-line))

;;; Better Helm Navigation
(defun my-helm-navigate ()
  (interactive)
  (if (file-directory-p (helm-get-selection))
      (helm-execute-persistent-action)
    (helm-maybe-exit-minibuffer)))
(define-key helm-find-files-map (kbd "RET") 'my-helm-navigate)
(define-key helm-find-files-map (kbd "<C-return>") 'my-helm-navigate)

;;; Projectile Switch Project Map
(defmacro bind-switch-key (key act)
  `(define-key my-switch-projectile-map ,key
     (lambda () (interactive)
       (let ((projectile-switch-project-action ,act))
         (helm-projectile-switch-project)))))

(defun my-helm-find-files ()
  (let ((input (expand-file-name default-directory))
        (helm-ff-transformer-show-only-basename t))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))

(defvar my-switch-projectile-map (make-sparse-keymap))
(bind-switch-key "t" 'multi-term)
(bind-switch-key "g" 'magit-status)
(bind-switch-key "e" 'my-helm-find-files)
(bind-switch-key "s" 'helm-projectile-ag)
(bind-switch-key "f" 'helm-projectile-find-file)

;;; Leader Bindings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "t" 'multi-term
  "g" 'magit-status
  "q" 'update-packages
  "w" 'window-mode/body
  "i" 'dtrt-indent-adapt
  "u" 'undo-tree-visualize
  "d" 'evil-show-file-info
  "a" 'helm-resume
  "e" 'helm-find-files
  "b" 'helm-buffers-list
  "s" 'helm-projectile-ag
  "f" 'helm-projectile-find-file
  "p" my-switch-projectile-map
  "c" (lambda () (interactive) (find-file "~/.emacs"))
  "r" (lambda () (interactive)
        (when (buffer-file-name)
          (do-auto-save t t)
          (when (string= ".emacs" (buffer-name))
            (load-file "~/.emacs"))
          (revert-buffer nil t)))
  "v" 'eval-expression
  "x" (lambda () (interactive)
        (if (string= "*scratch*" (buffer-name))
            (let ((current-prefix-arg 0))
              (call-interactively 'eval-print-last-sexp))
          (eval-last-sexp nil))))
