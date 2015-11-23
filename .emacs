;;;; TODO List

;;; do lazy package loading wherever possible

;;; current line highlight should be darker

;;; evil surround should be repeatable always

;;; popwin problems:
;;; - cannot seem to control what happens to popwin window when switching from
;;;   it; handler does not run
;;; - cannot open multiple or recursive popwins without some of them becoming
;;;   unbound, it seems that not all of them are tracked properly

;;; hitting o or O in a comment should continue the comment block
;;; - will probably need to reimplement comment-indent-newline to be general

;;; find a way to dwim words vs symbols
;;; - everything that previously worked on words should now work on symbols
;;; - everything that previously worked on WORDS should now work on words
;;; - subword-mode should be enabled

;;;; Packages

;;; Archive List
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Package List
(setq package-list
      '(monokai-theme
        helm helm-ag helm-projectile
        magit evil-magit gitattributes-mode gitconfig-mode gitignore-mode
        evil evil-tabs evil-leader evil-numbers evil-commentary evil-indent-plus
        evil-surround evil-quickscope evil-exchange evil-visualstar evil-matchit
        dtrt-indent multi-term hydra key-chord package-utils popwin autopair
        python-mode groovy-mode haskell-mode markdown-mode go-mode json-mode
        highlight-indent-guides highlight-quoted highlight-numbers paren-face
        fill-column-indicator))

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
(require 'magit)
(require 'evil-leader)
(require 'evil)
(require 'evil-numbers)
(require 'evil-quickscope)
(require 'evil-surround)
(require 'evil-exchange)
(require 'evil-indent-plus)
(require 'evil-visualstar)
(require 'evil-commentary)
(require 'evil-matchit)
(require 'evil-magit)
(require 'autopair)
(require 'popwin)
(require 'linum)
(require 'dtrt-indent)
(require 'highlight-indent-guides)
(require 'fill-column-indicator)
(require 'whitespace)
(require 'key-chord)
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

;;; Buffers To Hide
(setq my-boring-buffers
      '("^ " "^\\*Help\\*$" "^\\*Messages\\*$" "^\\*Buffer List\\*$"
        "^\\*Backtrace\\*$" "^\\*Warnings\\*$" "^\\*WoMan-Log\\*$"
        "^\\*Compile-Log\\*$" "^\\*tramp/.+\\*$" "^\\*Faces\\*$"
        "^\\*evil-marks\\*$" "^\\*evil-registers\\*$" "\\*Packages\\*"
        "^\\*helm[- ].+\\*$" "^\\*magit\\(-\\w+\\)?: .+$"))

;;; Helm Resumable Buffers
(setq my-helm-resumable-buffers
      '("*helm*" "*helm select xfont*" "*helm projectile*" "*Helm Find Files*"
        "*helm-ag*" "*helm M-x*" "*helm apt*" "*helm top*" "*helm eval*"
        "*helm calcul*" "*helm colors*" "*helm process*" "*helm buffers*"))

;;; Autosaves And Backups
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))

;;; Language Specific Changes
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'json-mode-hook 'highlight-numbers--turn-off)
(add-to-list 'auto-mode-alist '("README\\.md$" . gfm-mode))
(add-to-list 'dtrt-indent-hook-mapping-list
             '(groovy-mode c/c++/java c-basic-offset))

;;; Mode Specific Changes
(add-hook 'Man-mode-hook 'evil-motion-state)
(add-hook 'help-mode-hook 'evil-motion-state)
(add-hook 'magit-mode-hook 'evil-motion-state)
(add-hook 'package-menu-mode-hook 'evil-motion-state)
(add-hook 'messages-buffer-mode-hook 'evil-motion-state)
(evil-set-initial-state #'package-menu-mode 'motion)
(evil-set-initial-state #'messages-buffer-mode 'motion)

;;; Set Custom Variables
(custom-set-variables
 ;; Better Motion
 '(scroll-conservatively 5)
 '(make-pointer-invisible nil)
 '(mouse-avoidance-mode 'banish)
 ;; Better Start And Exit
 '(inhibit-startup-screen t)
 '(require-final-newline t)
 ;; Better Editing
 '(evil-want-fine-undo 'no)
 '(sentence-end-double-space nil)
 ;; Better VC Behavior
 '(vc-follow-symlinks t)
 '(magit-push-always-verify nil)
 ;; Better Term Behavior
 '(multi-term-program "/bin/zsh")
 '(multi-term-switch-after-close nil)
 '(Man-notify-method 'pushy)
 ;; Better Display
 '(show-paren-mode t)
 '(column-number-mode t)
 '(elscreen-display-tab nil)
 '(whitespace-style '(face lines-tail trailing tab-mark))
 '(autopair-blink nil)
 ;; Indentation
 '(indent-tabs-mode nil)
 '(dtrt-indent-max-merge-deviation 0.01)
 '(dtrt-indent-mode t)
 ;; Fill
 '(fill-column 80)
 '(whitespace-line-column nil)
 '(fci-handle-truncate-lines nil)
 '(truncate-partial-width-windows 90)
 ;; Autosaves And Backups
 '(auto-save-list-file-prefix autosave-dir)
 '(auto-save-file-name-transforms `((".*" ,autosave-dir t)))
 '(backup-directory-alist `((".*" . ,backup-dir)))
 ;; Helm
 '(projectile-completion-system 'helm)
 '(helm-split-window-preferred-function 'ignore)
 '(helm-boring-buffer-regexp-list my-boring-buffers)
 ;; Popwin
 '(same-window-buffer-names '("*Help*"))
 '(popwin:special-display-config
   '(help-mode debugger-mode Buffer-menu-mode compilation-mode
     messages-buffer-mode "*Warnings*" "*evil-marks*" "*evil-registers*"
     (" *undo-tree*" :width 60 :position right)
     ("*helm-mode-completion-at-point*" :noselect t)
     ("^\\*helm[- ].+\\*$" :regexp t)
     (magit-diff-mode :noselect t :width 80 :position right)
     (magit-revision-mode :noselect t :width 80 :position right)
     magit-status-mode)))

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
     (unwind-protect ad-do-it
       (ad-disable-advice 'buffer-name 'after 'boring-buffer-name)
       (ad-activate 'buffer-name))))

(make-boring-advice record-window-buffer record-window-no-boring-buffer)
(make-boring-advice switch-to-prev-buffer switch-to-prev-no-boring-buffer)
(make-boring-advice switch-to-next-buffer switch-to-next-no-boring-buffer)
(make-boring-advice get-next-valid-buffer get-next-valid-no-boring-buffer)
(make-boring-advice menu-bar-update-buffers menu-bar-update-no-boring-buffers)
(make-boring-advice msb-invisible-buffer-p msb-invisible-boring-buffer-p)
(make-boring-advice mouse-buffer-menu-alist mouse-no-boring-buffer-menu-alist)

;;; Do Not Kill Scratch
(defun my-save-scratch-buffer ()
  (not (string= (buffer-name (current-buffer)) "*scratch*")))
(add-hook 'kill-buffer-query-functions 'my-save-scratch-buffer)

;;; Hook Editing Via Term-Mode
(defvar-local my-term-prev-match nil)
(defadvice term-handle-ansi-terminal-messages
    (before handle-custom-ansi-terminal-messages activate)
  (when my-term-prev-match
    (setq message (concat my-term-prev-match message))
    (setq my-term-prev-match nil))
  (when (string-match "\eAnSiT.[ \t]*[^\r\n]*\r?\\'" message)
    (setq my-term-prev-match (substring message (match-beginning 0)))
    (setq message (replace-match "" t t message)))
  (when (string-match "\eAnSiT\\(.\\)[ \t]*\\([^\r\n]*\\)\r?\n" message)
    (let* ((command-code (aref message (match-beginning 1)))
           (argument (substring message (match-beginning 2) (match-end 2))))
      (save-excursion
        (cond ((= command-code ?e) (find-file-other-window argument))
              ((= command-code ?x) (find-file argument))
              ((= command-code ?m) (man argument)))))))

;;; Auto Open As Root
(defadvice find-file-noselect (before find-file-sudo activate)
  (unless (or (not filename) (file-writable-p filename))
    (setq filename (concat "/sudo::" (expand-file-name filename)))))

;;; Remove . And .. From Helm Find File
(defadvice helm-ff-filter-candidate-one-by-one
    (around helm-hide-dot-files (file) activate)
  (unless (string-match-p "/\\.\\.?$" file) ad-do-it))

;;; Control Which Helm Buffers Can Be Resumed
(defadvice helm-initialize (before helm-control-resume activate)
  (if (or (string-match-p "^\\*helm-mode-.+\\*$" (helm-buffer-get))
          (member (helm-buffer-get) my-helm-resumable-buffers))
      (when (eq any-resume 'noresume) (setq any-resume nil))
    (setq any-resume 'noresume)))

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
(key-chord-mode 1)
(global-evil-tabs-mode 1)

;;; Other Helpful Modes
(popwin-mode 1)
(evil-commentary-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode 1)
(global-evil-quickscope-mode 1)
(global-evil-matchit-mode 1)
(evil-exchange-install)
(evil-indent-plus-default-bindings)

;;; Highlighting And Misc
(global-paren-face-mode 1)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'text-mode-hook 'fci-mode)
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
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'text-mode-hook 'highlight-indent-guides-mode)

;;; Custom Highlights
(defun custom-highlights ()
  (font-lock-add-keywords
   nil
   '(("\\bFIXME\\b\\|\\bTODO\\b" 0 'match t)
     ("[[:nonascii:]]" 0 'whitespace-space-before-tab t))))
(add-hook 'prog-mode-hook 'custom-highlights)
(add-hook 'text-mode-hook 'custom-highlights)

;;; Disable Quickscope For Magit
(add-hook 'magit-mode-hook 'turn-off-evil-quickscope-mode)

;;; Load Theme
(load-theme 'monokai t)

;;; Face Customizations
(set-face-attribute 'linum nil :inverse-video nil :weight 'semi-bold)
(dotimes (i 6)
  (set-face-attribute
   (intern (concat "markdown-header-face-" (number-to-string (1+ i))))
   nil :height 'unspecified))

;;;; Modeline

;;; Track The Selected Window
(defvar my-mode-line-selected-window nil)
(defun set-my-mode-line-selected-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq my-mode-line-selected-window (selected-window))))
(add-function :before pre-redisplay-function #'set-my-mode-line-selected-window)

;;; Draw The Modeline
(defvar-local my-indent-offset '(nil . nil))

(defun get-modeline-left ()
  (unless (eq major-mode (car my-indent-offset))
    (let ((offset (caddr (dtrt-indent--search-hook-mapping major-mode))))
      (setq my-indent-offset (cons major-mode offset))))
  (let* ((is-record-state (and defining-kbd-macro
                               (eq my-mode-line-selected-window
                                   (selected-window))))
         (state (cond (is-record-state         " RECORD   ")
                      ((evil-normal-state-p)   " NORMAL   ")
                      ((evil-visual-state-p)   " VISUAL   ")
                      ((evil-insert-state-p)   " INSERT   ")
                      ((evil-replace-state-p)  " REPLACE  ")
                      ((evil-motion-state-p)   " MOTION   ")
                      ((evil-operator-state-p) " OPERATOR ")
                      (t                       " EMACS    ")))
         (buf (concat (buffer-name) (if (buffer-modified-p) "*" " ") " "))
         (ro (if buffer-read-only "[RO]" nil))
         (su (if (and buffer-file-name
                      (file-remote-p buffer-file-name)) "[SU]" nil))
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
         (proj (concat "[" dir vc-mode "]"))
         (emph '(face mode-line-emphasis)))
    (add-text-properties 0 (- (length buf) 2) '(face mode-line-buffer-id) buf)
    (add-text-properties (- (length buf) 2) (length buf) emph buf)
    (add-text-properties 0 (length state) emph state)
    (add-text-properties 0 (length mmode) emph mmode)
    (set-text-properties 0 (length proj) nil proj)
    (list state buf rosu mmode indent encode proj)))

(defun get-modeline-right ()
  (let ((perc (format-mode-line "%p"))
        (size (if (string-match-p "^\\*helm[- ].+\\*$" (buffer-name))
                  (concat
                   " (" (int-to-string (helm-candidate-number-at-point))
                   "/" (int-to-string (helm-get-candidate-number t)) ") ")
                (format-mode-line " (%l,%c) "))))
    (when (string= perc "Bottom") (setq perc "Bot"))
    (when (string-match-p "[0-9]+%$" perc) (setq perc (concat perc "%")))
    (concat size perc " " elscreen-mode-line-string " ")))

(defun draw-modeline (lefts right)
  (let* ((lefts (reverse lefts))
         (max (- (window-total-width) (length (format-mode-line right))))
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

;;; Helm Should Use New Modeline
(setq helm-mode-line-string nil)
(defadvice helm-display-mode-line (before helm-display-my-modeline activate)
  (when (listp source) (assq-delete-all 'mode-line source)))

;;; Message Buffer Should Use New Modeline
(kill-buffer "*Messages*")

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

;;; Escape Sequences
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "C-g") 'evil-normal-state)

;;; Bind Semicolon To Evil Ex
(define-key evil-motion-state-map ";" 'evil-ex)

;;; Bind Space To Repeat Find
(define-key evil-motion-state-map (kbd "SPC") 'evil-repeat-find-char)
(define-key evil-motion-state-map (kbd "S-SPC") 'evil-repeat-find-char-reverse)

;;; Window Jumps Column Zero
(defmacro jmp-zero (jmp)
  `(lambda () (interactive) (,jmp) (evil-beginning-of-line)))
(define-key evil-motion-state-map "H" (jmp-zero evil-window-top))
(define-key evil-motion-state-map "M" (jmp-zero evil-window-middle))
(define-key evil-motion-state-map "L" (jmp-zero evil-window-bottom))

;;; Evil Numbers
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)

;;; Evil Open Without Moving Point
(defun evil-open-below-save-excursion (count)
  (interactive "p")
  (save-excursion
    (evil-open-below count)
    (evil-normal-state)))
(define-key evil-normal-state-map "go" 'evil-open-below-save-excursion)

(defun evil-open-above-save-excursion (count)
  (interactive "p")
  (let ((col (current-column)))
    (evil-open-above count)
    (evil-normal-state)
    (next-line)
    (move-to-column col)))
(define-key evil-normal-state-map "gO" 'evil-open-above-save-excursion)

;;; Yank Rest Of Line
(evil-define-operator evil-yank-line-rest (beg end type register)
  "Saves rest of line into the kill-ring"
  :motion nil
  :repeat nil
  :move-point nil
  (interactive "<R><x>")
  (unless (evil-visual-state-p)
    (setq beg (point))
    (setq end (point-at-eol)))
  (evil-yank beg end type register))
(define-key evil-motion-state-map "Y" 'evil-yank-line-rest)
(define-key evil-normal-state-map "Y" 'evil-yank-line-rest)

;;; Center Motion
(evil-define-operator evil-scroll-motion-to-center (beg end type)
  "Scrolls the motion to the center of the window."
  :type line
  :repeat nil
  :move-point nil
  :keep-visual t
  (let* ((top (line-number-at-pos beg))
         (bot (line-number-at-pos end))
         (mid (/ (+ top bot) 2)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- mid))
      (recenter))))
(define-key evil-motion-state-map "zq" 'evil-scroll-motion-to-center)

;;; Man Quit Kills Buffer
(defun Info-kill ()
  (interactive)
  (if Info-standalone
      (save-buffers-kill-emacs)
    (quit-window t)))
(define-key Info-mode-map "q" 'Info-kill)
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
(define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)

;;; Helm M-x
(define-key global-map (kbd "M-x") 'helm-M-x)

;;; Term Motions
(defun term-try-jump-to-point ()
  (let* ((term-proc (get-buffer-process (current-buffer)))
         (term-point (marker-position (process-mark term-proc)))
         (jump-offs (- (point) term-point))
         (jump-cmd (concat "\e[j:" (int-to-string jump-offs) "x")))
    (term-send-raw-string jump-cmd)))

(defmacro my-term-evil-do (it)
  `(lambda () (interactive) (term-try-jump-to-point) (evil-insert-state) ,it))

(evil-define-key 'normal term-raw-map "I" (my-term-evil-do (term-send-home)))
(evil-define-key 'normal term-raw-map "i" (my-term-evil-do (ignore)))
(evil-define-key 'normal term-raw-map "A" (my-term-evil-do (term-send-end)))
(evil-define-key 'normal term-raw-map "a" (my-term-evil-do (term-send-right)))
(evil-define-key 'normal term-raw-map "P" (my-term-evil-do (term-paste)))
(evil-define-key 'normal term-raw-map "p"
  (my-term-evil-do (progn (term-send-right) (term-paste))))

;;; Newline Auto Comment
(evil-define-key 'insert prog-mode-map (kbd "RET") 'comment-indent-new-line)
(evil-define-key 'insert text-mode-map (kbd "RET") 'comment-indent-new-line)

;;; C-RET Fills Current Line
(defun fill-current-line ()
  (interactive)
  (fill-region (line-beginning-position) (line-end-position)))
(define-key evil-insert-state-map (kbd "<C-return>") 'fill-current-line)
(define-key evil-replace-state-map (kbd "<C-return>") 'fill-current-line)

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
