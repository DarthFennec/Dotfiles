;;;; TODO List
;;; do lazy package loading wherever possible
;;; add support for more vim keys in term-mode
;;; fix dd/cc in term-mode
;;; add d/c/s/x yank support in term-mode
;;; helm-repeat should not close existing window sometimes
;;; helm-repeat help should pop up help buffer properly
;;; add special handling to reshow some side windows (eg help buffer)
;;; fix issue with completion buffer messing up side buffer tree
;;; possibly implement window-local buffer lists
;;; helm-projectile-ag modeline should show correct row
;;; evil surround should be repeatable always

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
        dtrt-indent multi-term hydra key-chord package-utils autopair
        python-mode groovy-mode haskell-mode markdown-mode go-mode json-mode
        scala-mode2
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
    (dolist (pack (mapcar 'car package-alist))
      (when (and (package-installed-p pack)
                 (not (package-built-in-p pack)) (not (member pack to)))
        (package-utils-remove-by-name pack))))
  ;; clear the minibuffer
  (message nil))

;;; Allow Evil To Set Initial States
(defadvice evil-set-initial-state
    (before my-evil-really-set-initial-state activate)
  (let ((hk (intern (concat (symbol-name mode) "-hook")))
        (st (intern (concat "evil-" (symbol-name state) "-state"))))
    (add-hook hk st)))

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
(require 'linum)
(require 'hl-line)
(require 'dtrt-indent)
(require 'highlight-indent-guides)
(require 'fill-column-indicator)
(require 'whitespace)
(require 'key-chord)
(require 'woman)
(require 'man)
(require 'term)
(require 'multi-term)
(require 'rcirc)
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
        "^\\*Shell Command Output\\*$"
        "^\\*helm[- ].+\\*$" "^\\*magit\\(-\\w+\\)?: .+$" "^\\*irc\\..+\\*$"))

;;; Side Window Buffers
(setq my-side-window-buffers
      '((" *undo-tree*" :width 60 :position right)
        help-mode Buffer-menu-mode compilation-mode messages-buffer-mode
        "*Warnings*" "*Backtrace*" "*evil-marks*" "*evil-registers*"
        "*helm-mode-completion-at-point*"
        ("^\\*helm[- ].+\\*$" :regexp t)
        (magit-process-mode :position right)
        (magit-diff-mode :noselect t :position right)
        (magit-revision-mode :noselect t :position right)
        magit-status-mode))

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
(add-hook 'haskell-mode-hook 'highlight-quoted--turn-off)
(add-hook 'json-mode-hook 'highlight-numbers--turn-off)
(add-hook 'magit-mode-hook 'turn-off-evil-quickscope-mode)
(add-to-list 'auto-mode-alist '("README\\.md$" . gfm-mode))
(add-to-list 'dtrt-indent-hook-mapping-list
             '(groovy-mode c/c++/java c-basic-offset))

;;; Set Initial Evil States
(add-hook 'Man-mode-hook 'evil-motion-state)
(add-hook 'help-mode-hook 'evil-motion-state)
(evil-set-initial-state 'package-menu-mode 'motion)
(evil-set-initial-state 'messages-buffer-mode 'motion)
(evil-set-initial-state 'compilation-mode 'motion)
(evil-set-initial-state 'rcirc-mode 'insert)

;;; Set Custom Variables
(custom-set-variables
 ;; Better Motion
 '(scroll-conservatively 5)
 '(make-pointer-invisible nil)
 '(magit-display-buffer-noselect t)
 ;; Better Start And Exit
 '(inhibit-startup-screen t)
 '(require-final-newline t)
 ;; Better Editing
 '(evil-want-fine-undo 'no)
 '(sentence-end-double-space nil)
 ;; Better Window Behavior
 '(same-window-regexps '("."))
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
 ;; RCIRC
 '(rcirc-prompt "<%n> ")
 '(rcirc-nick-completion-format "%s, ")
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-program "chromium"))

;;; Keep Temporary Buffers Hidden
(defadvice buffer-name (after boring-buffer-name activate)
  (and ad-return-value
       (boundp 'my-mask-boring-buffer-name-p)
       (not (eq ?\s (aref ad-return-value 0)))
       (some (lambda (x) (string-match-p x ad-return-value)) my-boring-buffers)
       (setq ad-return-value (concat " " ad-return-value))))

(defmacro make-boring-advice (funcname advname)
  `(defadvice ,funcname (around ,advname activate)
     (let ((my-mask-boring-buffer-name-p t)) ad-do-it)))

(make-boring-advice record-window-buffer record-window-no-boring-buffer)
(make-boring-advice switch-to-prev-buffer switch-to-prev-no-boring-buffer)
(make-boring-advice switch-to-next-buffer switch-to-next-no-boring-buffer)
(make-boring-advice get-next-valid-buffer get-next-valid-no-boring-buffer)
(make-boring-advice menu-bar-update-buffers menu-bar-update-no-boring-buffers)
(make-boring-advice msb-invisible-buffer-p msb-invisible-boring-buffer-p)
(make-boring-advice mouse-buffer-menu-alist mouse-no-boring-buffer-menu-alist)

;;; Configure Side Windows
(defun my-display-maybe-close-window (&optional window)
  (interactive)
  (when (null window)
    (setq window (if (window-minibuffer-p) (minibuffer-selected-window)
                   (selected-window))))
  (let ((parent window))
    (while (and parent (not (window-parameter parent 'window-side)))
      (setq parent (window-parent parent)))
    (if (null parent) (keyboard-quit) (delete-side-window window))))

(defun my-display-get-config (buffer)
  (let ((bufname (if (bufferp buffer) (buffer-name buffer) buffer)))
    (-first (lambda (ent)
              (let ((regexp (and (consp ent) (plist-get (cdr ent) :regexp)))
                    (ident (if (consp ent) (car ent) ent)))
                (cond ((stringp ident)
                       (if regexp (string-match-p ident bufname)
                         (string= ident bufname)))
                      ((symbolp ident)
                       (let ((buf (get-buffer buffer)))
                         (eq ident (buffer-local-value 'major-mode buf))))
                      ((functionp ident) (funcall ident buffer)))))
            my-side-window-buffers)))

(defun my-display-condition (buffer action)
  (and (my-display-get-config buffer) t))

(defun my-display-action (buffer alist)
  (let ((ent (my-display-get-config buffer)) props alist window)
    (when ent
      (when (consp ent) (setq props (cdr ent)))
      (setq alist `((side . ,(or (plist-get props :position) 'bottom))
                    (window-width . ,(or (plist-get props :width) 80))
                    (window-height . ,(or (plist-get props :height) 15))
                    ,@(when (plist-get props :noselect) '((noselect . t)))))
      (setq window (display-buffer-in-side-window (get-buffer buffer) alist)))
    window))

(add-to-list 'display-buffer-alist '(my-display-condition my-display-action))

(defadvice magit-popup-mode-display-buffer
    (around my-magit-popup-display-buffer-same-window activate)
  (let ((winconf (current-window-configuration)))
    (switch-to-buffer buffer)
    (funcall mode)
    (setq magit-popup-previous-winconf winconf)))

;;; Track Side Windows
(defun my-display-find-windows-in-stack (node)
  (and from (equal from (car node))
       (or (null fptr)
           (not (time-less-p (cadddr (cadr node)) (cadddr (cadr fptr)))))
       (setq fptr node))
  (and to (equal to (car node))
       (or (null tptr)
           (not (time-less-p (cadddr (cadr node)) (cadddr (cadr tptr)))))
       (setq tptr node))
  (dolist (n (cddr node)) (my-display-find-windows-in-stack n)))

(defun my-display-prune-stack (node)
  (let (rets retval filt live)
    (if (car node) (setq live (buffer-live-p (caadr node))) (setq live t))
    (when (and live (caddr (cadr node))) (setq retval 'no-sel))
    (when (eq sel node) (setq retval 'is-sel))
    (dolist (n (cddr node))
      (push (cons (my-display-prune-stack n) n) rets)
      (when (memq (caar rets) '(has-sel-dead has-sel is-sel))
        (setq retval 'has-sel))
      (when (or (eq 'has-sel-dead (caar rets))
                (and (eq 'is-sel (caar rets)) (null to)))
        (setcar (car rets) nil)
        (setq sel node)))
    (unless (or live (memq retval '(nil no-sel))) (setq retval 'has-sel-dead))
    (unless (memq retval '(nil has-sel-dead))
      (dolist (n rets) (when (car n) (push (cdr n) filt)))
      (setcdr (cdr node) filt))
    retval))

(defun my-display-collect-windows-in-stack (node)
  (unless (null (car node))
    (let ((lkup (assoc (car node) blist)))
      (if (null lkup) (push (cons (car node) (cadr node)) blist)
        (unless (time-less-p (cadddr (cadr node)) (cadddr (cdr lkup)))
          (setcdr lkup (cadr node))))))
  (dolist (n (cddr node)) (my-display-collect-windows-in-stack n)))

(defun my-maybe-update-side-window-state (win)
  (let ((node (frame-parameter nil 'my-side-window-tree))
        (to win) tptr from window)
    (when node
      (my-display-find-windows-in-stack node)
      (when tptr
        (setq window
              (-first
               (lambda (x)
                 (let ((p x))
                   (while (and p (not (window-parameter p 'window-side)))
                     (setq p (window-parent p)))
                   (and p (eq (car win) (window-parameter p 'window-side))
                        (eq (cadr win) (window-parameter p 'window-slot)))))
               (get-buffer-window-list (caadr tptr) 0)))
        (when window (setcar (cdadr tptr) (current-window-configuration)))))))

(defun my-display-switch-side-window (from &optional to buffer)
  (let (stack sel selwin fptr tptr blist (my-recursive-display-side-window t))
    (setq stack (frame-parameter nil 'my-side-window-tree))
    (unless stack
      (setq stack '(nil nil))
      (set-frame-parameter nil 'my-side-window-tree stack))
    (my-display-find-windows-in-stack stack)
    (setq sel (or (unless buffer tptr)
                  (unless (and to (not buffer)) fptr) stack))
    (when (and to buffer (not (eq (car sel) to))
               (not (eq (car buffer) (caadr sel))))
      (setcdr (cdr sel) (cons (list to buffer) (cddr sel)))
      (unless (caddr (cadr (caddr sel))) (setq sel (caddr sel))))
    (my-display-prune-stack stack)
    (when (car sel) (setcar (cdddr (cadr sel)) (current-time)))
    (my-display-collect-windows-in-stack stack)
    (save-excursion
      (dolist (win blist)
        (when (caddr win)
          (set-window-configuration (caddr win))
          (setcar (cddr win) nil)))
      (dolist (win (window-list nil 0))
        (let (side slot match (parent win))
          (while (and parent (not (window-parameter parent 'window-side)))
            (setq parent (window-parent parent)))
          (when parent
            (setq side (window-parameter parent 'window-side))
            (setq slot (window-parameter parent 'window-slot))
            (setq match (assoc (list side slot) blist))
            (if (not match) (delete-side-window win)
              (setcar match (cons win (car match)))))))
      (dolist (win blist)
        (unless (windowp (caar win))
          (let (alist window (buf (cadr win)))
            (setq alist `((side . ,(or (caar win) 'bottom))
                          (slot . ,(or (cadar win) 0))))
            (setq window (display-buffer-in-side-window buf alist))
            (setcar win (cons window (car win)))))
        (when (and (eq (cdar win) (car sel)) (eq (cdr win) (cadr sel)))
          (setq selwin (caar win)))))
    (when (windowp selwin) (select-window selwin))))

(defadvice display-buffer-in-side-window (around my-show-side-window activate)
  (if (boundp 'my-recursive-display-side-window) ad-do-it
    (let* ((parent (if (window-minibuffer-p) (minibuffer-selected-window)
                     (selected-window)))
           (nosel (cdr (assq 'noselect alist)))
           (buf (list buffer nil nosel (current-time)))
           (tside (or (cdr (assq 'side alist)) 'bottom))
           (tslot (or (cdr (assq 'slot alist)) 0))
           (to (list tside tslot))
           from fside fslot)
      (while (and parent (not (window-parameter parent 'window-side)))
        (setq parent (window-parent parent)))
      (when parent
        (setq fside (window-parameter parent 'window-side))
        (setq fslot (window-parameter parent 'window-slot)))
      (setq from (list fside fslot))
      (my-maybe-update-side-window-state to)
      ad-do-it
      (my-display-switch-side-window from to buf))))

(defadvice select-window (around my-select-side-window-maybe-close activate)
  (if (or norecord (boundp 'my-recursive-display-side-window)) ad-do-it
    (let ((from '(nil nil)) (to '(nil nil)))
      (let* (side slot (win (window-normalize-window window)) (parent win))
        (while (and parent (not (window-parameter parent 'window-side)))
          (setq parent (window-parent parent)))
        (when parent
          (setq side (window-parameter parent 'window-side))
          (setq slot (window-parameter parent 'window-slot))
          (setq to (list side slot))))
      (let* ((win (if (window-minibuffer-p) (minibuffer-selected-window)
                    (selected-window))) side slot (parent win))
        (while (and parent (not (window-parameter parent 'window-side)))
          (setq parent (window-parent parent)))
        (when parent
          (setq side (window-parameter parent 'window-side))
          (setq slot (window-parameter parent 'window-slot))
          (setq from (list side slot))))
      (my-display-switch-side-window from to)
      (when (equal to '(nil nil)) ad-do-it))))

(defadvice delete-window (around my-kill-side-window activate)
  (if (boundp 'my-recursive-display-side-window) ad-do-it
    (let* (side slot (win (window-normalize-window window)) (parent win))
      (while (and parent (not (window-parameter parent 'window-side)))
        (setq parent (window-parent parent)))
      (if (null parent) ad-do-it
        (setq side (window-parameter parent 'window-side))
        (setq slot (window-parameter parent 'window-slot))
        (let ((ignore-window-parameters t)) ad-do-it)
        (my-display-switch-side-window (list side slot))))))

(defadvice quit-restore-window (around my-quit-side-window activate)
  (if (boundp 'my-recursive-display-side-window) ad-do-it
    (let* (side slot (win (window-normalize-window window)) (parent win))
      (while (and parent (not (window-parameter parent 'window-side)))
        (setq parent (window-parent parent)))
      (if (null parent) ad-do-it
        (setq side (window-parameter parent 'window-side))
        (setq slot (window-parameter parent 'window-slot))
        (let ((my-recursive-display-side-window t)
              (ignore-window-parameters t))
          (delete-window window))
        (my-display-switch-side-window (list side slot))))))

;;; Do Not Kill Scratch Buffer
(defun my-save-scratch-buffer ()
  (not (string= (buffer-name (current-buffer)) "*scratch*")))
(add-hook 'kill-buffer-query-functions 'my-save-scratch-buffer)

;;; Hook Editing Via Term-Mode
(defvar-local my-term-prev-match nil)
(defadvice term-emulate-terminal
    (around handle-custom-ansi-terminal-messages activate)
  ;; (with-current-buffer "*scratch*"
  ;;   (save-excursion (goto-char (point-max)) (insert str)))
  (when my-term-prev-match
    (setq str (concat my-term-prev-match str))
    (setq my-term-prev-match nil))
  (when (string-match "\eAnSiT.[ \t]*[^\r\n]*\r?\\'" str)
    (setq my-term-prev-match (substring str (match-beginning 0)))
    (setq str (replace-match "" t t str)))
  (if (string-match "\eAnSiT\\(.\\)[ \t]*\\([^\r\n]*\\)\r?\n" str)
      (let* ((command-code (aref str (match-beginning 1)))
             (argument (substring str (match-beginning 2) (match-end 2))))
        ad-do-it
        (cond ((= command-code ?e)
               (display-buffer-in-side-window
                (find-file-noselect argument)
                '((side . bottom) (window-width . 80) (window-height . 15))))
              ((= command-code ?x) (find-file argument))
              ((= command-code ?m) (man argument))))
    ad-do-it))

;;; Auto Open As Root
(defadvice find-file-noselect (before find-file-sudo activate)
  (unless (or (not filename) (file-writable-p filename))
    (setq filename (concat "/sudo::" (expand-file-name filename)))))

;;; Remove . And .. From Helm Find File
(defadvice helm-ff-directory-files
    (after my-helm-ff-directory-files-hide-dots activate)
  (setq ad-return-value
        (or (cddr ad-return-value)
            (list (car ad-return-value)))))

;;; Control Which Helm Buffers Can Be Resumed
(defadvice helm-initialize (before helm-control-resume activate)
  (if (or (string-match-p "^\\*helm-mode-.+\\*$" (helm-buffer-get))
          (member (helm-buffer-get) my-helm-resumable-buffers))
      (when (eq any-resume 'noresume) (setq any-resume nil))
    (setq any-resume 'noresume)))

;;; Newline Auto Comment
(defadvice newline (around my-comment-newline activate)
  (if (boundp 'my-recursive-newline) ad-do-it
    (let ((lst (parent-mode-list major-mode)))
      (if (or (memq 'prog-mode lst) (memq 'text-mode lst))
          (let ((my-recursive-newline t)
                (fill-prefix (fill-context-prefix (point) (point))))
            (comment-indent-new-line)
            (indent-according-to-mode))
        ad-do-it))))

;;; Open Line Auto Comment
(defvar-local my-comment-starter nil)

(defadvice indent-according-to-mode (before my-insert-comment-starter activate)
  (when my-comment-starter (insert my-comment-starter)))

(defadvice evil-cleanup-insert-state (after my-clear-comment-starter activate)
  (setq my-comment-starter nil))

(defmacro my-open-comment-build (oper name)
  `(defadvice ,oper (before ,name activate)
     (setq my-comment-starter (fill-context-prefix (point) (point)))))
(my-open-comment-build evil-open-below my-evil-open-comment-below)
(my-open-comment-build evil-open-above my-evil-open-comment-above)

;;; Join Line Remove Prefix
(defadvice join-line (around join-line-remove-prefix activate)
  (let ((fill-prefix (fill-context-prefix (point) (point)))) ad-do-it))

;;; Switch Word And Symbol Handling
(defadvice forward-thing (before my-forward-thing activate)
  (when (eq thing 'evil-word) (setq thing 'evil-symbol))
  (when (eq thing 'evil-WORD) (setq thing 'evil-word)))

;;; Subword Mode In Evil
(add-hook
 'after-init-hook
 (lambda ()
   (define-category ?U "Uppercase")
   (define-category ?u "Lowercase")
   (modify-category-entry (cons ?A ?Z) ?U)
   (modify-category-entry (cons ?a ?z) ?u)
   (make-variable-buffer-local 'evil-cjk-word-separating-categories)))
(add-hook
 'subword-mode-hook
 (lambda ()
   (if subword-mode
       (push '(?u . ?U) evil-cjk-word-separating-categories)
     (setq evil-cjk-word-separating-categories
           (default-value 'evil-cjk-word-separating-categories)))))

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
(evil-commentary-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode 1)
(global-evil-quickscope-mode 1)
(global-evil-matchit-mode 1)
(global-subword-mode 1)
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
(defface custom-highlights-todo-face
  '((t (:weight bold :foreground "white" :background "black")))
  "Face to highlight FIXMEs and TODOs.")

(defface custom-highlights-non-ascii-face
  '((t (:background "#A20C41")))
  "Face to highlight non-ascii characters.")

(defun custom-highlights ()
  (font-lock-add-keywords
   nil
   '(("\\bFIXME\\b\\|\\bTODO\\b" 0 'custom-highlights-todo-face t)
     ("[[:nonascii:]]" 0 'custom-highlights-non-ascii-face t))))
(add-hook 'prog-mode-hook 'custom-highlights)
(add-hook 'text-mode-hook 'custom-highlights)

;;; Load Theme
(load-theme 'monokai t)

;;; Face Customizations
(set-face-attribute 'linum nil :inverse-video nil :weight 'semi-bold)
(set-face-attribute 'helm-selection nil
                    :background "#39382E" :underline 'unspecified)
(set-face-attribute 'hl-line nil :background "#39382E")
(dotimes (i 6)
  (set-face-attribute
   (intern (concat "markdown-header-face-" (number-to-string (1+ i))))
   nil :height 'unspecified))

;;;; Modeline

;;; Track The Selected Window
(defvar my-mode-line-sel-win nil)
(defun set-my-mode-line-sel-win (windows)
  (unless (minibuffer-window-active-p (frame-selected-window))
    (setq my-mode-line-sel-win (selected-window))))
(add-function :before pre-redisplay-function 'set-my-mode-line-sel-win)

;;; Draw The Modeline
(defconst my-home-dir (expand-file-name "~/"))
(defvar-local my-vc-data '(nil . nil))
(defvar-local my-indent-offset '(nil . nil))

(defun get-modeline-left ()
  (unless (or (string= default-directory (car my-vc-data))
              (and (projectile-project-p)
                   (string= (projectile-project-root) (car my-vc-data))))
    (let ((path default-directory) dir vctype vctype1)
      (if (not (projectile-project-p))
          (setq dir (if (string= default-directory my-home-dir) "~"
                      (file-name-base (directory-file-name default-directory))))
        (setq path (projectile-project-root)
              dir (projectile-project-name)
              vctype1 (projectile-project-vcs))
        (unless (eq 'none vctype1)
          (setq vctype (concat ":" (symbol-name vctype1)))))
      (setq my-vc-data (cons path (concat "[" dir vctype "]")))))
  (unless (eq major-mode (car my-indent-offset))
    (let ((offset (caddr (dtrt-indent--search-hook-mapping major-mode))))
      (setq my-indent-offset (cons major-mode offset))))
  (let* ((is-recording
          (and defining-kbd-macro (eq my-mode-line-sel-win (selected-window))))
         (state (cond ((evil-normal-state-p)   " NORM")
                      ((evil-visual-state-p)   " VISL")
                      ((evil-insert-state-p)   " INSR")
                      ((evil-replace-state-p)  " RPLC")
                      ((evil-motion-state-p)   " MOTN")
                      ((evil-operator-state-p) " OPER")
                      (t                       " EMCS")))
         (strec (concat state (if is-recording "\x25CF " "  ")))
         (buf (concat (buffer-name) (if (buffer-modified-p) "* " "  ")))
         (ro (if buffer-read-only "[RO]" nil))
         (su (if (and buffer-file-name
                      (file-remote-p buffer-file-name)) "[SU]" nil))
         (rosu (when (or ro su) (concat ro su " ")))
         (mmode (concat (symbol-name major-mode) " "))
         (offs (number-to-string (symbol-value (cdr my-indent-offset))))
         (tabs (if indent-tabs-mode "T" "S"))
         (indent (concat "[" offs tabs "]"))
         (coding (symbol-name (coding-system-type buffer-file-coding-system)))
         (eol (case (coding-system-eol-type buffer-file-coding-system)
                (0 "LF") (1 "CRLF") (2 "CR")))
         (encode (concat "[" coding ":" eol "]"))
         (proj (when (or buffer-file-name (eq major-mode 'term-mode))
                 (cdr my-vc-data)))
         (emph '(face mode-line-emphasis)))
    (add-text-properties 0 (- (length buf) 2) '(face mode-line-buffer-id) buf)
    (add-text-properties (- (length buf) 2) (length buf) emph buf)
    (add-text-properties 0 (length strec) emph strec)
    (add-text-properties 0 (length mmode) emph mmode)
    (-filter 'identity (list strec buf rosu mmode indent encode proj))))

(defun get-modeline-right ()
  (let ((perc (format-mode-line "%p"))
        (size (if (string-match-p "^\\*helm[- ].+\\*$" (buffer-name))
                  (concat
                   " (" (int-to-string (helm-candidate-number-at-point))
                   "/" (int-to-string (helm-get-candidate-number t)) ") ")
                (format-mode-line " (%l,%c) ")))
        (screen (when (and (window-at-side-p) (window-at-side-p nil 'right))
                  (concat " " elscreen-mode-line-string))))
    (when (string= perc "Bottom") (setq perc "Bot"))
    (when (string-match-p "[0-9]+%$" perc) (setq perc (concat perc "%")))
    (concat size perc screen " ")))

(defun draw-modeline (lefts right)
  (let* ((lefts (reverse lefts))
         (max (- (window-total-width) (length (format-mode-line right))))
         (sizes (mapcar 'length lefts))
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

;;; C-g Closes All Side Windows
(define-key global-map (kbd "C-g") 'my-display-maybe-close-window)

;;; Bind Semicolon To Evil Ex
(define-key evil-motion-state-map ";" 'evil-ex)

;;; Bind Space To Repeat Find
(define-key evil-motion-state-map (kbd "SPC") 'evil-repeat-find-char)
(define-key evil-motion-state-map (kbd "S-SPC") 'evil-repeat-find-char-reverse)

;;; Window Jumps Column Zero
(evil-define-motion evil-window-top-and-zero (count)
  "Move the cursor to line COUNT from the top of the window on column zero."
  :jump t
  :type line
  (move-to-window-line
   (max (or count 0) (if (= (point-min) (window-start)) 0 scroll-margin))))
(define-key evil-motion-state-map "H" 'evil-window-top-and-zero)

(evil-define-motion evil-window-middle-and-zero ()
  "Move the cursor to the middle line in the window on column zero."
  :jump t
  :type line
  (move-to-window-line
   (/ (1+ (save-excursion (move-to-window-line -1))) 2)))
(define-key evil-motion-state-map "M" 'evil-window-middle-and-zero)

(evil-define-motion evil-window-bottom-and-zero (count)
  "Move the cursor to line COUNT from the bottom of the window on column zero."
  :jump t
  :type line
  (move-to-window-line (- (max (or count 1) (1+ scroll-margin)))))
(define-key evil-motion-state-map "L" 'evil-window-bottom-and-zero)

;;; Evil Numbers
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)

;;; Evil Open With Extra Line
(defun evil-open-below-with-extra-line (count)
  (interactive "p")
  (evil-insert-newline-below)
  (previous-line)
  (evil-open-below count))
(define-key evil-normal-state-map "go" 'evil-open-below-with-extra-line)

(defun evil-open-above-with-extra-line (count)
  (interactive "p")
  (evil-insert-newline-above)
  (next-line)
  (evil-open-above count))
(define-key evil-normal-state-map "gO" 'evil-open-above-with-extra-line)

;;; Evil Global Textobject
(evil-define-text-object evil-global (&optional count beg end type)
  "A text object representing the entire buffer"
  (list (point-min) (point-max)))
(define-key evil-outer-text-objects-map "g" 'evil-global)
(define-key evil-inner-text-objects-map "g" 'evil-global)

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
(defun my-term-motion (motn &optional pt mk ct st yt ps)
  (delete-trailing-whitespace)
  (let* ((term-proc (get-buffer-process (current-buffer)))
         (term-point (marker-position (process-mark term-proc)))
         (ptn (if (eq pt t) (point) pt)) (mkn (if (eq mk t) (mark) mk))
         (pto (int-to-string (- ptn term-point)))
         (mko (when mk (concat "," (int-to-string (- mkn term-point)))))
         (cnt (when ct (concat "#" (int-to-string ct))))
         (psd (when (and ps (> (length ps) 0)) ps))
         (psl (when psd (concat ";" (int-to-string (length psd)))))
         (stp (or st "c")) (ytp (or yt "c"))
         (motn-cmd (concat "\e[::" pto mko cnt psl ":" motn stp ytp psd)))
    (term-send-raw-string motn-cmd)))

(defmacro my-term-change-delete-build (oper name motn)
  `(defadvice ,oper (around ,name activate)
     (if (not (eq major-mode 'term-mode)) ad-do-it
       (my-term-motion ,motn beg end nil (case type (line "l") (block "b")))
       ,@(when (string= motn "c") '((evil-insert-state))))))
(my-term-change-delete-build evil-change my-term-change "c")
(my-term-change-delete-build evil-delete my-term-delete "d")

(defmacro my-term-paste-build (oper name motn)
  `(defadvice ,oper (around ,name activate)
     (if (not (eq major-mode 'term-mode)) ad-do-it
       (let* ((txt (if register (evil-get-register register) (current-kill 0))))
         (when txt
           (when (and (not yank-handler) (stringp txt))
             (setq yank-handler
                   (car-safe (get-text-property 0 'yank-handler txt))))
           (when (vectorp txt) (setq txt (evil-vector-to-string txt)))
           (setq yank-handler
                 (cond ((eq yank-handler 'evil-yank-line-handler) "l")
                       ((eq yank-handler 'evil-yank-block-handler) "b")))
           (my-term-motion ,motn t nil count nil yank-handler txt))))))
(my-term-paste-build evil-paste-before my-term-paste-before "P")
(my-term-paste-build evil-paste-after my-term-paste-after "p")

(defadvice mouse-yank-primary (around my-term-mouse-paste activate)
  (if (not (eq major-mode 'term-mode)) ad-do-it
    (run-hooks 'mouse-leave-buffer-hook)
    (when select-active-regions (let (select-active-regions) (deactivate-mark)))
    (or mouse-yank-at-point (mouse-set-point click))
    (let ((text
           (if (fboundp 'x-get-selection-value)
               (if (eq (framep (selected-frame)) 'w32)
                   (or (x-get-selection 'PRIMARY) (x-get-selection-value))
                 (or (x-get-selection-value) (x-get-selection 'PRIMARY)))
             (x-get-selection 'PRIMARY))))
      (if text (my-term-motion "p" t nil nil nil nil text)
        (error "No selection is available")))))

(defmacro my-term-motion-do (ins motn &optional pt mk ct st yt ps)
  `(lambda ()
     (interactive)
     (my-term-motion ,motn ,pt ,mk ,ct ,st ,yt ,ps)
     ,@(when ins '((evil-insert-state)))))
(evil-define-key 'normal term-raw-map "A" (my-term-motion-do t "A" t))
(evil-define-key 'normal term-raw-map "a" (my-term-motion-do t "a" t))
(evil-define-key 'normal term-raw-map "I" (my-term-motion-do t "I" t))
(evil-define-key 'normal term-raw-map "i" (my-term-motion-do t "i" t))

;;; C-RET Fills Current Line
(defun fill-current-line ()
  (interactive)
  (fill-region (point-at-bol) (point-at-eol)))
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
(define-key helm-find-files-map (kbd "<S-return>") 'helm-maybe-exit-minibuffer)

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
        (if (memq 'lisp-interaction-mode (parent-mode-list major-mode))
            (let ((current-prefix-arg 0))
              (call-interactively 'eval-print-last-sexp))
          (eval-last-sexp nil)))
  "z" (lambda () (interactive)
        (setq debug-on-error (not debug-on-error))
        (message "Debug %s" (if debug-on-error "on" "off"))))
