;;;; TODO List
;;; get term-mode working at all again
;;; do lazy package loading wherever possible
;;; replace the old term-mode vim integration entirely
;;; add special handling to reshow some side windows (eg help buffer)
;;; fix issue with completion buffer messing up side buffer tree
;;; fix issue with side windows sticking around when they shouldn't in magit
;;; keep term-mode from stealing focus from side windows
;;; possibly implement window-local buffer lists

;;;; Packages

;;; Archive List
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Package List
(setq package-list
      '(monokai-theme
        package-lint flycheck-package
        ivy counsel counsel-projectile swiper
        magit evil-magit gitattributes-mode gitconfig-mode gitignore-mode
        magit-gerrit maven-test-mode
        evil evil-leader evil-numbers evil-commentary evil-indent-plus
        evil-surround evil-quickscope evil-exchange evil-visualstar evil-matchit
        dtrt-indent multi-term key-chord package-utils
        python-mode groovy-mode haskell-mode markdown-mode go-mode json-mode
        rust-mode enh-ruby-mode scala-mode shakespeare-mode yaml-mode
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
  (let ((from package-list) to curr)
    (while from
      (setq curr (pop from))
      (setq to (cons curr to))
      (dolist (dep (package-desc-reqs (cadr (assoc curr package-alist))))
        (unless (or (package-built-in-p (car dep))
                    (member (car dep) from) (member (car dep) to))
          (setq from (cons (car dep) from)))))
    (dolist (pack (mapcar 'car package-alist))
      (when (and (package-installed-p pack)
                 (not (package-built-in-p pack)) (not (member pack to)))
        (package-utils-remove-by-name pack))))
  ;; report success
  (message "package update complete"))

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
(require 'cl)
(require 'evil)
(require 'evil-magit)
(require 'magit-gerrit)
(require 'linum)
(require 'hl-line)
(require 'dtrt-indent)
(require 'woman)
(require 'man)
(require 'parent-mode)
(require 'groovy-mode)
(require 'cc-vars)
(require 'cc-fonts)

;;;; Behavior

;;; Disable Customization File
(setq custom-file "/dev/null")
(setq disabled-command-function nil)

;;; Custom Source Path
(setq find-function-C-source-directory "~/Documents/Reference/emacs/src/")

;;; Buffers To Hide
(setq my-boring-buffers
      '("^ " "^\\*Help\\*$" "^\\*Messages\\*$" "^\\*Buffer List\\*$"
        "^\\*Backtrace\\*$" "^\\*Warnings\\*$" "^\\*WoMan-Log\\*$"
        "^\\*Compile-Log\\*$" "^\\*tramp/.+\\*$" "^\\*Faces\\*$"
        "^\\*evil-marks\\*$" "^\\*evil-registers\\*$" "\\*Packages\\*"
        "^\\*Shell Command Output\\*$" "^\\*magit\\(-\\w+\\)?: .+$"))

;;; Side Window Buffers
(setq my-side-window-buffers
      '((" *undo-tree*" :width 60 :position right)
        help-mode Buffer-menu-mode compilation-mode messages-buffer-mode
        "*Warnings*" "*Backtrace*" "*evil-marks*" "*evil-registers*"
        (magit-log-mode :position right)
        (magit-refs-mode :position right)
        (magit-stash-mode :position right)
        (magit-process-mode :position right)
        (magit-revision-mode :position right)
        (magit-diff-mode :noselect t :position right)
        magit-status-mode))

;;; Editing Modes
(setq my-editing-modes
      '(prog-mode text-mode conf-mode css-mode haskell-cabal-mode
                  gitignore-mode))

;;; Autosaves And Backups
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq-default backup-inhibited t)

;;; Language Specific Changes
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'highlight-quoted--turn-off)
(add-hook 'json-mode-hook 'highlight-numbers--turn-off)
(add-hook 'magit-mode-hook 'turn-off-evil-quickscope-mode)
(add-to-list 'auto-mode-alist '("README\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.cassius$" . shakespeare-lucius-mode))
(add-to-list 'dtrt-indent-hook-mapping-list
             '(groovy-mode c/c++/java c-basic-offset))

;;; Set Initial Evil States
(add-hook 'Man-mode-hook 'evil-motion-state)
(add-hook 'help-mode-hook 'evil-motion-state)
(evil-set-initial-state 'package-menu-mode 'motion)
(evil-set-initial-state 'messages-buffer-mode 'motion)
(evil-set-initial-state 'compilation-mode 'motion)
(evil-set-initial-state 'debugger-mode 'motion)

;;; Set Custom Variables
(custom-set-variables
 ;; Package List
 '(package-selected-packages package-list)
 ;; Better Motion
 '(scroll-conservatively 5)
 '(make-pointer-invisible nil)
 '(magit-display-buffer-noselect t)
 ;; Better Start And Exit
 '(inhibit-startup-screen t)
 '(require-final-newline t)
 ;; Better Editing
 '(evil-leader/leader ",")
 '(evil-want-fine-undo 'no)
 '(sentence-end-double-space nil)
 '(create-lockfiles nil)
 ;; Better Window Behavior
 '(same-window-regexps '("."))
 ;; Better VC Behavior
 '(vc-follow-symlinks t)
 '(magit-push-always-verify nil)
 '(magit-no-message '("Turning on magit-auto-revert-mode..."))
 ;; Better Term Behavior
 '(term-suppress-hard-newline t)
 '(multi-term-program "/bin/zsh")
 '(multi-term-switch-after-close nil)
 '(Man-notify-method 'pushy)
 ;; Better Display
 '(show-paren-mode t)
 '(column-number-mode t)
 '(whitespace-style '(face trailing))
 ;; Consistent Font Size
 '(monokai-height-minus-1 1.0)
 '(monokai-height-plus-1 1.0)
 '(monokai-height-plus-2 1.0)
 '(monokai-height-plus-3 1.0)
 '(monokai-height-plus-4 1.0)
 ;; Indentation
 '(highlight-indent-guides-method 'character)
 '(indent-tabs-mode nil)
 '(dtrt-indent-mode t)
 '(tab-width 4)
 ;; Fill
 '(fill-column 80)
 '(whitespace-line-column nil)
 '(fci-handle-truncate-lines nil)
 '(fci-rule-color "#3A3A3A")
 '(truncate-partial-width-windows 90)
 '(git-commit-summary-max-length 50)
 '(git-commit-fill-column 72)
 ;; Autosaves And Backups
 '(auto-save-list-file-prefix autosave-dir)
 '(auto-save-file-name-transforms `((".*" ,autosave-dir t)))
 '(make-backup-files nil)
 ;; Ivy
 '(projectile-completion-system 'ivy)
 '(ivy-ignore-buffers my-boring-buffers)
 '(ivy-add-newline-after-prompt t)
 '(ivy-fixed-height-minibuffer t)
 '(ivy-on-del-error-function nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-extra-directories nil)
 '(ivy-height 15)
 ;; Web Links
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-program "chromium")
 ;; Maven
 '(maven-test-test-task-options "-q -Pitest")
 '(maven-test-test-method-name-regexes
   '("void +\\([a-zA-Z0-9_]+\\) *()\\(?: *throws *[a-zA-Z0-9_]+\\)? *\n? *{")))

;;; Fix Ivy Searching
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq ivy-initial-inputs-alist nil)

;;; Magit Gerrit
(setq-default magit-gerrit-ssh-creds "DarthFennec@review.gerrithub.io")
(setq-default magit-gerrit-remote "gerrit")

;;; Editing Mode Helpers
(defmacro my-add-hook-editing-modes (hook)
  (let* ((mkhk (lambda (mode) (intern (concat (symbol-name mode) "-hook"))))
         (ms (lambda (mode) `(add-hook ',(funcall mkhk mode) ,hook))))
    `(progn ,@(mapcar ms my-editing-modes))))

(defmacro my-check-if-editing-mode-p (lst)
  `(or ,@(mapcar (lambda (mode) `(memq ',mode ,lst)) my-editing-modes)))

;;; Set Buffer Indentation
(defun set-indent (width)
  (set (caddr (dtrt-indent--search-hook-mapping major-mode)) width))

;;; Clean Projectile Listing
(defun clean-projectile-list ()
  (interactive)
  (let ((f (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
        prelist postlist)
    (with-demoted-errors "Error during file deserialization: %S"
      (if (not (file-writable-p f))
          (message "Projectile list unwritable")
        (setq prelist
              (nreverse
               (with-temp-buffer
                 (insert-file-contents f)
                 (read (buffer-string)))))
        (dolist (elem prelist)
          (when (file-exists-p (expand-file-name elem))
            (setq postlist (cons elem postlist))))
        (with-temp-file f
          (insert (let (print-length) (prin1-to-string postlist))))
        (message "Projectile list cleaned")))))

;;; Display Proper Sources
(defmacro make-display-sources (funcname advname)
  `(defadvice ,funcname (after ,advname activate)
     (let ((mstr "^/usr/share/emacs/[0-9.]+")
           (rstr (expand-file-name "~/Documents/Reference/emacs"))
           (retval ad-return-value))
       (when retval
         (save-match-data
           (when (string-match mstr retval)
             (setq retval (replace-match rstr nil nil retval)))
           (when (string-match ".elc$" retval)
             (setq retval (replace-match ".el" nil nil retval))))
         (setq ad-return-value retval)))))

(make-display-sources symbol-file my-symbol-file)
(make-display-sources locate-file my-locate-file)

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

(defadvice pop-to-buffer (before my-help-pop-to-buffer activate)
  (when (and (eq major-mode 'help-mode) (boundp 'location))
    (let* ((window (if (window-minibuffer-p) (minibuffer-selected-window)
                     (selected-window)))
           (parent window))
      (while (and parent (not (window-parameter parent 'window-side)))
        (setq parent (window-parent parent)))
      (when parent (delete-window window)))))

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
    (when (and live (or (null (car node)) (caddr (cadr node))))
      (setq retval 'no-sel))
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
    (unless node
      (setq node '(nil nil))
      (set-frame-parameter nil 'my-side-window-tree node))
    (when node (my-display-find-windows-in-stack node))
    (when (and node tptr)
      (setq window
            (-first
             (lambda (x)
               (let ((p x))
                 (while (and p (not (window-parameter p 'window-side)))
                   (setq p (window-parent p)))
                 (and p (eq (car win) (window-parameter p 'window-side))
                      (eq (cadr win) (window-parameter p 'window-slot)))))
             (get-buffer-window-list (caadr tptr))))
      (when window (setcar (cdadr tptr) (current-window-configuration))))
    (when (null (cadr node))
      (setcar (cdr node) (current-window-configuration)))))

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
               (or (null (car sel)) (not (eq (car buffer) (caadr sel)))))
      (setcdr (cdr sel) (cons (list to buffer) (cddr sel)))
      (unless (caddr (cadr (caddr sel))) (setq sel (caddr sel))))
    (my-display-prune-stack stack)
    (when (car sel) (setcar (cdddr (cadr sel)) (current-time)))
    (my-display-collect-windows-in-stack stack)
    (save-excursion
      (when (and (null blist) (cadr stack))
        (set-window-configuration (cadr stack))
        (setcar (cdr stack) nil))
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
  (let ((parent (expand-file-name filename)))
    (while (and parent (not (file-exists-p parent)))
      (setq parent (file-name-directory (directory-file-name parent))))
    (unless (or (not parent) (file-writable-p parent))
      (setq filename (concat "/sudo::" (expand-file-name filename))))))

;;; Newline Auto Comment
(defadvice newline (around my-comment-newline activate)
  (let ((lst (parent-mode-list major-mode)))
    (if (my-check-if-editing-mode-p lst)
        (let ((my-comment-starter
               (if (and (not (bolp)) (eolp))
                   (fill-context-prefix (1- (point)) (1- (point)))
                 (fill-context-prefix (point) (point)))))
          ad-do-it)
      ad-do-it)))

;;; Open Line Auto Comment
(defvar-local my-comment-starter nil)

(defadvice indent-according-to-mode (before my-insert-comment-starter activate)
  (when (and my-comment-starter (bolp))
    (insert my-comment-starter)))

(defadvice evil-cleanup-insert-state (after my-clear-comment-starter activate)
  (setq my-comment-starter nil))

(defmacro my-open-comment-build (oper name)
  `(defadvice ,oper (before ,name activate)
     (when (null my-comment-starter)
       (setq my-comment-starter (fill-context-prefix (point) (point))))))
(my-open-comment-build evil-open-below my-evil-open-comment-below)
(my-open-comment-build evil-open-above my-evil-open-comment-above)

;;; Fix Haskell Reindent
(defadvice haskell-indentation-next-indentation (before my-hs-indent activate)
  (when (memq this-command '(evil-open-above evil-open-below))
    (setq col (save-excursion
                (end-of-line 0)
                (1- (haskell-indentation-current-indentation))))))

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

;;; Ivy And Friends
(ivy-mode 1)
(projectile-global-mode 1)
(counsel-projectile-on)

;;; Evil And Friends
(global-evil-leader-mode 1)
(evil-mode 1)
(key-chord-mode 1)

;;; Other Helpful Modes
(evil-commentary-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode 1)
(global-evil-quickscope-mode 1)
(global-evil-matchit-mode 1)
(global-subword-mode 1)
(evil-exchange-install)
(evil-indent-plus-default-bindings)

;;; Custom Highlights
(defface custom-highlights-todo-face
  '((t (:weight bold :foreground "white" :background "black")))
  "Face to highlight task tags.")

(defface custom-highlights-non-ascii-face
  '((t (:background "#A20C41")))
  "Face to highlight non-ascii characters.")

(defface custom-highlights-tab-face
  '((t (:background "#282222")))
  "Face to highlight tab characters.")

(defvar my-task-tags
  '("ATTENTION" "ATTN" "BUG" "CHECK" "DEBUG" "DEPRECATED" "FIX" "FIXME" "HACK"
    "NOTE" "NOTES" "OPTIMIZE" "REFACTOR" "REVIEW" "TBD" "TEMP" "TEST" "TMP"
    "TODO" "WTF" "XXX"))

(defun custom-highlights ()
  (font-lock-add-keywords
   nil
   `((,(regexp-opt my-task-tags 'words) 0 'custom-highlights-todo-face t)
     ("[[:nonascii:]]" 0 'custom-highlights-non-ascii-face t)
     ("\t" 0 'custom-highlights-tab-face t)) t))
(my-add-hook-editing-modes 'custom-highlights)

;;; Highlighting And Misc
(global-paren-face-mode 1)
(my-add-hook-editing-modes 'fci-mode)
(my-add-hook-editing-modes 'linum-mode)
(my-add-hook-editing-modes 'hl-line-mode)
(my-add-hook-editing-modes 'whitespace-mode)
(my-add-hook-editing-modes 'electric-pair-mode)
(my-add-hook-editing-modes 'highlight-indent-guides-mode)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;;; Load Theme
(load-theme 'monokai t)

;;; Face Customizations
(set-face-attribute 'linum nil :inherit 'default)

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
    (let ((path default-directory) dir vctype vctype1 ptype ptype1)
      (if (not (projectile-project-p))
          (setq dir (if (string= default-directory my-home-dir) "~"
                      (file-name-base (directory-file-name default-directory))))
        (setq path (projectile-project-root)
              dir (projectile-project-name)
              ptype1 (projectile-project-type)
              vctype1 (projectile-project-vcs))
        (unless (eq 'generic ptype1)
          (setq ptype (concat ":" (symbol-name ptype1))))
        (unless (eq 'none vctype1)
          (setq vctype (concat ":" (symbol-name vctype1)))))
      (setq my-vc-data (cons path (concat "[" dir ptype vctype "]")))))
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
         (offs (let ((x (symbol-value (cdr my-indent-offset))))
                 (if (numberp x) (number-to-string x) "?")))
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
        (size (format-mode-line " (%l,%c) ")))
    (when (string= perc "Bottom") (setq perc "Bot"))
    (when (string-match-p "[0-9]+%$" perc) (setq perc (concat perc "%")))
    (concat size perc " ")))

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

;;; Message Buffer Should Use New Modeline
(kill-buffer "*Messages*")

;;;; Keybindings

;;; Custom Ex Commands
(evil-ex-define-cmd "k[ill-buffer]" 'kill-this-buffer)

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

;;; Bind ^J/^K To Scroll Screen
(define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-up)

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
  (evil-with-single-undo
    (evil-insert-newline-below)
    (previous-line)
    (evil-open-below count)))
(define-key evil-normal-state-map "go" 'evil-open-below-with-extra-line)

(defun evil-open-above-with-extra-line (count)
  (interactive "p")
  (evil-with-single-undo
    (evil-insert-newline-above)
    (next-line)
    (evil-open-above count)))
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

;;; Ivy Projectile Special Handling
(defun my-ivy-alt-done (&optional arg)
  (interactive "P")
  (if (string-suffix-p " Switch to project: " ivy--prompt)
      (ivy-dispatching-done)
    (ivy-alt-done arg)))

;;; Ivy Evil Motions
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-l") 'counsel-up-directory)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "RET") 'my-ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "<C-return>") 'my-ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "<S-return>") 'ivy-done)

;;; Counsel Bindings
(define-key help-map (kbd "f") 'counsel-describe-function)
(define-key help-map (kbd "v") 'counsel-describe-variable)
(define-key global-map (kbd "M-x") 'counsel-M-x)

;;; Leader, Ex, And C-w Bindings Everywhere
(define-key global-map (kbd "C-,") evil-leader--default-map)
(define-key global-map (kbd "C-;") 'evil-ex)
(define-key global-map (kbd "C-w") 'evil-window-map)
(define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)

;; ;;; Term Motions
;; (defun my-term-motion (motn &optional pt mk ct st yt ps)
;;   (delete-trailing-whitespace)
;;   (let* ((term-proc (get-buffer-process (current-buffer)))
;;          (term-point (marker-position (process-mark term-proc)))
;;          (ptn (if (eq pt t) (point) pt)) (mkn (if (eq mk t) (mark) mk))
;;          (pto (int-to-string (- ptn term-point)))
;;          (mko (when mk (concat "," (int-to-string (- mkn term-point)))))
;;          (cnt (when ct (concat "#" (int-to-string ct))))
;;          (psd (when (and ps (> (length ps) 0)) ps))
;;          (psl (when psd (concat ";" (int-to-string (length psd)))))
;;          (stp (or st "c")) (ytp (or yt "c"))
;;          (motn-cmd (concat "\e[::" pto mko cnt psl ":" motn stp ytp psd)))
;;     (term-send-raw-string motn-cmd)))

;; (defmacro my-term-change-delete-build (oper name motn)
;;   `(defadvice ,oper (around ,name activate)
;;      (if (not (eq major-mode 'term-mode))
;;          (flet ((called-interactively-p (kind) t)) ad-do-it)
;;        (my-term-motion ,motn beg end nil (case type (line "l") (block "b")))
;;        ,@(when (string= motn "c") '((evil-insert-state))))))
;; (my-term-change-delete-build evil-change my-term-change "c")
;; (my-term-change-delete-build evil-delete my-term-delete "d")

;; (defmacro my-term-paste-build (oper name motn)
;;   `(defadvice ,oper (around ,name activate)
;;      (if (not (eq major-mode 'term-mode)) ad-do-it
;;        (let* ((txt (if register (evil-get-register register) (current-kill 0))))
;;          (when txt
;;            (when (and (not yank-handler) (stringp txt))
;;              (setq yank-handler
;;                    (car-safe (get-text-property 0 'yank-handler txt))))
;;            (when (vectorp txt) (setq txt (evil-vector-to-string txt)))
;;            (setq yank-handler
;;                  (cond ((eq yank-handler 'evil-yank-line-handler) "l")
;;                        ((eq yank-handler 'evil-yank-block-handler) "b")))
;;            (my-term-motion ,motn t nil count nil yank-handler txt))))))
;; (my-term-paste-build evil-paste-before my-term-paste-before "P")
;; (my-term-paste-build evil-paste-after my-term-paste-after "p")

;; (defadvice mouse-yank-primary (around my-term-mouse-paste activate)
;;   (if (not (eq major-mode 'term-mode)) ad-do-it
;;     (run-hooks 'mouse-leave-buffer-hook)
;;     (when select-active-regions (let (select-active-regions) (deactivate-mark)))
;;     (or mouse-yank-at-point (mouse-set-point click))
;;     (let ((text
;;            (if (fboundp 'gui-get-primary-selection)
;;                (if (eq (framep (selected-frame)) 'w32)
;;                    (or (gui-get-selection 'PRIMARY) (gui-get-primary-selection))
;;                  (or (gui-get-primary-selection) (gui-get-selection 'PRIMARY)))
;;              (gui-get-selection 'PRIMARY))))
;;       (if text (my-term-motion "p" t nil nil nil nil text)
;;         (error "No selection is available")))))

;; (defmacro my-term-motion-do (ins motn &optional pt mk ct st yt ps)
;;   `(lambda ()
;;      (interactive)
;;      (my-term-motion ,motn ,pt ,mk ,ct ,st ,yt ,ps)
;;      ,@(when ins '((evil-insert-state)))))
;; (evil-define-key 'normal term-raw-map "A" (my-term-motion-do t "A" t))
;; (evil-define-key 'normal term-raw-map "a" (my-term-motion-do t "a" t))
;; (evil-define-key 'normal term-raw-map "I" (my-term-motion-do t "I" t))
;; (evil-define-key 'normal term-raw-map "i" (my-term-motion-do t "i" t))

;;; C-RET Fills Current Line
(defun fill-current-line ()
  (interactive)
  (fill-region (point-at-bol) (point-at-eol)))
(define-key evil-insert-state-map (kbd "<C-return>") 'fill-current-line)
(define-key evil-replace-state-map (kbd "<C-return>") 'fill-current-line)

;;; Projectile Switch Project Map
(defmacro my-bind-action (key act doc)
  `'(,key (lambda (dir)
            (let ((projectile-switch-project-action ,act))
              (projectile-switch-project-by-name dir arg))) ,doc))

(ivy-set-actions
 'counsel-projectile-switch-project
 (list
  (my-bind-action "t" 'multi-term "open terminal")
  (my-bind-action "g" 'magit-status "open in magit")
  (my-bind-action "e" 'counsel-find-file "edit file")
  (my-bind-action "s" 'counsel-projectile-ag "search with ag")
  (my-bind-action "f" 'counsel-projectile-find-file "find file")))

;;; Leader Bindings
(evil-leader/set-key
  "t" 'multi-term
  "g" 'magit-status
  "q" 'update-packages
  "u" 'undo-tree-visualize
  "d" 'evil-show-file-info
  "a" 'ivy-resume
  "e" 'counsel-find-file
  "b" 'ivy-switch-buffer
  "s" 'counsel-projectile-ag
  "f" 'counsel-projectile-find-file
  "p" 'counsel-projectile-switch-project
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
