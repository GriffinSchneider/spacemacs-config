(setq exec-path (append exec-path '("~/.nvm/versions/node/v13.5.0/bin")))
(setq undo-tree-enable-undo-in-region nil)
(setq history-delete-duplicates t)
(setq frame-resize-pixelwise t)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(spacemacs/set-default-font '("Input Mono Narrow" :size 12))

(set-face-attribute 'variable-pitch nil :family "Input Sans Condensed")
(global-visual-line-mode)

(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
(ivy-posframe-mode 1)
(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
(setq ivy-posframe-border-width 10)

(use-package adaptive-wrap
  :config (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package org-roam
  :after org
  :hook (org-mode . org-roam-mode)
  :custom
  (org-roam-directory "~/roam/")
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file-functionile)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))
(with-eval-after-load 'org
  (defun gcs-org-paste-image ()
    (interactive)
    (let* ((image-folder (concat default-directory "res/img/screenshots/"))
           (image-name (format-time-string "%Y%m%d%H%M%S.png"))
           (image-file (concat image-folder image-name))
           (exit-status (call-process "pngpaste" nil nil nil image-file)))
      (insert "#+ATTR_ORG: :width 600")
      (newline-and-indent)
      (org-insert-link nil (concat "file:" image-file) nil)
      (org-display-inline-images)))
  (set-face-attribute 'org-drawer nil :height 0.5)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (defun gcs-org-mode-hook ()
    (olivetti-mode 1))
  (add-hook 'org-mode-hook 'gcs-org-mode-hook)

  (defun gcs-org-show-notification-handler (msg)
    (ns-do-applescript (concat "display notification \"" msg "\" with title \"Org Notification\" sound name \"Basso\"")))
  (setq org-show-notification-handler 'gcs-org-show-notification-handler)

  (setq org-todo-keywords '((sequence "TODO(!)" "DOING(!)" "CANCELLED(c!)" "HOLDING(h!)" "DONE(!)")))
  (setq org-log-into-drawer t)
  (setq org-log-done nil)
  (setq org-log-states-order-reversed nil)

  (setq org-modules '(org-habit ol-eww ol-docview))
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-superstar-special-todo-items t)

  (setq org-agenda-files '("~/roam"))
  (setq org-extend-today-until 6)
  (setq org-agenda-span 10
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-5d")
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-graph-column 80)
  (setq org-roam-dailies-capture-templates
        `(("d" "daily" plain (function org-roam-capture--get-point) ""
           :head ,(concat
             "* %<%Y-%m-%d>\n"
             " <[[file:%(format-time-string \"%Y-%m-%d\" (time-add (* -1 86400) (s--aget org-roam-capture--info 'time))).org][yesterday]]>"
             " <[[file:%(format-time-string \"%Y-%m-%d\" (time-add (*  1 86400) (s--aget org-roam-capture--info 'time))).org][tomorrow]]>\n"
             )
           :immediate-finish t
           :file-name "%<%Y-%m-%d>"
           )))

  (setq org-publish-project-alist
        `(("roam-files"
           :base-directory "~/roam"
           :base-extension "org"
           :publishing-directory "~/roam-publish/html"
           :publishing-function org-html-publish-to-html
           :with-author nil
           :with-toc nil
           :html-head-include-scripts nil
           :html-head-include-default-style nil
           :html-validation-link nil
           :html-head-extra ,gcs-org-preamble
           :preserve-breaks t
           )
          ("roam-res"
           :base-directory "~/roam/res"
           :base-extension ".*"
           :recursive t
           :publishing-directory "~/roam-publish/html/res"
           :publishing-function org-publish-attachment
           )
          ("roam" :components ("roam-files" "roam-res"))))
  (defun my/org-roam--backlinks-list-with-content (file)
    (with-temp-buffer
      (if-let* ((backlinks (org-roam--get-backlinks file))
                (grouped-backlinks (--group-by (nth 0 it) backlinks)))
          (progn
            (insert (format "\n\n* %d Backlinks\n"
                            (length backlinks)))
            (dolist (group grouped-backlinks)
              (let ((file-from (car group))
                    (bls (cdr group)))
                (insert (format "** [[file:%s][%s]]\n"
                                file-from
                                (org-roam--get-title-or-slug file-from)))
                (dolist (backlink bls)
                  (pcase-let ((`(,file-from _ ,props) backlink))
                    (insert (s-trim (s-replace "\n" " " (plist-get props :content))))
                    (insert "\n\n")))))))
      (buffer-string)))
  (defun my/org-export-preprocessor (backend)
    (let ((links (my/org-roam--backlinks-list-with-content (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n") links)))))
  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))

(with-eval-after-load 'olivetti
  (setq-default olivetti-body-width 130))

(with-eval-after-load 'term
  (defun gcs-clear-terminal ()
    (interactive)
    (let ((inhibit-read-only t)) (term-reset-terminal)))
  (setq multi-term-program "/usr/local/bin/fish")
  (define-key term-mode-map (kbd "s-K") 'gcs-clear-terminal)
  (define-key term-raw-map (kbd "s-K") 'gcs-clear-terminal))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; Craziness to enable fira code glyphs from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions.kj
; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;                (36 . ".\\(?:>\\)")
;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;                (48 . ".\\(?:x[a-zA-Z]\\)")
;                (58 . ".\\(?:::\\|[:=]\\)")
;                (59 . ".\\(?:;;\\|;\\)")
;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;                (91 . ".\\(?:]\\)")
;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;                (94 . ".\\(?:=\\)")
;                (119 . ".\\(?:ww\\)")
;                (123 . ".\\(?:-\\)")
;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;                )
;              ))
;   (dolist (char-regexp alist)
;     (set-char-table-range composition-function-table (car char-regexp)
;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

(with-eval-after-load 'auto-complete
  (define-key ac-completing-map (kbd "s-e") 'ac-next)
  (define-key ac-menu-map (kbd "s-k") 'ac-previous)
  (define-key ac-completing-map (kbd "s-j") 'ac-next)
  (define-key ac-completing-map (kbd "s-k") 'ac-previous))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "s-j") 'company-select-next)
  (define-key company-active-map (kbd "s-k") 'company-select-previous))

(with-eval-after-load 'tetris
  (evilified-state-evilify tetris-mode tetris-mode-map
    "j" 'tetris-move-left
    "f" 'tetris-move-bottom
    "k" 'tetris-rotate-prev
    "l" 'tetris-move-right
    "m" 'tetris-move-down
    "q" 'spacemacs/tetris-quit-game))

(with-eval-after-load 'powerline
  (setq powerline-height 15))



(with-eval-after-load 'magit
  (evil-define-key 'normal magit-mode-map "\\" nil))

(with-eval-after-load 'ibuffer
  (defconst gcs-ibuffer-fontification-alist
    '((ruby-mode . font-lock-string-face)
      (sh-mode . font-lock-string-face)
      (objc-mode . font-lock-constant-face)
      (c-mode . font-lock-constant-face)
      (java-mode . font-lock-constant-face)
      (emacs-lisp-mode . font-lock-variable-name-face)
      (org-mode . font-lock-negation-char-face)
      (dired-mode . font-lock-function-name-face)
      (term-mode . font-lock-doc-string-face)))
  (setq ibuffer-formats
        `((mark
           modified
           read-only
           " "
           (name 30 30 :left :elide)
           ,(propertize "| " 'font-lock-face ibuffer-title-face)
           (mode 10 10 :left)
           ,(propertize " | " 'font-lock-face ibuffer-title-face)
           filename)))
  (setq ibuffer-fontification-alist
        `(,@(mapcar (lambda (b)
                      `(9999 (eq major-mode ',(car b)) ,(cdr b)))
                    gcs-ibuffer-fontification-alist)
          (90 (string-match "magit" (symbol-name major-mode))
              font-lock-function-name-face)
          (90 (or (string-match "^*" (buffer-name))
                  (memq major-mode ibuffer-help-buffer-modes))
              font-lock-comment-face)))
  (define-key ibuffer-mode-map (kbd "C-g") 'quit-window)
  (define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line)
  (define-key ibuffer-mode-map (kbd "C-j") 'ibuffer-forward-filter-group)
  (define-key ibuffer-mode-map (kbd "C-k") 'ibuffer-backward-filter-group)
  (defun gcs-ibuffer-hook ()
    (face-remap-add-relative 'default 'font-lock-comment-face)
    (copy-face 'font-lock-keyword-face 'tempface )
    (setq ibuffer-filter-group-name-face 'tempface)
    (face-remap-add-relative ibuffer-filter-group-name-face
                             :box '(:style released-button
                                           :line-width 2))
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (add-hook 'ibuffer-hook 'gcs-ibuffer-hook))

(with-eval-after-load 'neotree
  ;; Improve performance with icon fonts
  (setq inhibit-compacting-font-caches t)
  (setq neo-theme 'icons)
  ;; Override to not insert a chevron icon, and just use open vs closed folder icons to indicate whether it's open or closed.
  (defun gcs-all-the-icons-icon-for-dir (dir &optional chevron padding)
    (let* ((matcher (all-the-icons-match-to-alist (file-name-base (directory-file-name dir)) all-the-icons-dir-icon-alist))
           (path (expand-file-name dir))
           (padding (or padding "\t"))
           (icon (cond
                  ((file-symlink-p path)
                   (all-the-icons-octicon "file-symlink-directory" :height 1.0))
                  ((all-the-icons-dir-is-submodule path)
                   (all-the-icons-octicon "file-submodule" :height 1.0))
                  ((file-exists-p (format "%s/.git" path))
                   (format "%s" (all-the-icons-octicon "repo" :height 1.1)))
                  (t (apply (car matcher) (cdr matcher)))))
           (good-folder (if (equal chevron "down") (all-the-icons-faicon "folder-open") (all-the-icons-faicon "folder")))
           (real-icon (if (equal icon (all-the-icons-octicon "file-directory")) good-folder icon)))
      (format "%s%s%s" padding real-icon padding)))
  (advice-add #'all-the-icons-icon-for-dir :override #'gcs-all-the-icons-icon-for-dir)
  ;; Override to not insert a bunch of indentation before leaf nodes, now that directories have less indentation due to
  ;; removing the huge chevron.
  (defun gcs-neo-buffer--insert-fold-symbol (orig-fun &rest args)
    (if (equal (car args) 'leaf)
        (insert (format "\t%s\t" (all-the-icons-icon-for-file (cadr args))))
      (apply orig-fun args)))
  (advice-add #'neo-buffer--insert-fold-symbol :around #'gcs-neo-buffer--insert-fold-symbol))

(with-eval-after-load 'typescript-mode
  ;; Typescript standard lib .d.ts files are full of \M
  (defun remove-dos-eol ()
    "Do not show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))
  (add-hook 'typescript-mode-hook 'remove-dos-eol))


(with-eval-after-load 'helm
  (defun gcs-helm-find-files-smart-enter ()
    "Avoid dired - RET is like pressing TAB on directories, except if it's the '/.' that
shows at the top of every directory. In that case, open magit status on the directory"
    (interactive)
    (cond
     ((and (stringp (helm-get-selection)) (not (file-directory-p (helm-get-selection))))
      (helm-maybe-exit-minibuffer))
     ((string-equal (substring (helm-get-selection) -2) "/.")
      (let ((repo (helm-get-selection)))
        (run-at-time nil nil 'gcs-maybe-magit-directory repo)
        (call-interactively 'helm-keyboard-quit)))
     (t
      (helm-execute-persistent-action))))
  (define-key helm-map (kbd "s-j") 'helm-next-line)
  (define-key helm-map (kbd "s-k") 'helm-previous-line)
  (define-key helm-find-files-map (kbd "<return>") 'gcs-helm-find-files-smart-enter))

;; GRIFF STUFF ;;;;;;
(require 'cl)
(require 'ibuffer)
(defun map2 (function list)
  (case (length list)
    (0 list)
    (1 (error "map2 got an odd-length list"))
    (t (cons (funcall function (first list) (second list))
             (map2 function (cddr list))))))

(defmacro global-set-keys (&rest bindings)
  `(progn ,@(map2 (lambda (key command)
                    (if (listp command)
                        `(global-set-key (kbd ,key) (lambda () (interactive) ,command))
                      `(global-set-key (kbd ,key) ',command)))
                  bindings)))

(defun gcs-copy-buffer-to-window (dir)
  "Copy the current window's buffer onto the buffer list of the window
 in direction DIR."
  (let* ((this-window (selected-window))
         (other-window (progn
                         (windmove-do-window-select dir)
                         (selected-window)))
         (this-buffer  (window-buffer this-window))
         (other-buffer (window-buffer other-window))
         (this-start   (window-start this-window))
         (other-start  (window-start other-window)))
    (set-window-buffer other-window this-buffer)
    (set-window-start  other-window this-start)))

(defun gcs-put-buffer-in-window (dir)
  "Pop the current window's buffer off the window's buffer list
 and push it onto the buffer list of the window in direction DIR."
  (let* ((this-window (selected-window)))
    (gcs-copy-buffer-to-window dir)
    (switch-to-prev-buffer this-window)))

(defun gcs-go-to-definition ()
  (interactive)
  (if (bound-and-true-p tide-mode)
      (tide-jump-to-definition)
    (spacemacs/jump-to-definition)))

(defun gcs-command-click (event)
  (interactive "e")
  (mouse-set-point event)
  (gcs-go-to-definition))

(global-set-key [s-mouse-1] 'gcs-command-click)

(global-set-keys
 ;; Use [C-]s-[y, u, i, o] to resize windows
 "s-y"   (shrink-window-horizontally 5)
 "s-u"   (shrink-window 5)
 "s-i"   (enlarge-window 5)
 "s-o"   (enlarge-window-horizontally 5)

 ;; Xcode-like keybindings
 ; "s-O" gcs-find-file-dwim
 "C-s-<up>" ff-find-other-file

 ;; Use s-[h, j, k, l] for window navigation
 "s-h" windmove-left
 "s-l" windmove-right
 "s-k" windmove-up
 "s-j" windmove-down

 ;; Use s-[H, J, K, L] to swap windows
 "s-C-H" (gcs-put-buffer-in-window  'left)
 "s-M-h" (gcs-copy-buffer-to-window 'left)
 "s-M-˙" (gcs-copy-buffer-to-window 'left)
 "s-C-J" (gcs-put-buffer-in-window  'down)
 "s-M-j" (gcs-copy-buffer-to-window 'down)
 "s-M-∆" (gcs-copy-buffer-to-window 'down)
 "s-C-K" (gcs-put-buffer-in-window  'up)
 "s-M-k" (gcs-copy-buffer-to-window 'up)
 "s-M-˚" (gcs-copy-buffer-to-window 'up)
 "s-C-L" (gcs-put-buffer-in-window  'right)
 "s-M-l" (gcs-copy-buffer-to-window 'right)
 "s-M-¬" (gcs-copy-buffer-to-window 'right)

 ;; s-=, s--, and s-0 to adjust font size like in a browser
 "s-=" (text-scale-increase 1)
 "s--" (text-scale-increase -1)
 "s-0" (text-scale-adjust 0)

 "s-J" (neotree-show)

 "s-C-M-T" tetris
)

 ;;;;; EVIL STATE MAP KEYS ;;;;;
(defun gcs-define-evil-motion-key (key def)
  (define-key evil-normal-state-map key def)
  (define-key evil-visual-state-map key def)
  (define-key evil-motion-state-map key def))

;; Use C-j and C-k for scrolling 2 lines up/down, similar to w3m binding
(defmacro def-gcs-move-line (updown count)
  (let* ((name (if (eq 'up (eval updown)) "gcs-previous-line-" "gcs-next-line-"))
         (funsymbol (intern (concat name (int-to-string count)))))
    `(evil-define-command ,funsymbol (&optional fakecount)
       "Move the cursor COUNT lines down."
       :repeat motion :type line :keep-visual t
       (interactive "<c>")
       ,(if (eq (eval updown) 'up)
            `(progn (evil-scroll-line-up ,count) (evil-previous-line ,count))
          `(progn (evil-scroll-line-down ,count) (evil-next-line ,count))))))

(def-gcs-move-line 'up 2)
(def-gcs-move-line 'down 2)

(gcs-define-evil-motion-key (read-kbd-macro "C-j") 'gcs-next-line-2)
(gcs-define-evil-motion-key (read-kbd-macro "C-k") 'gcs-previous-line-2)

;; Use j and k pressed within .15 seconds to exit insert mode
(defun gcs-evil-maybe-exit (entry-key exit-key)
  (let ((modified (buffer-modified-p)))
    (insert entry-key)
    (let ((evt (read-event nil nil 0.15)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

(evil-define-command gcs-evil-maybe-exit-j ()
  :repeat change
  (interactive)
  (gcs-evil-maybe-exit ?j ?k))
(define-key evil-insert-state-map "j" 'gcs-evil-maybe-exit-j)

(evil-define-command gcs-evil-maybe-exit-k ()
  :repeat change
  (interactive)
  (gcs-evil-maybe-exit ?k ?j))
(define-key evil-insert-state-map "k" 'gcs-evil-maybe-exit-k)

;; Smooth half-page scrolling
(defun gcs-smooth-scroll-up (n)
  (when (> n 0)
    (scroll-down-line 7)
    (run-at-time 0.016 nil 'gcs-smooth-scroll-up (- n 7))))
(defun gcs-smooth-scroll-down (n)
  (when (> n 0)
    (scroll-up-line 7)
    (run-at-time 0.016 nil 'gcs-smooth-scroll-down (- n 7))))
(defun gcs-smooth-scroll-down-half-screen ()
  (interactive)
  (gcs-smooth-scroll-down (/ (window-height) 2)))
(defun gcs-smooth-scroll-up-half-screen ()
  (interactive)
  (gcs-smooth-scroll-up (/ (window-height) 2)))
(evil-global-set-key 'normal (kbd "C-d") 'gcs-smooth-scroll-down-half-screen)
(evil-global-set-key 'normal (kbd "C-u") 'gcs-smooth-scroll-up-half-screen)



 ;;;;; PREFIX KEYBINDINGS ;;;;;
;; "\k" kills the buffer without asking and makes sure the buffer menu
;;  opens with point at the first line.
(defun gcs-kill-buffer-command ()
  (interactive)
  (kill-buffer (current-buffer))
  (let ((buffer-menu-buffer (get-buffer "*Ibuffer*")))
    (when buffer-menu-buffer
      (with-current-buffer buffer-menu-buffer
        (ibuffer-update nil)))))

;; "\K" kills the buffer like gcs-kill-buffer-command, while also killing
;; the window.
(defun gcs-kill-buffer-and-window ()
  (interactive)
  (gcs-kill-buffer-command)
  (delete-window))

;; Use \<left> and \<right> to navigate buffer list, ignoring buffer menu
(defun gcs-previous-buffer ()
  (interactive)
  (previous-buffer)
  (when (string= (buffer-name) "*Ibuffer*") (previous-buffer)))

(defun gcs-next-buffer ()
  (interactive)
  (next-buffer)
  (when (string= (buffer-name) "*Ibuffer*") (next-buffer)))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
    If no region is selected and current line is not blank and we are not at the end of the line,
    then comment current line.
    Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (case major-mode
    ('magit-mode (evil-magit-toggle-text-mode))
    (t
     (comment-normalize-vars)
     (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
         (comment-or-uncomment-region (line-beginning-position) (line-end-position))
       (comment-dwim arg)))))

(defun gcs-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8"
  (interactive)
  (setq tab-width (cond ((= tab-width 8) 4)
                        ((= tab-width 4) 2)
                        (t 8)))
  (message (format "Tab width is now %d" tab-width))
  (redraw-display))


(defun gcs-show-in-finder ()
  (interactive)
  (if (shell-command "open -a \"Path Finder\" .")
      (shell-command "open .")))

(defun gcs-open-with-external-editor ()
  "If this buffer isn't visiting a file, show default-directory in finder.
 If it is, open it with an external editor based on its major mode, or fallback
 to OSX defaults for unknon modes."
  (interactive)
  (if (not (buffer-file-name))
      (gcs-show-in-finder)
    (let* ((appname (case major-mode
                      ('objc-mode "Xcode")
                      ('java-mode "Eclipse")
                      ('emacs-lisp-mode "TextEdit")
                      (t nil)))
           (command (concat "open "
                            (if appname (concat "-a " appname " ") " ")
                            "\"" (buffer-file-name) "\"")))
      (shell-command command))))


(defconst gcs-prefix-key-commands
  (map2
   (lambda (key cmd) (list (read-kbd-macro key) cmd))
   '("q"   quit-window
     "k"   gcs-kill-buffer-command
     "s-k" delete-window
     "K"   gcs-kill-buffer-and-window
     "g"   magit-status
     "l"   gcs-go-to-definition
     "u"   undo-tree-visualize
     "a"   helm-projectile-ag
     "x"   helm-M-x

     "d"   helm-projectile-find-file
     "f"   spacemacs/helm-find-files
     ; "F"   ido-find-alternate-file
     "s-f" gcs-show-in-finder
     "s-x" gcs-open-with-external-editor

     "w" save-buffer
     "W" write-file
     "b" ibuffer
     "v" helm-mini
     ; ("V" ido-switch-buffer-other-frame)

     "s-v" visual-line-mode
     "s-b" magit-blame

     ; ("c" gcs-compile)
     "e" next-error
     "E" previous-error
     "r" eval-buffer

     "0" delete-window
     "7" delete-window
     "1" delete-other-windows
     "2" split-window-vertically
     "3" split-window-horizontally
     "4" balance-windows

     "<left>"  gcs-previous-buffer
     "<right>" gcs-next-buffer
     "\\"      comment-dwim-line
     "s-t"     gcs-toggle-tab-width-setting)))

(defun gcs-prefix-key-command ()
  (interactive)
  (let* ((old-cursor-color (prog1 (face-background 'cursor) (set-cursor-color "Green")))
         (key (read-key-sequence nil)))
    (set-cursor-color old-cursor-color)
    (call-interactively (second (assoc key gcs-prefix-key-commands)))))

(defconst gcs-prefix-key "\\")
(defconst gcs-prefix-key-maps (list evil-normal-state-map
                                    evil-motion-state-map
                                    evil-emacs-state-map))
(mapc (lambda (keymap)
        (define-key keymap gcs-prefix-key 'gcs-prefix-key-command)
        (define-key keymap (read-kbd-macro (concat "s-" gcs-prefix-key)) 'gcs-prefix-key-command))
      gcs-prefix-key-maps)



 ;;;;; KEY-CHORD KEYBINDINGS ;;;;;
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.035) ;; was 0.035

;; Any prefix key, "\x" can also be triggered with the key chord "jx"
(mapc (lambda (prefix-command)
        (let* ((key-string (first prefix-command))
               (key (aref key-string 0)))
          (when (and (numberp key) (<= key 126) (>= key 32)
                     (not (equal key-string "j"))
                     (not (equal key-string "k")))
            (key-chord-define-global (vector (aref "j" 0) key) (second prefix-command)))))
      gcs-prefix-key-commands)

                                        ; (key-chord-define-global "jl" 'gcs-helm-dwim)

;; Numbers for window splitting
(key-chord-define-global "89" 'split-window-vertically)
(key-chord-define-global "78" 'split-window-horizontally)

;; First fingers column
(key-chord-define evil-normal-state-map "jk" 'keyboard-quit)
(key-chord-define minibuffer-local-map "jk" 'abort-recursive-edit)
(key-chord-define-global "ji" 'org-roam-insert)
(key-chord-define-global "ip" 'gcs-org-paste-image)


(key-chord-define ibuffer-mode-map "jk" 'ibuffer-quit)
(key-chord-define-global "m," 'helm-M-x)

;; K + o or . for killing buffer or window
(key-chord-define-global "k." 'delete-window)
(key-chord-define-global "ko" 'gcs-kill-buffer-command)

;; H-chords for help
(key-chord-define-global "hf" 'describe-function)
(key-chord-define-global "hv" 'describe-variable)
(key-chord-define-global "hk" 'describe-key)

;; K + u or m for moving by half-screen
(key-chord-define-global "ku" 'gcs-smooth-scroll-up-half-screen)
(key-chord-define-global "km" 'gcs-smooth-scroll-down-half-screen)

(key-chord-define-global "kg" 'evil-goto-line)

;; Semicolon chords for evaluation
(defun gcs-eval-dwim ()
  (interactive)
  (if (not mark-active)
      (call-interactively 'eval-last-sexp)
    (call-interactively 'eval-region)
    (message "eval-ed.")))

(key-chord-define-global "j;" 'gcs-eval-dwim)
(key-chord-define-global "k;" 'eval-defun)
(key-chord-define-global "l;" 'eval-expression)


(defmacro gcs-quit-and-run (body &rest args)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (run-at-time nil nil (lambda (,@args) ,body))
     (minibuffer-keyboard-quit)))

(defun gcs-maybe-magit-directory (dir)
  "If dir is in a git repo, open git status on it. Otherwise, open dired in dir."
  (if (magit-toplevel dir)
      (magit-status-internal (file-name-directory dir))
    (dired dir)))



(defun gcs-magit-config ()
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (add-to-list 'magit-log-section-arguments "--graph")
  (evil-define-key 'normal magit-mode-map ";" 'magit-section-toggle)
  (evil-define-key 'normal magit-mode-map "\\" nil))
(eval-after-load "magit" #'gcs-magit-config)


(setq
 js2-basic-offset 2
 typescript-indent-level 2
 js-indent-level 2
 ;; web-mode
 css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2)


(setq msp-dev-dirs (list
  ;; "api-starter-kit"
  "auth-and-auth"
  "configured-rabbitmq-client"
  ;; "configured-winston"
  "gb-services"
  "gb-services-tester"
  "identity-api"
  "identity-serv"
  "loves-serv"
  "loyalty-serv"
  "membership-serv"
  "mobile-orchestration-api"
  ;; "notes"
  "notification-serv"
  "payment-api"
  "payment-serv"
  "payment-web"
  ;; "poi-serv"
  "secure-payment-serv"
  "service"
  "tooling"
  "wex-serv"
  ))

(defun msp-helm-dev-do-ag-region-or-symbol ()
  "Search in current project with `ag' using a default input."
  (interactive)
  (spacemacs//helm-do-ag-region-or-symbol 'helm-do-ag "/Users/mpav/dev/"))

(defun msp-helm-dev-do-ag ()
  "Search in current project with `ag' using a default input."
  (interactive)
  (helm-do-ag "/Users/mpav/dev/" msp-dev-dirs))

(spacemacs/set-leader-keys "sd" 'msp-helm-dev-do-ag)
(spacemacs/set-leader-keys "sD" 'msp-helm-dev-do-ag-region-or-symbol)

(spacemacs/toggle-vi-tilde-fringe-off)

(setq create-lockfiles nil)

(setq helm-display-function #'helm-display-buffer-in-own-frame)
(setq helm-display-buffer-reuse-frame t)
(setq helm-use-undecorated-frame-option t)
(setq helm-display-buffer-default-width 128)
(setq helm-display-buffer-width 128)


;; Which-key does (add-hook 'pre-command-hook #'which-key--hide-popup), which runs which-key--hide-buffer-side-window
;; Treemacs runs treemacs--fix-width-after-which-key in advice on which-key--hide-buffer-side-window
;; End result: typing is very slow with treemacs open when which-key is on.
;; Treemacs is gone now, but I don't use this anyway so leaving it off just in case.
(which-key-mode -1)


(setq gcs-org-preamble "
<style type='text/css'>
body {
  display: flex;
  flex-direction: column;
  align-items: center;
  font-family: 'Input Sans Condensed', 'Helvetica', 'Arial', sans-serif;
  font-weight: 100;
  color: #eee;
  background-color: #222;
}

#content {
  max-width: 50em;
}

a:link,
a:visited {
  color: #bbb;
  font-family: 'Input Sans Condensed', 'Helvetica', 'Arial', sans-serif;
  font-weight: normal;
  text-decoration: underline;
}

a:hover {
  background-color: #bbb;
  color: #000;
}

a:active {
  color: #f00;
}

h1, h2, h3, h4, h5, h6 {
  color: #fff;
  font-family: 'Input Sans', 'Helvetica', 'Arial', sans-serif;
  font-weight: normal;
  line-height: 1.5em;
  padding-top: 1em;
}

h4, h5, h6 {
  font-size: 1em;
}

h1.title {
  font-weight: normal;
  margin: 0 auto;
  padding: .2em 0;
  text-align: center;
}

#preamble {
  font-family: 'Input Sans Condensed', 'Helvetica', 'Arial', sans-serif;
  height: 24px;
  text-align: center;
}

#preamble a:link, #preamble a:visited {
  border: none;
  display: block;
  height: 24px;
  line-height: 24px;
  margin: 0 auto;
  text-decoration: none;
}

#preamble a:active, #preamble a:hover {
  border: none;
  background-color: transparent;
  color: #fff;
}

#postamble {
  color: #999;
  font-style: italic;
  text-align: right;
}

#postamble a.source-link:link,
#postamble a.source-link:visited {
  border-bottom: none;
  color: #ccc;
  font-family: 'Input Mono Condensed', monospace;
  font-size: .7em;
  font-style: normal;
  line-height: 24px;
  text-transform: lowercase;
  text-decoration: none;
}

#postamble a.source-link:hover,
#postamble a.source-link:active {
  background-color: transparent;
  color: #0f0;
}

code {
  border-top: solid #000 1px;
  border-bottom: solid #000 1px;
  padding: 0 .2em;
}

pre.src, pre.example {
  background-color: #111;
  border-top: none;
  border-bottom: solid #000 1px;
  border-left: none;
  border-right: solid #000 1px;
  box-shadow: none;
  font-size: .9em;
  padding: 1em 2em;
  overflow: auto;
}

pre.src:before {
  background-color: transparent;
  border: none;
  top: 0;
  right: 0;
}

sup {
  line-height: 0;
}

hr {
  border-top: solid 1px #000;
  border-bottom: solid 1px #333;
}

li p {
  margin: 0;
}

.footpara {
  margin: 0;
}

.footnotes {
  margin-top: 1em;
}

h2, h3, h4, h5, h6,
.footnotes {
  margin: 12px auto;
}

p, ul {
  margin: 24px auto;
}

table {
  margin: 12px auto;
}

li ul {
  margin-top: 0;
  margin-bottom: 0;
}

pre {
  margin: 0 auto;
}

div.figure {
  text-align: center;
}

div.figure p, div.figure img {
}

span.tag {
  background-color: #333;
}

.done {
  color: green;
  font-weight: bold;
}
.todo {
  color: red;
  font-weight: bold;
}

.footnotes {
  font-size: 14px;
  line-height: 24px;
  margin-top: 24px;
  padding: 24px;
}

.footdef {
  margin-top: 24px;
}

.footpara {
  display: inline;
}
</style>
")
