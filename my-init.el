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

(defun gcs-put-buffer-in-window (dir)
  "Pop the current window's buffer off the window's buffer list
 and push it onto the buffer list of the window in direction DIR."
  (let* ((this-window (selected-window))
         (other-window (progn
                         (windmove-do-window-select dir)
                         (selected-window)))
         (this-buffer  (window-buffer this-window))
         (other-buffer (window-buffer other-window))
         (this-start   (window-start this-window))
         (other-start  (window-start other-window)))
    (set-window-buffer other-window this-buffer)
    (set-window-start  other-window this-start)
    (switch-to-prev-buffer this-window)))

(global-set-keys
 ;; Use [C-]s-[y, u, i, o] to resize windows
 "s-y"   (shrink-window-horizontally 5)
 "s-u"   (shrink-window 5)
 "s-i"   (enlarge-window 5)
 "s-o"   (enlarge-window-horizontally 5)

 ;; Xcode-like keybindings
                                        ;"s-O" gcs-find-file-dwim
 "C-s-<up>" ff-find-other-file

 ;; Use s-[h, j, k, l] for window navigation
 "s-h" windmove-left
 "s-l" windmove-right
 "s-k" windmove-up
 "s-j" windmove-down

 ;; Use s-[H, J, K, L] to swap windows
 "s-H" (gcs-put-buffer-in-window 'left)
 "s-J" (gcs-put-buffer-in-window 'down)
 "s-K" (gcs-put-buffer-in-window 'up)
 "s-L" (gcs-put-buffer-in-window 'right)

 ;; s-=, s--, and s-0 to adjust font size like in a browser
 "s-=" (text-scale-increase 1)
 "s--" (text-scale-increase -1)
 "s-0" (text-scale-adjust 0)
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

(def-gcs-move-line 'up 3)
(def-gcs-move-line 'down 3)

(gcs-define-evil-motion-key (read-kbd-macro "C-j") 'gcs-next-line-3)
(gcs-define-evil-motion-key (read-kbd-macro "C-k") 'gcs-previous-line-3)

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
    (scroll-down-line 20)
    (run-at-time 0.016 nil 'gcs-smooth-scroll-up (- n 20))))
(defun gcs-smooth-scroll-down (n)
  (when (> n 0)
    (scroll-up-line 20)
    (run-at-time 0.016 nil 'gcs-smooth-scroll-down (- n 15))))
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

(defun comment-dwim-line-or-toggle-term-mode (&optional arg)
  "Replacement for the comment-dwim command.
    If no region is selected and current line is not blank and we are not at the end of the line,
    then comment current line.
    Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line.
    Also, toggles between term-line-mode and term-char-mode in multi-term"
  (interactive "*P")
  (if (equal 'term-mode major-mode)
      (if (term-in-line-mode)
          (progn (term-char-mode) (message "CHAR MODE"))
        (term-line-mode) (message "LINE MODE"))

    (comment-normalize-vars)
    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (comment-dwim arg))))

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
  (mapcar
   (lambda (binding) (list (read-kbd-macro (first binding)) (second binding)))
   '(("q"   quit-window)
     ("k"   gcs-kill-buffer-command)
     ("s-k" delete-window)
     ("K"   gcs-kill-buffer-and-window)
     ("g"   magit-status)
     ("l"   magit-file-log)
     ("u"   undo-tree-visualize)
     ("a"   helm-projectile-ag)

     ("d"   helm-projectile-find-file)
     ("f"   spacemacs/helm-find-files)
     ;; ("F"   ido-find-alternate-file)
     ("s-f" gcs-show-in-finder)
     ("s-x" gcs-open-with-external-editor)

     ("w" save-buffer)
     ("W" write-file)
     ("b" ibuffer)
     ("v" helm-mini)
                                        ; ("V" ido-switch-buffer-other-frame)

     ("s-v" visual-line-mode)
     ("s-b" magit-blame-mode)

                                        ; ("c" gcs-compile)
     ("e" next-error)
     ("E" previous-error)
     ("r" eval-buffer)

     ("0" delete-window)
     ("7" delete-window)
     ("1" delete-other-windows)
     ("2" split-window-vertically)
     ("3" split-window-horizontally)
     ("4" balance-windows)

     ("<left>"  gcs-previous-buffer)
     ("<right>" gcs-next-buffer)
     ("\\"      comment-dwim-line-or-toggle-term-mode)
     ("s-t"     gcs-toggle-tab-width-setting))))

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
(setq key-chord-two-keys-delay 0.05)

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
(key-chord-define ibuffer-mode-map "jk" 'ibuffer-quit) ;TODO: why doesn't this work without the require?
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

                                        ; (key-chord-define-global " j" 'yas-expand)

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
