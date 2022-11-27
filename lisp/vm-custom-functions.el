;;; lisp/vm-custom-functions.el -*- lexical-binding: t; -*-

;; My Helper functions
;; Open an Eshell in the current directory.
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name   (car (last (split-string parent "/" t)))))
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun split-and-follow-horizontally ()
  "Function splits the current window horizontally and jumps to
new window.

Normally Emacs split (horizontal or vertical) splits the
window but doesn't move to it. This doesn't make any sense. By
default, I am splitting because, I want to do something in the
new window.

These two functions split-and-follow-horizontally and
split-and-follow-vertically fix this issue. They also bind the
keys appropriately."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "Function splits the current window horizontally and jumps to
new window.

Normally Emacs split (horizontal or vertical) splits the window
but doesn't move to it. This doesn't make any sense. By
default, I am splitting because, I want to do something in the
new window.

These two functions split-and-follow-horizontally and
split-and-follow-vertically fix this issue. They also bind the
keys appropriately."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; My function to enable swapping from vertical split to horizontal split
(defun toggle-window-split ()
  "Toggle the vertical and horizontal split.

If the windows are split vertically, they will be replaced with a
horizontal split and if they are split horizontally, will be
replaced with a vertical split.

Works with exactly 2 windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Functions to insert date and time in a custom format
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-time-format-journal "%H:%M"
  "Format of date to insert with `insert-current-time' func. This
is specifically for adding the time stamp in the interstitial
journal. Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
                                        ;       (insert (let () (comment-start)))
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert "*")
  (insert (format-time-string current-time-format (current-time)))
  (insert "*")
  (insert " - ")
  )

(defun insert-current-time-for-journal ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert "*")
  (insert (format-time-string current-time-format-journal (current-time)))
  (insert "*")
  (insert " - ")
  )


(provide 'vm-custom-functions)
