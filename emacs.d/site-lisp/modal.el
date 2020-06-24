(require 'cl-lib)
(require 'quail)
(require 'projectile)

(defgroup modal nil
  "A sensible modal editing scheme"
  :group  'editing
  :tag    "Modal"
  :prefix "modal-"
  :link   '(url-link :tag "GitHub" "https://github.com/spearalot/modal"))

(defcustom modal-excluded-modes nil
  "List of major modes for which `modal-mode' should not be activated."
  :tag  "Excluded Modes"
  :type '(repeat :tag "Major modes to exclude" symbol))

(defvar modal-mode-map (make-sparse-keymap))
(defvar modal--go-map (make-sparse-keymap))
(defvar modal--action-map (make-sparse-keymap))

(defun modal--keybindingp (b)
  "Return t if b is a keymapp."
  (and (not (memq b '(nil undefined)))
       (keymapp b)))

(defun modal--activate-go-mode (&optional n)
  (interactive "p")
  (if (eq n 1)
      (set-transient-map modal--go-map)
    (goto-line n)))

(defun modal--activate-projectile ()
  (interactive)
  (set-transient-map projectile-command-map))

(defun modal--activate-eglot ()
  (interactive)
  (set-transient-map eglot-mode-map))

(defun modal--activate-action-mode ()
  (interactive)
  (set-transient-map modal--action-map))

(defun modal--newline-below (&optional n)
  (interactive "p")
  (move-end-of-line 1)
  (dotimes (_ n)
    (electric-newline-and-maybe-indent))
  (modal-mode 0))

(defun modal--newline-above (&optional n)
  (interactive "p")
  (save-excursion
    (previous-line 1)
    (move-end-of-line 1)
    (dotimes (_ n)
      (electric-newline-and-maybe-indent n))
    (modal-mode 0)))

(defun modal--append (&optional n)
  (interactive "p")
  (end-of-line n)
  (modal-mode 0))

(defun modal--kill (&optional n)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char n)))

(defun modal--change (&optional n)
  (interactive "p")
  (modal--kill n)
  (modal-mode 0))

(defun modal--mark-word-forward (&optional n)
  (interactive "p")
  (let ((b (bounds-of-thing-at-point 'word)))
    (cond
     ((and b (= (point) (cdr b)))
      (forward-char 1)) ; Should probably skip all non-word chars here...
     ((and b (> (point) (car b)) (< (point) (cdr b)))
      (backward-word 1)))
    (set-mark-command nil)
    (forward-word n)))

(defun modal--mark-word-backwards (&optional n)
  (interactive "p")
  (let ((b (bounds-of-thing-at-point 'word)))
    (if (and b (< (point) (cdr b)))
	(forward-word 1))
    (set-mark-command nil)
    (backward-word n)))

(defun modal--mark-line (&optional n)
  (interactive "p")
  (beginning-of-line 1)
  (set-mark-command nil)
  (end-of-line n))

(defun modal--forwad (&optional n)
  (interactive "p")
  (set-mark-command nil)
  (forward-sexp n))

(defun modal--backward (&optional n)
  (interactive "p")
  (set-mark-command nil)
  (backward-sexp n))

(define-minor-mode modal-mode "Toggle the `modal-mode' minor mode." nil " |N| " modal-mode-map
  ;; Normal mode map
  (define-key modal-mode-map (kbd "h") 'backward-char)
  (define-key modal-mode-map (kbd "j") 'next-line)
  (define-key modal-mode-map (kbd "k") 'previous-line)
  (define-key modal-mode-map (kbd "l") 'forward-char)
  (define-key modal-mode-map (kbd "i") 'modal-mode)
  (define-key modal-mode-map (kbd "c") 'modal--change)
  (define-key modal-mode-map (kbd "o") 'modal--newline-below)
  (define-key modal-mode-map (kbd "O") 'modal--newline-above)  
  (define-key modal-mode-map (kbd "A") 'modal--append)
  (define-key modal-mode-map (kbd "d") 'modal--kill)
  (define-key modal-mode-map (kbd "w") 'modal--mark-word-forward)
  (define-key modal-mode-map (kbd "W") 'modal--mark-word-backwards)
  (define-key modal-mode-map (kbd "x") 'modal--mark-line)
  (define-key modal-mode-map (kbd "v") 'set-mark-command)
  (define-key modal-mode-map (kbd "V") 'keyboard-quit)
  (define-key modal-mode-map (kbd "y") 'kill-ring-save)
  (define-key modal-mode-map (kbd "p") 'yank)
  (define-key modal-mode-map (kbd "/") 'helm-swoop)
  (define-key modal-mode-map (kbd "?") 'helm-rg)
  (define-key modal-mode-map (kbd "g") 'modal--activate-go-mode)
  (define-key modal-mode-map (kbd "SPC") 'modal--activate-action-mode)
  ;; Digit Arguments
  (dotimes (n 10)
    (define-key modal-mode-map (kbd (number-to-string (- n 1))) 'digit-argument))
  ;; Go map
  (define-key modal--go-map (kbd "l") 'move-end-of-line)
  (define-key modal--go-map (kbd "h") 'move-beginning-of-line)
  (define-key modal--go-map (kbd "i") 'back-to-indentation)
  (define-key modal--go-map (kbd "g") 'beginning-of-buffer)
  (define-key modal--go-map (kbd "e") 'end-of-buffer)
  ;; Actions map
  (define-key modal--action-map (kbd "p") 'modal--activate-projectile)
  (define-key modal--action-map (kbd "l") 'modal--activate-eglot)
  ;; Default
  (define-key modal-mode-map [remap self-insert-command] 'ignore))

(defun modal--maybe-activate ()
  "Activate `modal-mode' if current buffer is not minibuffer or blacklisted."
  (unless (or (minibufferp)
              (member major-mode modal-excluded-modes))
    (modal-mode 1)))

(define-globalized-minor-mode modal-global-mode
  modal-mode
  modal--maybe-activate)

(defun modal--input-function-advice (fnc key)
  (funcall (if modal-mode #'list fnc) key))

(advice-add 'quail-input-method :around #'modal--input-function-advice)

(provide 'modal)
