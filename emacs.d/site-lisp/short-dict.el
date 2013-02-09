
;; USAGE:
;; (require 'short-dict)
;; (short-dict-read "~/.emacs.d/short.dict")
;;
;; In the mode-hook add:
;; (short-dict-mode)
;;
;; To override major-mode bound keys use:
;; (define-key abc-mode-map (kbd "(") 'short-dict-insert)
;;
;; Author: Martin Carlson <spearalot@gmail.com>
;; Date: 2010-01-14
(provide 'short-dict)


;; Dictionary format: (("word" . "short"))
(defvar *short-dict* '())


;; Definition of a subword
(defvar *short-disable-chars* '(?: ?-))

;; Defines the short-mode map
(defvar short-dict-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "SPC") 'short-dict-insert)
    (define-key keymap (kbd "(") 'short-dict-insert)
    (define-key keymap (kbd ")") 'short-dict-insert)
    (define-key keymap (kbd "[") 'short-dict-insert)
    (define-key keymap (kbd "]") 'short-dict-insert)
    (define-key keymap (kbd "{") 'short-dict-insert)
    (define-key keymap (kbd "}") 'short-dict-insert)
    (define-key keymap (kbd "#") 'short-dict-insert)
    (define-key keymap (kbd ";") 'short-dict-insert)
    (define-key keymap (kbd ",") 'short-dict-insert)
    (define-key keymap (kbd ".") 'short-dict-insert)
    keymap)
  "Short dict mode map.")


;; Defines the minor-mode
;;;###autoload
(define-minor-mode short-dict-mode
  "Replaces words with their sort eq."
  nil " >_< " short-dict-mode-map)


;; Short dict insert
(defun short-dict-insert (&optional arg)
  (interactive "P")
  (let ((thing (bounds-of-thing-at-point 'symbol))
        (state (short-literal)))
    (when (and (consp thing)
               (>= (point) (cdr thing))
               (or (eq state 'atomic) (eq state nil))
               (not (member (char-before (car thing))
                            *short-disable-chars*)))
      (short-dict-lookup (car thing) (cdr thing))))
  (self-insert-command (prefix-numeric-value arg)))


;; Reads definitions from the inputted dictionary
(defun short-dict-read-interactive ()
  (interactive)
  (short-dict-read
   (read-file-name
    (format "Read dict file (default %s): "
            abbrev-file-name)
    nil abbrev-file-name t)))


;; Reads a file on the format:
;; word short\n
;; I.e.
;; state st\n
(defun short-dict-read (file)
  (interactive)
  (when (file-readable-p file)
    (setq *short-dict* '())
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((pos (bounds-of-thing-at-point 'line))
               (pair (delete "" (split-string
                                 (buffer-substring (car pos) (cdr pos))))))
            (add-to-list '*short-dict* (cons (car pair) (cadr pair))))
        (forward-line)))
    (message (concat "Read definitions from: " file))))


(defun short-dict-lookup (start end)
  (let ((word (buffer-substring start end)))
    (dolist (entry *short-dict* end)
      (let ((match (car entry)) (replacement (cdr entry)))
        (when (string-match (car entry) word)
          (goto-char start)
          (search-forward match nil t)
          (replace-match replacement))))
    (goto-char start)
    (forward-thing 'symbol)))


;; Returns 'atomic' 'string' 'comment' or nil
;; If (point) is within the abovementioned.
(defun short-literal ()
  (let ((state (syntax-ppss)))
    (cond
     ((eq (nth 3 state) ?') 'atomic)
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment)
     (t nil))))
