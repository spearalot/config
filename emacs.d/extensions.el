
(defun dired-open-marked-files ()
  (interactive)
  (mapc 'find-file (dired-get-marked-files)))
