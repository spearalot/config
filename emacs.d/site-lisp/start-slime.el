(set-language-environment "utf-8")

(add-to-list 'load-path "~/.emacs.d/slime/")

;;; CCL
;;; Note that if you save a heap image, the character
;;; encoding specified on the command line will be preserved,
;;; and you won't have to specify the -K utf-8 any more.
;;; (setq inferior-lisp-program "~/Library/UNIX/bin/ccl -K utf-8")

;;; SBCL
(setq inferior-lisp-program "~/Library/UNIX/bin/sbcl")

(setq common-lisp-hyperspec-root
      "file:/Users/martin/Documents/CS/Lisp/HyperSpec/")

;;; W3M
(setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)))

(require 'slime)

;;; CCL
;;;(setq slime-net-coding-system 'utf-8-unix)

(slime-setup '(slime-fancy))
