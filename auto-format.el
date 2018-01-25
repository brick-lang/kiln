;; Requires ocp-indent to be installed with opam
;; Run this file using:
;; emacs -q -batch **/*.ml -l [full path to kiln]/untabify.el

(load-file "~/.opam/4.06.0/share/emacs/site-lisp/ocp-indent.el")

; (if (< 1 (count-windows))
;     (delete-other-windows (selected-window)))
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when buffer-file-name  ; nil for *scratch* buffer
      (progn
        (message "Writing %s..." buffer-file-name)
        (untabify (point-min) (point-max))
        (ocp-indent-region (point-min) (point-max))
        (write-file buffer-file-name)))))
