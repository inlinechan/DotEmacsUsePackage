;;; package --- Summary

;;; Commentary:

;;; Code:

(let ((base-dir "~/.emacs.d/el-get-init-files")
      (packages '(gdb-ui gud gtags)))
  (while packages
    (message "Loading %s..." (car packages))
    (require (car packages))
    (setq init-file-path (format "%s/init-%s.el" base-dir (car packages)))
    (and init-file-path
	 (file-exists-p init-file-path)
	 (load-file init-file-path)
     (el-get-byte-compile-file init-file-path))
    (setq packages (cdr packages))))

(provide 'hc-local-packages)
;;; hc-local-packages ends here
