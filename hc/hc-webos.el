;;; Package --- Summary

;;; Commentary:

;;; Code:

(require 'find-dired)

(defvar webos-top-patterns
  '("^\\(.*/build-[^/]*\\).*$" "^\\(.*/AGL\\).*$" "^/\\(.*/poky\\).*$")
  "The pattern to search top dir.")

(require 'cl-extra)

(defun webos-top (path)
  "Return wtop from PATH."
  (cl-some (apply-partially 'webos--top-internal path) webos-top-patterns))

(defun webos--top-internal (path top-pattern)
  "Return wtop from PATH with TOP-PATTERN."
  (let* ((case-fold-search nil)
         (parent-dir (file-name-directory (directory-file-name path))))
    (if (string-match top-pattern path)
        (match-string 1 path)
      nil)))

(defun webos-find-meta-directories (topdir)
  "Find meta layer diretories from TOPDIR."
  (when topdir
    (directory-files topdir 'full "^\\(.*\\(meta\\|oe-core\\)\\).*$")))

(defun webos-find-recipe-candidates ()
  "Find bitbake recipe candidates in the subdirectories recursively."
  (let* ((wtop (webos-top default-directory))
         (recipe-directories (mapconcat #'identity (webos-find-meta-directories wtop) " "))
         (buffer (get-buffer-create "*webos-recipe*"))
         ;; (find-command "find -E meta* -type f -regex \".*(bb|bbappend|bbclass)$\" ") ;; mac
         (find-command (concat "find " recipe-directories " -type f \\( "
                               "-name \\*.bb -o -name \\*.bbclass "
                               "-o -name \\*.bbappend -o -name \\*.inc"
                               " \\)"))
         (recipes nil))
    (when wtop
      (progn
        (with-current-buffer buffer
          (erase-buffer)
          (cd wtop)
          (when (= 0 (call-process-shell-command find-command nil buffer))
            (setq recipes (mapcar #'(lambda (path)
                                      (let ((parts (split-string path "/")))
                                        (last parts)))
                                  (split-string (buffer-string))))))
        (kill-buffer buffer)))
    recipes))

(defun webos-find-recipes (pattern)
  "Find bitbake recipes with matching PATTERN."
  (interactive
   (list (helm-comp-read "recipe: " (webos-find-recipe-candidates))))

  (let* ((buffer-name (concat "*Find*" " - " pattern))
         (wtop (webos-top default-directory))
         (recipe-buffer (get-buffer-create buffer-name))
         (find-command nil)
         (dir (expand-file-name wtop)))
    (if (and wtop (string-match wtop default-directory))
        (progn
          (switch-to-buffer (get-buffer-create buffer-name))

          (let ((find (get-buffer-process (current-buffer))))
            (when find
              (if (or (not (eq (process-status find) 'run))
                      (yes-or-no-p "A `find' process is running; kill it? "))
                  (condition-case nil
                      (progn
                        (interrupt-process find)
                        (sit-for 1)
                        (delete-process find))
                    (error nil))
                (error "Cannot have two processes in `%s' at once" (buffer-name)))))

          (widen)
          (kill-all-local-variables)
          (setq buffer-read-only nil)
          (erase-buffer)

          (cd wtop)
          (setq find-command (concat "find meta\* -type f -ls | egrep \"(.*/)"
                                     pattern
                                     "$\""))
          (shell-command find-command (current-buffer) "*Message*")

          (dired-mode dir (cdr find-ls-option))
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map (current-local-map))
            (define-key map "\C-c\C-k" 'kill-find)
            (define-key map "g" nil)
            (use-local-map map))
          (make-local-variable 'dired-sort-inhibit)

          (set (make-local-variable 'revert-buffer-function)
               `(lambda (ignore-auto noconfirm)
                  (find-dired ,dir ,find-args)))

          (if (fboundp 'dired-simple-subdir-alist)
              ;; will work even with nested dired format (dired-nstd.el,v 1.15
              ;; and later)
              (dired-simple-subdir-alist)
            ;; else we have an ancient tree dired (or classic dired, where
            ;; this does no harm)
            (set (make-local-variable 'dired-subdir-alist)
                 (list (cons default-directory (point-min-marker)))))
          (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches)
          (setq buffer-read-only nil)
          ;; Subdir headlerline must come first because the first marker in
          ;; subdir-alist points there.
          (insert "  " dir ":\n")
          ;; Make second line a ``find'' line in analogy to the ``total'' or
          ;; ``wildcard'' line.
          (insert "  " find-command "\n")
          (setq buffer-read-only t)
          ;; (let ((proc (get-buffer-process (current-buffer))))
          ;;   (set-process-filter proc (function find-dired-filter))
          ;;   (set-process-sentinel proc (function find-dired-sentinel))
          ;;   ;; Initialize the process marker; it is used by the filter.
          ;;   (move-marker (process-mark proc) 1 (current-buffer)))
          (setq mode-line-process '(":%s")))
      (message "Not in webos directory"))))

(defun webos-find-build-directories (topdir)
  "Find meta layer diretories from TOPDIR."
  (when topdir
    (car (directory-files topdir 'nil "^.*\\(\\(build\\|BUILD\\)\\)$"))))

(defun webos-cd-candidates ()
  "`find-file' to DIR."
  (let* ((wtop (webos-top default-directory))
         (buffer (get-buffer-create "*webos-cd*"))
         (dirs nil)
         (webos-builddir (webos-find-build-directories wtop))
         (command (concat "ls " webos-builddir "\*/work/\* | grep -v \":$\" | sed \"/^\s\*$/d\"")))
    (when wtop
      (with-current-buffer buffer
        (erase-buffer)
        (cd wtop)
        (when (= 0 (call-process-shell-command command nil buffer))
          (setq dirs (split-string (buffer-string)))))
      (kill-buffer buffer)
      dirs)))

(defun webos-find-module-directory (target)
  "Find module directory TARGET from webos-top."
  (let* ((wtop (webos-top default-directory))
         (webos-builddir (webos-find-build-directories wtop))
         (found nil))
    (dolist (build (directory-files wtop t))
      ;; (message "build: %s" build))))
      (setq case-fold-search nil)
      (when (and (file-directory-p build)
                 (string-match (concat "^" webos-builddir) (car (last (split-string build "/")))))
        (let ((work (concat build "/work")))
          ;; (message "work: %s" work))))))
          (dolist (arch (directory-files work t))
            (when (directory-files arch t)
              (dolist (module (directory-files arch t))
                (when (and (file-directory-p module)
                           (string-equal (car (last (split-string module "/"))) target))
                  (setq found module))))))))
    found))

(defun webos-cd (module)
  "`find-file' MODULE directory in webos."
  (interactive
   (list (helm-comp-read "module: " (webos-cd-candidates))))

  (let ((wtop (webos-top default-directory)))
    (if wtop
        (find-file (webos-find-module-directory module))
      (message "Not in webos directory"))))

(provide 'hc-webos)

;;; hc-webos ends here
