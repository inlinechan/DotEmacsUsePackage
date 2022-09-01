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
  (or (cl-some (apply-partially 'webos--top-internal path) webos-top-patterns)
      (webos-find-build-directory-in-heirarchy path)))

(defun webos--parent-directory (dir)
  "Parent directory from DIR until $HOME or / found."
  (let ((parent (directory-file-name dir)))
    (unless
        (or (equal (getenv "HOME") parent)
            (equal "/" parent))
      (file-name-directory parent))))

(defun webos-find-build-directory-in-heirarchy (path)
  "Search for build-directory pattern upwards through the directory hierarchy, starting from PATH."
  (let ((current-dir (file-name-directory path))
        (found nil))
    (dolist (file (directory-files current-dir))
      (dolist (pat webos-top-patterns)
        (let ((fullpath (concat current-dir file)))
          ;; (message "%s %s" pat fullpath)
          (if (and (file-directory-p fullpath)
                   (not (equal "." file))
                   (not (equal ".." file))
                   (string-match pat fullpath))
              (setq found fullpath)))))
    (if found
        found
      (webos-find-build-directory-in-heirarchy (webos--parent-directory current-dir)))))

(defun webos--top-internal (path top-pattern)
  "Return wtop from PATH with TOP-PATTERN."
  (let* ((case-fold-search nil))
    (when (string-match top-pattern path)
      (match-string 1 path))))

(defun webos-find-meta-directories (topdir)
  "Find meta layer diretories from TOPDIR."
  (when topdir
    (directory-files topdir nil "^\\(.*\\(meta\\|oe-core\\)\\).*$")))

(defun webos-find-build-directories (topdir)
  "Find meta layer diretories from TOPDIR."
  (when topdir
    (car (directory-files topdir 'nil "^.*\\(\\(build\\|BUILD\\)\\)$"))))

(defun webos-cd-candidates ()
  "`find-file' to DIR."
  (when (webos-top default-directory)
    ;; (message "top: %s" (webos-top default-directory))
    (let* ((wtop (webos-top default-directory))
           (buffer (get-buffer-create "*webos-cd*"))
           (dirs '("wtop"))
           (webos-builddir (webos-find-build-directories wtop))
           (command (concat "ls " webos-builddir "\*/work/\* | grep -v \":$\" | sed \"/^\s\*$/d\"")))
      (when wtop
        (with-current-buffer buffer
          (erase-buffer)
          (cd wtop)
          (when (= 0 (call-process-shell-command command nil buffer))
            (setq dirs (append dirs (split-string (buffer-string))))))
        (kill-buffer buffer)
        dirs))))

(defun webos-find-module-directory (target)
  "Find module directory TARGET from webos-top."
  (let* ((wtop (webos-top default-directory))
         (webos-builddir (webos-find-build-directories wtop))
         (case-fold-search nil)
         (found nil))
    (if (equal target "wtop")
        (setq found wtop)
      (dolist (build (directory-files wtop t))
        ;; (message "build: %s" build))))
        (when (and (file-directory-p build)
                   (string-match (concat "^" webos-builddir) (car (last (split-string build "/")))))
          (let ((work (concat build "/work")))
            ;; (message "work: %s" work))))))
            (dolist (arch (directory-files work t))
              (when (directory-files arch t)
                (dolist (module (directory-files arch t))
                  (when (and (file-directory-p module)
                             (string-equal (car (last (split-string module "/"))) target))
                    (setq found module)))))))))
    found))

(defun webos--sole-child (dir)
  "Return fullpath of sole child if any from DIR."
  (let ((children (delete ".." (delete "." (directory-files dir)))))
    (if (eq 1 (length children))
        (concat (file-name-as-directory dir) (car children))
      dir)))

(defun webos--git-child (dir)
  "Return fullpath of git directory if any from DIR."
  (let ((children (delete ".." (delete "." (directory-files dir))))
        (git-dir "git"))
    (if (cl-find-if (lambda (x) (string-equal git-dir x)) children)
        (concat (file-name-as-directory dir) git-dir)
      dir)))

;;;###autoload
(defun webos-cd (module)
  "`find-file' MODULE directory in webos."
  (interactive
   (list (ivy-completing-read "module: " (append (webos-cd-candidates) (webos-meta-candidates)))))

  (let* ((wtop (file-name-as-directory (webos-top default-directory)))
         (fullpath (concat (file-name-directory wtop) module)))
    (if wtop
        (if (file-directory-p fullpath)
            (find-file fullpath)
          (find-file (webos--git-child (webos--sole-child (webos-find-module-directory module)))))
      (error "Not in webos directory"))))

(defun webos-meta-candidates ()
  "Find meta layers from webos-top."
  (let* ((wtop (webos-top default-directory))
         (buffer (get-buffer-create "*webos-cd*"))
         (metas (directory-files wtop nil "\\(meta-\*\\|oe-core\\)")))
    metas))

;;;###autoload
(defun webos-meta (meta)
  "`find-file' META directory in webos."
  (interactive
   (list (ivy-completing-read "module: " (webos-meta-candidates))))

  (let ((wtop (webos-top default-directory)))
    (if wtop
        (find-file (concat (file-name-as-directory wtop) meta))
      (error "Not in webos directory"))))


(defun webos-find-recipe-candidates-2 ()
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
            (setq recipes (split-string (buffer-string))))))
      (kill-buffer buffer))
    recipes))

;;;###autoload
(defun webos-find-recipes-2 ()
  "Find bitbake recipes from wtop directory."
  (interactive)

  (let* ((choices (webos-find-recipe-candidates-2))
         (recipe (ivy-read "Recipe: " choices
                           :require-match t
                           :caller 'webos-find-recipes-2)))
    (let* ((wtop (file-name-as-directory (webos-top default-directory)))
           (fullpath (concat (file-name-directory wtop) recipe)))
      (if wtop
          (find-file fullpath))
        (message "Not in webos directory"))))

(provide 'hc-yocto)

;;; hc-yocto.el ends here
