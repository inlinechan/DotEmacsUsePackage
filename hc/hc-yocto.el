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


(defun helm-findutils-transformer-ignore-directory (candidates _source)
  (let (non-essential
        (default-directory (helm-default-directory)))
    (cl-loop for i in candidates
             for abs = (expand-file-name
                        (helm-aif (file-remote-p default-directory)
                            (concat it i) i))
             for type = (car (file-attributes abs))
             for disp = (if (and helm-ff-transformer-show-only-basename
                                 (not (string-match "[.]\\{1,2\\}$" i)))
                            (helm-basename abs) abs)
             when (and (not (file-directory-p abs))
                       (string-match "\\.\\(bb\\|bbappend\\|bbclass\\|inc\\)$" abs))
             collect (cond ((eq t type)
                            (cons (propertize disp 'face 'helm-ff-directory)
                                  abs))
                           ((stringp type)
                            (cons (propertize disp 'face 'helm-ff-symlink)
                                  abs))
                           (t (cons (propertize disp 'face 'helm-ff-file)
                                    abs))))))

(defvar helm-source-findutils-ignore-directory
  (helm-build-async-source "Find"
    :header-name (lambda (name)
                   (concat name " in [" (helm-default-directory) "]"))
    :candidates-process 'helm-find-shell-command-fn
    :filtered-candidate-transformer 'helm-findutils-transformer-ignore-directory
    :action-transformer 'helm-transform-file-load-el
    :persistent-action 'helm-ff-kill-or-find-buffer-fname
    :action 'helm-type-file-actions
    :help-message 'helm-generic-file-help-message
    :keymap helm-find-map
    :candidate-number-limit 9999
    :requires-pattern 3))


(defun webos-helm-find-1 (dir)
  (let ((default-directory (file-name-as-directory dir)))
    (helm :sources 'helm-source-findutils-ignore-directory
          :buffer "*helm find*"
          :ff-transformer-show-only-basename nil
          :webos-helm-findutils-transformer
          :case-fold-search helm-file-name-case-fold-search)))


;;;###autoload
(defun webos-find-recipes ()
  "Find bitbake recipes from wtop directory."
  (interactive)
  (let ((wtop (webos-top default-directory))
        (completion-ignored-extensions
         (append '("BUILD/" "buildhistory/" "__pycache__/" "scripts/" "cache/"
                   "build-template/" ".py" ".patch" "sstate-cache/" "downloads/"))))
    (if (not wtop)
        (error "Not in webos directory")
      (let ((ignore-dirs t))
        (webos-helm-find-1 wtop)))))

(defun webos-find-build-directories (topdir)
  "Find meta layer diretories from TOPDIR."
  (when topdir
    (car (directory-files topdir 'nil "^.*\\(\\(build\\|BUILD\\)\\)$"))))

(defun webos-cd-candidates ()
  "`find-file' to DIR."
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
      dirs)))

(defun webos-find-module-directory (target)
  "Find module directory TARGET from webos-top."
  (let* ((wtop (webos-top default-directory))
         (webos-builddir (webos-find-build-directories wtop))
         (found nil))
    (if (equal target "wtop")
        (setq found wtop)
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
                    (setq found module)))))))))
    found))

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
          (find-file (webos-find-module-directory module)))
      (message "Not in webos directory"))))

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
   ;; helm-comp-read
   (list (ivy-completing-read "module: " (webos-meta-candidates))))

  (let ((wtop (webos-top default-directory)))
    (if wtop
        (find-file (concat (file-name-as-directory wtop) meta))
      (message "Not in webos directory"))))


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
