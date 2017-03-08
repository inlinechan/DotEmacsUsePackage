;;; compiler-version.el --- Lookup Compiler Version.
;; Author: Per Nordlöw

;; https://github.com/nordlow/elisp/blob/master/mine/compiler-version.el

;; (require 'power-utils)

;; conflict with the of flycheck
(defun string-trim-2 (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defvar cached-compiler-versions
  (make-hash-table :test 'equal)
  "Versions of Compilers")

(defun get-compiler-version (compiler)
  "Ask COMPILER for its version or nil if COMPILER executable is missing."
  (let* ((compiler (downcase compiler))
         (str (when (executable-find compiler)
                (cond ((string-match "gcc" compiler)
                       (string-trim-2 (shell-command-to-string (concat compiler " -dumpversion"))))
                      ((string-match "clang" compiler)
                       (let ((str (shell-command-to-string (concat compiler " --version"))))
                         (progn (string-match "clang version \\([0-9-\\.]+\\)" str)
                                (match-string 1 str))))
                      ((string-match "dmd" compiler)
                       (let ((str (shell-command-to-string compiler)))
                         (progn (string-match "DMD\\(32\\|64\\) D Compiler v\\([0-9\\.]+\\)" str)
                                (message (match-string 2 str))
                                (match-string 2 str))))))))
    (when str
      (car (last (split-string str "\n"))))))

(defun compiler-version (compiler &optional uncached)
  "Cached version of `get-compiler-version'."
  (or (and (not uncached)
           (gethash compiler cached-compiler-versions))
      (puthash compiler
               (get-compiler-version compiler)
               cached-compiler-versions)))
;; Use: (compiler-version "gcc")
;; Use: (compiler-version "GCC")
;; Use: (compiler-version "clang")
;; Use: (compiler-version "dmd" t)

(defun gcc-version (&optional compiler) (compiler-version (or compiler "gcc")))
(defun clang-version (&optional compiler) (compiler-version (or compiler "clang")))
(defun dmd-version (&optional compiler) (compiler-version (or compiler "dmd")))
;; Use: (gcc-version)
;; Use: (clang-version)
;; Use: (dmd-version)

;; https://github.com/nordlow/elisp/blob/master/mine/power-utils.el
(when t
  (when (not (boundp 'version<=))
    (defun version<= (v1 v2)
      "Return non-nil if V1 is less than or equal to V2."
      (or (version< v1 v2)
          (version= v1 v2))))

  (when (not (boundp 'version>))
    (defun version>  (v1 v2)
      "Return non-nil if V1 is greater than to V2."
      (not (version< v1 v2))))

  (when (not (boundp 'version>=))
    (defun version>= (v1 v2)
      "Return non-nil if V1 is greater than or equal to V2."
      (or (version> v1 v2)
          (version= v1 v2))))

  (when (require 'inversion nil t)
    (when (not (boundp 'inversion-<=))
      (defun inversion-<= (v1 v2)
        "Return non-nil if V1 is less than or equal to V2."
        (or (inversion-< v1 v2)
            (inversion-= v1 v2))))

    (when (not (boundp 'inversion->))
      (defun inversion-> (v1 v2)
        "Return non-nil if V1 is greater than to V2."
        (not (inversion-< v1 v2))))

    (when (not (boundp 'inversion->=))
      (defun inversion->= (v1 v2)
        "Return non-nil if V1 is greater than or equal to V2."
        (or (inversion-> v1 v2)
            (inversion-= v1 v2))))))

(defun compiler-version-at-least (version &optional compiler)
  "Return non-nil if GCC COMPILER has at least version VERSION-STRING."
  (let ((version-installed (compiler-version compiler)))
    (when version-installed
      (inversion->= (version-to-list version-installed)
                    (cond ((stringp version)
                           (version-to-list version))
                          ((numberp version)
                           (version-to-list (number-to-string version)))
                          ((listp version)
                           version))))))
;; Use: (compiler-version-at-least "4.7" "gcc")
;; Use: (compiler-version-at-least "4.8.1" "gcc")
;; Use: (compiler-version-at-least "4.8.2" "gcc")
;; Use: (compiler-version-at-least "3.4" "clang")
;; Use: (compiler-version-at-least "3.3" "clang")
;; Use: (compiler-version-at-least "3.2" "clang")
;; Use: (compiler-version-at-least "3.1" "clang")
;; Use: (compiler-version-at-least "3.5" "clang")
;; Use: (compiler-version-at-least "3.0" "clang")
;; Use: (compiler-version-at-least "2.067" "dmd")
;; Use: (compiler-version-at-least "2.068" "dmd")
;; Use: (compiler-version-at-least "2.069" "dmd")

(defun gcc-version-at-least (version &optional compiler) (compiler-version-at-least version (or compiler "gcc")))
(defun clang-version-at-least (version &optional compiler) (compiler-version-at-least version (or compiler "clang")))
(eval-when-compile
  (when (executable-find "gcc-4.7") (assert-nil (gcc-version-at-least "4.8" "gcc-4.7")))
  (when (executable-find "gcc-4.7") (assert-t   (gcc-version-at-least "4.7" "gcc-4.7")))
  (when (executable-find "gcc-4.7") (assert-nil (gcc-version-at-least 4.8 "gcc-4.7")))
  (when (executable-find "gcc-4.7") (assert-t   (gcc-version-at-least 4.7 "gcc-4.7"))))

(provide 'compiler-version)
