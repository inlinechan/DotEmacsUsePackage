;;; hc-general.el --- basic settings

;;; Commentary:

;;; Code:

(setq-default transient-mark-mode t
              make-backup-files nil
              fill-column 80
              default-major-mode 'text-mode

              ;;    If `gdb-many-windows' is non-`nil', then `M-x gdb' displays the
              ;; following frame layout:
              ;;      +--------------------------------+--------------------------------+
              ;;      |   GUD buffer (I/O of GDB)      |   Locals/Registers buffer      |
              ;;      |--------------------------------+--------------------------------+
              ;;      |   Primary Source buffer        |   I/O buffer for debugged pgm  |
              ;;      |--------------------------------+--------------------------------+
              ;;      |   Stack buffer                 |   Breakpoints/Threads buffer   |
              ;;      +--------------------------------+--------------------------------+
              gdb-many-windows t

              ;; notab by default
              indent-tabs-mode nil
              c-basic-offset 4
              default-tab-width 4
              tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parenthesis matching
;; http://www.emacswiki.org/cgi-bin/wiki/parenthesismatching
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace.
ARG."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace
;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
(require 'saveplace)
(save-place-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff
;; http://www.emacswiki.org/emacs/EdiffMode
(setq-default ediff-split-window-function (lambda (&optional arg)
                                            (if (> (frame-width) 150)
                                                (split-window-horizontally arg)
                                              (split-window-vertically arg))))
;; ediff marked file
(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (let (marked-files (dired-get-marked-files))
  ;; (set 'marked-files (dired-get-marked-files))
    (when (= (safe-length marked-files) 2)
      (ediff-files (nth 0 marked-files) (nth 1 marked-files)))

    (when (= (safe-length marked-files) 3)
      (ediff3 (buffer-file-name (nth 0 marked-files))
              (buffer-file-name (nth 1 marked-files))
              (buffer-file-name (nth 2 marked-files))))))

;; getting git root dir
;; http://blog.uberweiss.net/2009/11/scoping-emacs-to-a-git-root-directory.html
(defun my-git-root ()
  "Find .git directory from current dir."
  (if buffer-file-name
      (let* ((current-directory (file-name-directory buffer-file-name))
             (git-directory (concat current-directory ".git")))
        (while (and
                current-directory
                (not (file-exists-p git-directory)))
          (setq current-directory (file-name-directory (substring current-directory 0 -1)))
          (setq git-directory (concat current-directory ".git")))
        current-directory)))

;; Go to the line of the file easily especially in gdb call stack.
;; /etc/passwd:10
(defun find-file-at-point-with-line()
  "If file has an attached line num goto that line, ie boom.rb:12."
  (interactive)
  (let ((line-num 0))
    (save-excursion
      (search-forward-regexp "[^ ]:" (point-max) t)
      (if (looking-at "[0-9]+")
          (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
    (find-file-at-point)
    (if (not (equal line-num 0))
        (forward-line line-num))))

(global-set-key (kbd "C-<return>") 'find-file-at-point-with-line)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq-default x-select-enable-clipboard t)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
;; (global-auto-revert-mode 1)

;; http://www.emacswiki.org/emacs/SearchAtPoint
;; http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
;; I-search with initial contents
(defvar isearch-initial-string nil)
(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))
(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun other-window-prev (&optional step)
  "Go to other window to opposite direction.
STEP ignored for now."
  (interactive "P")
  (setq step -1)
  (other-window step))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
(require 'recentf)

;; enable recent files mode.
(recentf-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notab


(defun hc/add-styles ()
  "Add c/c++ styles."
  (require 'google-c-style)

  (c-add-style "hc" '("google"
                      (c-basic-offset . 4)
                      (c-offsets-alist . ((substatement-open 0)
                                          (arglist-cont-nonempty . +)
                                          ))))

  (c-add-style "webkit" '("google"
                          (c-basic-offset . 4)
                          (c-offsets-alist . ((innamespace . 0)
                                              (access-label . -)
                                              (case-label . 0)
                                              (member-init-intro . +)
                                              (statement-cont . +)
                                              (arglist-cont-nonempty . +)
                                              (topmost-intro . 0)))))

  ;; https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/emacs.el
  (c-add-style "llvm.org"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)))))
  (c-add-style "qt-gnu"
               '("gnu"
                 (c-access-key .
                               "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
                 (c-basic-offset . 4)))

  (c-add-style "weston"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 4)
                 (c-basic-offset . 4)
                 (tab-width . 4)
                 (indent-tabs-mode . t)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)))))
)

(defun hc/decide-c-mode-style ()
  "Decide which style to be used."
  (when (buffer-file-name)
    (cond
     ((or (string-match "WebCore" buffer-file-name)
          (string-match "JavaScriptCore" buffer-file-name)
          (string-match "WebKit" buffer-file-name)
          (string-match "WebKit2" buffer-file-name)
          (string-match "wam" buffer-file-name)
          (string-match "blink" buffer-file-name))
      (progn
        ;; (message "%s" "hc/maybe-webkit-style")
        (c-set-style "webkit")))
     ((or (string-match "chromium" buffer-file-name)
          (string-match "v8" buffer-file-name))
      (progn
        ;; (message "%s" "hc/maybe-google-style")
        (c-set-style "google")))
     ((or (string-match "glib" buffer-file-name)
          (string-match "gtk" buffer-file-name))
      (c-set-style "gnu"))
     ((or (string-match "llvm" buffer-file-name)
          (string-match "clang" buffer-file-name))
      (c-set-style "llvm.org"))
     ((or (string-match "luna-surfacemanager" buffer-file-name)
          (string-match "[Qq][Tt]" buffer-file-name)
          (string-match "qml" buffer-file-name))
      (c-set-style "qt-gnu"))
     ((or (string-match "weston" buffer-file-name)
          (string-match "wayland" buffer-file-name))
      (c-set-style "weston"))
     (t
      (c-set-style "hc")))))

(add-hook 'c-mode-common-hook 'hc/add-styles)
(add-hook 'c-mode-common-hook 'hc/decide-c-mode-style t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hook
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; css-mode
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;; makefile-mode
(setq auto-mode-alist
      (append
       '(
         ;; makefile
         ("makefile\\."  . makefile-mode)
         ("Makefile\\.*" . makefile-mode)
         ("\\.mak$"      . makefile-mode)
         ("\\.min$"      . makefile-mode)
         ("Android.mk$"  . makefile-mode)
         ;;  perl
         ("\\.\\([pp][llm]\\|al\\)\\'" . cperl-mode)
         ;; gyp
         ("\\.gypi?$"    . python-mode)
         ;; bitbake
         ("\\.\\(bb\\|bbappend\\|bbclass\\|inc\\)$"       . python-mode)
         )
       auto-mode-alist))

;; http://stackoverflow.com/a/3346308
(defun c-c++-header ()
  "Set either `c-mode' or `c++-mode', whichever is appropriate for header."
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; perl mode
(setq interpreter-mode-alist
      (append
       '(("perl"     . cperl-mode)
         ("perl5"    . cperl-mode)
         ("miniperl" . cperl-mode)) interpreter-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-function-mode
;; http://www.emacswiki.org/emacs/WhichFuncMode#WhichFunctionMode
(which-function-mode)
(defun which-func-update ()
  "Update the Which-Function mode display for all windows."
  (walk-windows 'which-func-update-1 nil 'visible))

;; which-func-mode in the header line
(let ((which-func '(which-function-mode (which-func-mode (#1="" which-func-format " ")))))
  (setq mode-line-misc-info (delete (assoc 'which-function-mode
                                           mode-line-misc-info) mode-line-misc-info))
  (setq-default header-line-format which-func))

;; http://stackoverflow.com/a/3669681/2229134
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; (dolist (mode (list
;;                'c++-mode
;;                'c-mode
;;                'java-mode
;;                'js2-mode
;;                'lisp-mode
;;                ;; 'python-mode
;;                'sh-mode))
;;   (add-to-list 'which-func-modes mode))

(dolist (mode (list
               ;; 'c++-mode-hook      ;; wrong detection with starting || statement
               ;; 'c-mode-hook
               ;; 'cperl-mode-hook
               ;; 'css-mode-hook
               ;; 'emacs-lisp-mode-hook
               'git-commit-mode-hook
               ;; 'java-mode-hook
               ;; 'js2-mode-hook
               ;; 'perl-mode-hook
               ;; 'python-mode-hook
               ;; 'sh-mode-hook
               'text-mode-hook
               ))
  (add-hook mode 'turn-on-orgtbl))

(setq magic-mode-alist
      (append
       '(("#!/usr/bin/expect"     . tcl-mode))
       magic-mode-alist))

;; Do not use new frame while run ediff
;; http://stackoverflow.com/a/1680825
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

(when (file-exists-p "~/.local/bin")
  (setq exec-path (append exec-path '("~/.local/bin"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update which-function-mode
;; http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
(defun rescan-imenu ()
  (imenu--menubar-select imenu--rescan-item))

(add-hook 'after-save-hook 'rescan-imenu)

(defvar rescan-timer nil)

(defun rescan-callback ()
  (when (buffer-modified-p)
    (rescan-imenu)))

(defun rescan-start ()
  (interactive)
  (when (timerp rescan-timer)
    (cancel-timer rescan-timer))
  (setq rescan-timer
          (run-with-timer 1 5 #'rescan-callback)))

(dolist (mode (list
               'c++-mode-hook
               'c-mode-hook
               'emacs-lisp-mode
               'js2-mode
               'python-mode
               'sh-mode))
  (add-hook mode 'rescan-start))

;; Load sandbox if exist
(require 'sandbox nil t)

;; let woman uses wider width
(setq woman-fill-frame t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch among impl and headers
(defun impl-p (base other pp ext)
  (string-match ext "^\\(cc\\|cpp\\|c\\)$"))

(defun header-p (base other pp ext)
  (string-match ext "^\\(h\\|hpp)$"))

(defun has-no-p-p (base other pp ext)
  (string-equal pp ""))

(defun has-single-p-p (base other pp ext)
  (string-equal pp "_p"))

(defun has-double-p-p (base other pp ext)
  (string-equal pp "_p_p"))

(defun every-p (pred arg)
  (if (listp pred)
      (let ((result t)
            (res nil))
        (dolist (p pred)
          (setq result (and result (apply (indirect-function p) arg))))
        result)
    (apply (function pred) arg)))

(defun hc-get-next-match (current &optional table)
  (when (null table)
    (setq table
          '(((impl-p) (".h" ".hpp" "_p.h" "_p_p.h"))
            ((header-p has-no-p-p) ("_p.h" "_p_p.h" ".cpp" ".cc" ".c"))
            ((header-p has-single-p-p) ("_p_p.h" ".cpp" ".cc" ".c"))
            ((header-p has-double-p-p) (".cpp" ".cc" ".c")))))
  (let ((result ()))
    (dolist (row table)
      (string-match "^\\(.*?\\)\\(\\(\\(_p\\)*\\).\\([^.]*\\)\\)$" current)
      (let ((preds (car row))
            (cands (car (cdr row)))
            (base (match-string 1 current))
            (other (match-string 2 current))
            (pp (match-string 3 current))
            (ext (match-string 5 current)))
        ;; (message "hc-get-next-match: %s %s %s %s" base other pp ext)
        (when (every-p preds (list base other pp ext))
          (dolist (ext cands)
            (let ((cand (concat base ext)))
              (and (file-exists-p cand)
                   (if (null result)
                       (setq result (list cand))
                     (add-to-list 'result cand))))))))
    (car (nreverse result))))

(defun hc-switch-cc-to-h ()
  (interactive)
  (let ((next (hc-get-next-match buffer-file-name)))
    (when next
      (find-file next))))

(defun hc-switch-header-impl()
  (local-set-key (kbd "M-p") 'hc-switch-cc-to-h)
  (local-set-key (kbd "C-M-p") 'hc-switch-cc-to-h))

(dolist (mode (list
               'c++-mode-hook
               'c-mode-hook))
  (add-hook mode 'hc-switch-header-impl))

(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(add-hook 'python-mode-hook #'(lambda ()
                                (when (string-match "gypi?$" buffer-file-name)
                                  (make-local-variable 'python-indent)
                                  (setq python-indent 2))))

;; (let ((org-jekyll-publish-path "~/Documents/org-jekyll/lisp"))
;;   (when (file-directory-p org-jekyll-publish-path)
;;     (add-to-list 'load-path org-jekyll-publish-path)
;;     (require 'org-jekyll-publish nil t)))

;; If non-nil, Dired tries to guess a default target directory. AWESOME
;; (setq dired-dwim-target t)

;; http://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term/
(require 'term)

(defun jnm/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(define-key term-mode-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-mode-map (kbd "C-c C-k") 'jnm/term-toggle-mode)

(define-key term-raw-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-k") 'jnm/term-toggle-mode)


(defun hc/search-class-decl ()
  (interactive)

  ;; (let ((pattern "class ?[a-za-z0-9_]* ?[a-za-z0-9_]+"))
  ;; (let ((pattern "^\'*class ?\\([A-Z_0-9]+\\)? ?[A-Za-z0-9_]+"))
  (let ((pattern "^\'*class ?\\([A-Z_0-9]+\\)? ?[A-Za-z0-9_]+[^;][ :]*[A-Za-z0-9_ ]+{? *
"))
    (isearch-forward-regexp nil 1)
    (setq isearch-regexp t
          isearch-string pattern
          isearch-yank-flag t)
    (isearch-search-and-update)))

(dolist (mode (list
               'c-mode-common-hook
               'qml-mode-hook
               'js-mode-hook
               'python-mode-hook
               'emacs-lisp-mode))
  (add-hook mode (lambda ()
                   (setq show-trailing-whitespace t))))

(add-hook 'js2-mode-hook #'lambda()
          (setq js2-basic-offset 2))


(require 'vc-git)
(defun grep-find-on-git-root (command-args)
  "Run \\[grep-find] on git root, with user-specified args COMMAND-ARGS."
  (interactive
   (progn
     (let ((cmd "find . -type f -print0 | xargs -0 -P 10 grep --color -nH --null -e "))
       (grep-apply-setting 'grep-find-command (cons cmd (+ 1 (length cmd)))))
     (grep-compute-defaults)
     (if grep-find-command
         (list (read-shell-command "Run find (like this): "
                                   grep-find-command 'grep-find-history))
       ;; No default was set
       (read-string
        "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil))))
  (when command-args
    (let ((null-device nil))		; see grep
      (let* ((git-root (vc-git-root (or buffer-file-name default-directory)))
             (default-directory git-root))
        (grep command-args)))))

(defun dired-do-copy-dwim-target (&optional arg)
  "Do dired-do-copy while `dired-dwim-target' is not nil (with optional ARG)."
  (interactive)
  (if current-prefix-arg
      (let ((dired-dwim-target t))
        (dired-do-copy arg))
    (dired-do-copy arg)))

(require 'dired)
(define-key dired-mode-map (kbd "C") 'dired-do-copy-dwim-target)

(setq comment-style 'extra-line)

(defun hc/kill-some-magit-buffer()
  "Kill troublesome buffers."
  (interactive)
  (mapc (lambda (buffer)
          (let ((buf-name (buffer-name (get-buffer buffer)))
                (target-list '("magit: build-signage"
                               "magit-log: build-signage"
                               "magit-revision: build-signage"
                               "build-signage")))
            (dolist (target target-list)
              (when (string= target (buffer-name (get-buffer buffer)))
                ;; (message (buffer-name (get-buffer buffer)))
                (kill-buffer (get-buffer buffer))
                ))))
        (buffer-list)))

(provide 'hc-general)

;;; hc-general.el ends here
