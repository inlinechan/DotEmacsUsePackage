;;; package --- Summary
;; http://doc.norang.ca/org-mode.html#OrgBabel
;;; Commentary:

;;; Code:

;; Standard key bindings
;; (global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; babel specific
(if (file-exists-p "/usr/share/ditaa/ditaa.jar")
    (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
  (setq org-ditaa-jar-path (expand-file-name "~/.emacs.d/babel/ditaa.jar")))

(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/babel/plantuml.jar"))

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         ;; (sh . t)
         ;; (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t)
         (C . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

(setq hc/org-todo-file "~/Documents/org/todo.org.gpg")
(setq org-agenda-files (list hc/org-todo-file))

(setq org-src-fontify-natively t)

(setq doc-view-conversion-refresh-interval 1)

;; Do not fold org-mode in ediff
;; http://web.archiveorange.com/archive/v/Fv8aAM6yHysyeOVrnWBE#uTlUCjbhZTHNL53
(add-hook 'ediff-prepare-buffer-hook 'f-ediff-prepare-buffer-hook-setup)
(defun f-ediff-prepare-buffer-hook-setup ()
  ;; specific modes
  (cond ((eq major-mode 'org-mode)
         (f-org-vis-mod-maximum))
        ;; room for more modes
        )
  ;; all modes
  (setq truncate-lines nil))
(defun f-org-vis-mod-maximum ()
  "Visibility: Show the most possible."
  (cond
   ((eq major-mode 'org-mode)
    (visible-mode 1)  ; default 0
    (setq truncate-lines nil)  ; no `org-startup-truncated' in hook
    (setq org-hide-leading-stars t))  ; default nil
   (t
    (message "ERR: not in Org mode")
    (ding))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)" "DEFERRED(x@)")))

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/Documents/org/todo.org.gpg" "Tasks")
         "* TODO %?\n  %i\n")
         ("p" "Personal" entry (file+headline "~/Documents/org/todo.org.gpg" "Personal")
         "* TODO %?\n  %i\n")
        ("c" "chromium_upgrade" entry (file+headline "~/Documents/org/todo.org.gpg" "CHROMIUM UPGRADE")
         "* TODO %?\n  %i\n")
        ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org.gpg")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("g" "Goldilocks" entry (file+headline "~/Documents/org/todo.org.gpg" "Goldilocks")
         "* TODO %?\n  %i\n")
        ("e" "Emacs" entry (file+headline "~/Documents/org/todo.org.gpg" "Emacs")
         "* TODO %?\n  %i\n")))

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 80)))

(define-key global-map "\C-cc" 'org-capture)

;; https://emacs.stackexchange.com/questions/7629/the-syntax-highlight-and-indentation-of-source-code-block-in-exported-html-file?rq=1
;; (setq org-html-htmlize-output-type 'css) ; default inline-css
;; (setq org-html-htmlize-output-type 'inline-css)
;; (setq org-html-htmlize-font-prefix "org-")

(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css if EXPORTER is html."
  (when (eq exporter 'html)
    (let ((my-pre-bg (face-background 'default))
          (my-pre-fg (face-foreground 'default)))
      (setq org-html-head-include-default-style nil
            org-html-head
            ;; (format "<style type=\"text/css\">\n pre.src { background-color: %s;}</style>\n" my-pre-bg)))))
            (format "<style type=\"text/css\">\n pre.src { background-color: %s; color: %s}</style>\n" my-pre-bg my-pre-fg)))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

(setq org-clock-idle-time 30)

(defvar hc/org-clock-clocking-in nil
  "Indicate it's between org-clock-in-prepare-hook and org-clock-in-hook.")

;; http://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/
;; state, last-state => org-state, org-last-state
(defun wicked/org-clock-in-if-starting ()
  "Clock in when the task is marked STARTED."
  (when (and (string= org-state "STARTED")
             (not (string= org-last-state org-state)))
    (when (not hc/org-clock-clocking-in)
      (org-clock-in))))

(add-hook 'org-after-todo-state-change-hook
          'wicked/org-clock-in-if-starting)

(defun wicked/org-clock-out-if-waiting ()
  "Clock out when the task is marked WAITING."
  (when (and (string= org-state "WAITING")
             (equal (marker-buffer org-clock-marker) (current-buffer))
             (< (point) org-clock-marker)
             (> (save-excursion (outline-next-heading) (point))
                org-clock-marker)
             (not (string= org-last-state org-state)))
    (org-clock-out)))

(add-hook 'org-after-todo-state-change-hook
          'wicked/org-clock-out-if-waiting)

(add-hook 'org-clock-in-prepare-hook
          #'(lambda ()
              (setq hc/org-clock-clocking-in t)))

(add-hook 'org-clock-in-hook
          #'(lambda ()
              (setq hc/org-clock-clocking-in nil)))

(setq org-clock-in-switch-to-state
      #'(lambda (state)
          (when (not (string= state "STARTED"))
            "STARTED")))

(setq org-clock-out-switch-to-state
      #'(lambda (state)
          (when (not (string= state "WAITING"))
            "WAITING")))

(global-set-key (kbd "<f12>") 'org-clock-goto)
(global-set-key (kbd "C-<f12>") 'org-clock-in)

(provide 'hc-org)
;;; hc-org ends here
