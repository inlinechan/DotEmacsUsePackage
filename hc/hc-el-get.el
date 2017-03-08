(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

(require 'compiler-version)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
       )
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defun minimal-p (arglist)
  "Check if has --minimal in the arglist"
  (let ((args arglist)
        (found nil))
    (dolist (arg args)
      (if (string-match "--minimal" arg)
          (setq found t)))
    found))

(eval-after-load 'el-get
  '(progn
     (setq my:el-get-packages-minimal
           '(bookmark+
             google-c-style
             js2-mode
             magit
             org-mode
             markdown-mode))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; el-get :: now set our own packages
     (setq
      my:el-get-packages
      '(
        bookmark+
        company-mode
        cmake-mode
        clang-format
        ;; css-mode-simple
        ;; flymake-cursor
        emmet-mode
        flycheck
        ;; highlight-chars
        ;; gdict
        ;; ggtags
        google-c-style
        ;; jade
        jquery-doc
        js2-mode
        ;; json - requirement
        ;; http://stackoverflow.com/a/39085903/2229134
        ;; sudo npm install jsonlint -g
        json-mode
        magit
        org-mode
        popup
        ;; popup-pos-tip
        pos-tip
        psvn
        qmake-mode
        ;; org-octopress
        orglue
        slime
        yasnippet
        ;; tern - requirement
        ;; $ apt-get install nodejs, npm
        ;; $ npm install acorn glob minimatch
        tern
        web-mode
        org-reveal
        multiple-cursors
        use-package
        ))

     ;; Optional packages
     ;; For example, emacs-w3m is available if w3m exist
     (setq mode-alist
           '(((executable-find "cvs")          . emacs-w3m)                 ; w3m
             ((executable-find "markdown")     . markdown-mode)             ; markdown
             ((executable-find "gnuplot")      . gnuplot-mode)              ; gnuplot
             ((executable-find "virtualenv")   . jedi)                      ; virtualenv
             ((executable-find "dot")          . graphviz-dot-mode)         ; graphviz
             ;; ((gcc-version-at-least "4.8")     . rtags)                     ; rtags
             ;; ((file-exists-p "/usr/share/info/python.info") . pydoc-info)   ; pydoc-info
             ))

     (dolist (item mode-alist)
       (let ((conds (car item))
             (mode (cdr item)))
         (when (apply 'funcall conds)
           (add-to-list 'my:el-get-packages mode))))

     (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

     (setq my:el-get-packages
           (append
            my:el-get-packages
            (loop for src in el-get-sources collect (el-get-source-name src))))

     ;; install new packages and init already installed packages
     (if (minimal-p command-line-args)
         (el-get 'sync my:el-get-packages-minimal)
       (el-get 'sync my:el-get-packages))))

(provide 'hc-el-get)
