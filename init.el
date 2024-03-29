;;; init.el --- My own emacs configuration

;; Copyright (C) 2017 Hyungchan Kim

;; Author: Hyungchan Kim <inlinechan@gmail.com>
;; Keywords: lisp use-package

;;; Commentary:

;; `use-package'

;;; Code:

(add-to-list 'load-path "~/.emacs.d/hc")

(require 'hc-general)
;; (require 'hc-general-key)
(require 'hc-korean)
(require 'hc-ui)
(require 'hc-shell)
(require 'js-beautify)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; first time only once then comment out again
;; (setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends)))

(use-package gnuplot
  :mode ("\\.dem\\'" . gnuplot-mode))

(use-package org
  :config
  (with-eval-after-load 'org
    (require 'hc-org)))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "javascript"
  :config
  (defun override-gtags-find-file-hook ()
    (local-set-key (kbd "C-c C-f") 'gtags-find-file)
    (local-set-key (kbd "<f5>") 'js2-mode-toggle-hide-functions))

  (add-hook 'js2-mode-hook 'override-gtags-find-file-hook)
  (add-hook 'js2-mode-hook (lambda ()
                             (setq js2-basic-offset 2
                                   js2-strict-missing-semi-warning nil
                                   js2-missing-semi-one-line-override t)))
  )

(use-package magit
  :commands (magit-status magit-blame-addition)
  :bind (("C-c s" . magit-status)
         ("<f2> b" . magit-blame-addition)
         ("C-x g" . magit-status))
  :config
  ;; https://github.com/magit/magit/issues/1743
  ;; use 2way ediff like we used to do in version 1.x
  (setq magit-ediff-dwim-show-on-hunks t)
  (add-hook 'magit-diff-mode-hook
            (lambda ()
              (define-key magit-diff-mode-map (kbd "C-j") 'magit-diff-visit-worktree-file)))

  (when (and (not window-system) (string-match "1\\.4*" (magit-version)))
    (set-face-background 'magit-log-head-label-tags "Grey85")))

(use-package json-mode
  :config
  (add-hook 'json-mode-hook (lambda ()
                              (setq js-indent-level 2)))
  (eval-after-load 'flycheck
    `(progn
       (require 'flycheck)
       (flycheck-add-mode 'json-jsonlint 'json-mode)
       (add-hook 'json-mode-hook 'flycheck-mode))))

(use-package clang-format
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (when (fboundp 'clang-format)
                (define-key c++-mode-map (kbd "C-M-\\") 'clang-format)))))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (add-to-list 'flycheck-disabled-checkers 'c/c++-cppcheck)
              (setq flycheck-gcc-language-standard "c++1z"
                    flycheck-clang-language-standard "c++1z"
                    flycheck-clang-warnings '("all" "extra" "no-c++1z-extensions"))))

  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))

(use-package web-mode
  :mode ("\\.jsx\\'" "\\.html\\'" "\\.css\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil)


  ;; To associate .js as web-mode, add this to lisp/hc-local.el
  ;; (add-to-list 'auto-mode-alist '("/home/hyungchan/source/redux/.*\\.js[x]?\\'" . web-mode))

  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'"))))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1))))

(use-package yasnippet-snippets)

(use-package tern
  :config
  (use-package tern-auto-complete
    :config
    (setq tern-ac-on-dot t)
    (tern-ac-setup))

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (not (tramp-tramp-file-p (buffer-file-name)))
                (tern-mode t)))))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style))

;; gtags
(use-package gtags
  :diminish gtags-mode
  :load-path "lisp/"
  :init
  (setq gtags-path-style 'relative
        gtags-ignore-case nil)
  ;; To fix up "Key sequence C-c g p starts with non-prefix"
  (global-set-key (kbd "C-c g") nil)
  :commands (gtags-mode gtags-find-file)
  :bind (("C-c C-f" . gtags-find-file)
         ("C-c g p" . gtags-find-pattern)
         ("C-c t" . hc/mktag)
         ("C-c u" . hc/gtags-update)
         :map gtags-select-mode-map
         ("q" . gtags-pop-stack)
         ("C-o" . gtags-select-tag-other-window)
         ("o" . gtags-select-tag-other-window)
         ("\eo" . gtags-select-tag-other-window-focus)
         ("\e*" . gtags-pop-stack)
         ("\e." . hc/tag-from-here)
         ("\e," . hc/find-rtag)
         ("\ep" . previous-line)
         ("\en" . next-line)
         ("p" . previous-line)
         ("n" . next-line)
         :map gtags-mode-map
         ("\e*" . gtags-pop-stack)
         ("\e." . hc/tag-from-here)
         ("\e," . hc/find-rtag))
  :config
  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (setq hl-line-face 'underline)
              (hl-line-mode 1)))

  (defun hc/tag-from-here ()
    (interactive)
    (if (gtags-get-rootpath)
        (if current-prefix-arg
            (gtags-find-tag)
          (gtags-find-tag-from-here))
      (message "No gtags index found")))

  (defun hc/find-rtag ()
    (interactive)
    (if (gtags-get-rootpath)
        (gtags-find-rtag)
      (message "No gtags index found")))

  (defun hc/mktag (dir)
    (interactive "Dmktag (directory): ")
    (let ((mktag-script "mktag")
          (buffer-name "*mktag*"))
      (and (executable-find mktag-script)
           (or (cd dir) (error "Fail to change directory to %s" dir))
           (start-process-shell-command mktag-script buffer-name mktag-script)
           (switch-to-buffer buffer-name))))

  (defun hc/mktag-webos (dir)
    (interactive "Dmktag-webos (directory): ")
    (let ((mktag-script "~/bin/webos/mktag_webos.py")
          (buffer-name "*mktag-webos*"))
      (and (executable-find mktag-script)
           (or (cd dir) (error "Fail to change directory to %s" dir))
           (start-process-shell-command mktag-script buffer-name
                                        (concat "python" " " mktag-script))
           (switch-to-buffer buffer-name))))

  (defun hc/gtags-update (dir)
    (interactive "Dglobal -u (directory): ")
    (let ((global-script "global")
          (buffer-name "*mktag*"))
      (and (executable-find global-script)
           (or (cd dir) (error "Fail to change directory to %s" dir))
           (let ((result
                  (benchmark-run 1
                    (call-process global-script nil buffer-name t "-u"))))
             (message "Took %3.0f ms in running `%s -u in %s'"
                      (* 1000.0 (car result)) global-script dir))))))

;; global scope begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enable-gtags-mode ()
  (diminish 'gtags-mode)
  (gtags-mode 1))

(dolist (mode (list 'c++-mode-hook
                    'c-mode-hook
                    'dired-mode-hook))
  (add-hook mode 'enable-gtags-mode))

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  ;; (setq ido-enable-flex-matching t)
  ;; (setq ido-use-faces nil)
  )

(use-package helm-ls-git
  :disabled
  :commands helm-browse-project
  :bind (("C-c C-l" . helm-browse-project)
         ("C-c g l" . helm-browse-project)))

(use-package helm-ag
  :disabled
  :commands helm-ag-project-root
  :bind ("<f2> g" . helm-ag-project-root))

(use-package swift-mode
  :disabled
  )

(use-package flycheck-swift
  :disabled
  (eval-after-load 'flycheck '(flycheck-swift-setup)))

(use-package tide
  :disabled
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package editorconfig
  :diminish ""
  :config
  (editorconfig-mode 1))

(use-package ng2-mode
  :config
  (setq typescript-indent-level 2)
  (setq tide-format-options '(:indentSize 2)))

(use-package go
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (add-hook 'before-save-hook 'gofmt-before-save)
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (local-set-key (kbd "M-*") 'pop-tag-mark)
                            (company-mode))))

(use-package company-go
  :after (go))

(use-package helm
  :disabled
  :commands webos-helm-find-1
  :config
  (setq helm-buffers-fuzzy-matching t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  :bind (("<f2> b" . helm-buffers-list)
         ("<f2> m" . helm-mini)))

(use-package ivy
  :ensure t
  :config
  (setq ivy-re-builders-alist '((swiper-isearch . ivy--regex-plus)
                                (ivy-switch-buffer . ivy--regex-fuzzy)
                                (t . ivy--regex-fuzzy)))
  :bind (("C-x b" . ivy-switch-buffer)))

(use-package counsel
  :ensure t
  :bind (("<f9> r" . counsel-recentf)
         ("<f2> l" . counsel-git)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)))

(use-package swiper
  :ensure t
  :bind (("C-M-r" . swiper-isearch-backward)))

(use-package hc-yocto
  :after (ivy)
  :config
  (setq ivy-re-builders-alist
        (append '((webos-find-recipes-2 . ivy--regex-plus)) ivy-re-builders-alist))
  :commands (webos-find-recipes webos-cd webos-meta)
  :bind (("<f2> ;" . webos-find-recipes-2)
         ("<f2> '" . webos-cd)
         ("<f2> [" . webos-meta))
  :load-path "hc/")

(use-package company-jedi
  :disabled
  :init
  (setq jedi:key-goto-definition (kbd "C-c ."))
  ;; :after (python)
  :config
  (defun my/python-mode-hook ()
    (jedi:setup)
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'my/python-mode-hook))

(use-package qml-mode
  :mode ("\\.qmltypes\\'" . qml-mode)
  :config
  (add-hook 'qml-mode-hook (lambda ()
                             (setq-local comment-start "// ")
                             (setq-local comment-end "")
                             (setq js-indent-level 4))))

(use-package graphviz-dot-mode)

(use-package cmake-mode
  :custom
  (cmake-tab-width 4 "Number of columns to indent cmake blocks"))

(use-package hc-local
  :load-path "lisp/")

(use-package qmake-mode
  :if (file-directory-p "~/.emacs.d/qmake-mode")
  :load-path "qmake-mode/")

(use-package hc-keys
  :diminish hc-keys-minor-mode
  :load-path "lisp/")

;; http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode))

(use-package pyenv-mode-auto
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (require 'pyenv-mode-auto))))

(use-package dockerfile-mode)

(use-package git-commit
  :after magit
  :config
  (setq git-commit-summary-max-length 72)
  (setq git-commit-known-pseudo-headers
        '("Signed-off-by"
          "Acked-by"
          "Modified-by"
          "Cc"
          "Suggested-by"
          "Reported-by"
          "Tested-by"
          "Reviewed-by"))
  (setq git-commit-style-convention-checks
        '(non-empty-second-line
          overlong-summary-line)))

(use-package powerline
  :config
  ;;;###autoload
  (defun powerline-default-theme-without-which-func ()
    "Setup the default mode-line."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" face0 'l)
                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size face0 'l))
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face0 'l))
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       ;; (when (and (boundp 'which-func-mode) which-func-mode)
                                       ;;   (powerline-raw which-func-format face0 'l))
                                       (powerline-raw " " face0)
                                       (funcall separator-left face0 face1)
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face1 'l))
                                       (powerline-major-mode face1 'l)
                                       (powerline-process face1)
                                       (powerline-minor-modes face1 'l)
                                       (powerline-narrow face1 'l)
                                       (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (powerline-vc face2 'r)
                                       (when (bound-and-true-p nyan-mode)
                                         (powerline-raw (list (nyan-create)) face2 'l))))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (funcall separator-right face2 face1)
                                       (unless window-system
                                         (powerline-raw (char-to-string #xe0a1) face1 'l))
                                       (powerline-raw "%4l" face1 'l)
                                       (powerline-raw ":" face1 'l)
                                       (powerline-raw "%3c" face1 'r)
                                       (funcall separator-right face1 face0)
                                       (powerline-raw " " face0)
                                       (powerline-raw "%6p" face0 'r)
                                       (when powerline-display-hud
                                         (powerline-hud face0 face2))
                                       (powerline-fill face0 0)
                                       )))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))
  (powerline-default-theme-without-which-func)
  (load "~/.emacs.d/lisp/custom-powerline.el"))

(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'auto-revert-mode)
  (diminish 'company-mode "Co")
  (diminish 'eldoc-mode)
  (diminish 'flycheck-mode "Fc"))

(use-package git-commit
  :after magit
  :config
  (setq git-commit-summary-max-length 72
        git-commit-known-pseudo-headers
        '("Signed-off-by"
          "Acked-by"
          "Modified-by"
          "Cc"
          "Suggested-by"
          "Reported-by"
          "Tested-by"
          "Reviewed-by"))
  (add-hook 'git-commit-mode-hook (lambda ()
                                    (setq fill-column 72)))
  (setq git-commit-style-convention-checks
        '(non-empty-second-line
          overlong-summary-line)))

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(defconst tramp-local-coding-commands
  `(
    ;; (b64 base64-encode-region base64-decode-region)
    (uu  tramp-uuencode-region uudecode-decode-region)
    (pack ,(format tramp-perl-pack "perl") ,(format tramp-perl-unpack "perl")))
  "List of local coding commands for inline transfer.
Each item is a list that looks like this:

\(FORMAT ENCODING DECODING)

FORMAT is  symbol describing the encoding/decoding format.  It can be
`b64' for base64 encoding, `uu' for uu encoding, or `pack' for simple packing.

ENCODING and DECODING can be strings, giving commands, or symbols,
giving functions.  If they are strings, then they can contain
the \"%s\" format specifier.  If that specifier is present, the input
file name will be put into the command line at that spot.  If the
specifier is not present, the input should be read from standard
input.

If they are functions, they will be called with two arguments, start
and end of region, and are expected to replace the region contents
with the encoded or decoded results, respectively.")


(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))


(load "~/.emacs.d/lisp/custom.el")
