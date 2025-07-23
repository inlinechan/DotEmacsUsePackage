;;; init.el --- My own emacs configuration

;; Copyright (C) 2017 Hyungchan Kim

;; Author: Hyungchan Kim <inlinechan@gmail.com>
;; Keywords: lisp use-package

;;; Commentary:

;; `use-package'

;;; Code:

;; (add-to-list 'load-path "~/DotEmacsUsePackage_30/hc")
(add-to-list 'load-path (concat user-emacs-directory "hc"))

(require 'hc-general)
;; (require 'hc-general-key)
(require 'hc-korean)
(require 'hc-ui)
(require 'hc-shell)
(require 'js-beautify)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(setq package-check-signature nil)

;; first time only once then comment out again
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'use-package)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-when-compile
;;   (require 'use-package))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'auto-revert-mode)
  (diminish 'company-mode "Co")
  (diminish 'eldoc-mode)
  (diminish 'flycheck-mode "Fc"))

(use-package magit
  :ensure t
  :commands (magit-status magit-blame-addition)
  :bind (("C-c s" . magit-status)
         ("<f2> b" . magit-blame-addition)
         ("C-c g b" . magit-blame-addition)
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
  :disabled
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
         ;; ("\e*" . gtags-pop-stack)
         ;; ("\e." . hc/tag-from-here)
         ;; ("\e," . hc/find-rtag)
         ("\ep" . previous-line)
         ("\en" . next-line)
         ("p" . previous-line)
         ("n" . next-line)
         :map gtags-mode-map
         ;; ("\e*" . gtags-pop-stack)
         ;; ("\e." . hc/tag-from-here)
         ;; ("\e," . hc/find-rtag)
)
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
                             (setq-local js-indent-level 4))))

(use-package graphviz-dot-mode)

(use-package cmake-mode
  :custom
  (cmake-tab-width 4 "Number of columns to indent cmake blocks"))

(use-package qmake-mode
  :if (file-directory-p "~/.emacs.d/qmake-mode")
  :load-path "qmake-mode/")

(use-package hc-local
  :load-path "lisp/")

(use-package hc-keys
  :diminish hc-keys-minor-mode
  :load-path "lisp/")

(use-package python
  :config
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4)
  :bind (:map python-mode-map
              ("C->" . python-indent-shift-right)
              ("C-<" . python-indent-shift-left)
              ("M-*" . pop-tag-mark)))

(use-package flycheck-mypy
  :disabled
  :ensure t
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (add-to-list 'flycheck-checkers 'python-mypy))))

;; http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
(use-package pyenv-mode
  :disabled
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode))

(use-package pyenv-mode-auto
  :disabled
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (require 'pyenv-mode-auto))))

(use-package dockerfile-mode)

(use-package git-commit
  :disabled
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

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

;; https://robert.kra.hn/posts/rust-emacs-setup/
(use-package lsp-mode
  :ensure t
  :defer t
  :init
  ;; (setq lsp-keymap-prefix "s-l")
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; (add-hook 'lsp-after-open-hook (lambda ()
  ;;                                  (when (lsp-find-workspace 'rust-analyzer nil)
  ;;                                    (lsp-rust-analyzer-inlay-hints-mode))))
  (setq gc-cons-threshold 100000000
        lsp-idel-delay 0.500
        read-process-output-max (* 1024 1024))

  ;; (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  ;; (setq lsp-clients-clangd-executable "clangd-14")
  :hook ((python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (c++-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :custom
  ;; (lsp-auto-guess-root nil)
  ;; (lsp-clients-clangd-executable "clangd-17")
  (lsp-prefer-flymake nil)
  (lsp-ui-doc-position 'bottom)

  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-inlay-hints-mode t)
  (lsp-rust-analyzer-store-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/rust-analyzer")
  :bind (:map lsp-mode-map
              ("<f1>" . lsp-describe-thing-at-point))
  :commands lsp)

(use-package lsp-ui
  ;; :disabled
  :config
  ;; (setq lsp-ui-doc-use-webkit t)
  :commands lsp-ui-mode)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package hotfuzz
  :after vertico
  :ensure t
  :custom
  (completion-ignore-case t)
  :init
  (setq completion-styles '(hotfuzz)))

(use-package rust-mode
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(load "~/.emacs.d/lisp/custom.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(clang-format cmake-mode consult-org-roam copilot counsel diminish
                  dockerfile-mode flycheck-mypy git-commit google-c-style
                  hotfuzz iregister json-mode lsp-treemacs lsp-ui magit
                  modern-cpp-font-lock org-journal powerline qml-mode
                  register-quicknav rust-mode tern tern-auto-complete vertico
                  web-mode yasnippet yasnippet-snippets))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main"))))
