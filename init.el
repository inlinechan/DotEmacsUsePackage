;;; init.el --- My own emacs configuration

;; Copyright (C) 2017 Hyungchan Kim

;; Author: Hyungchan Kim <inlinechan@gmail.com>
;; Keywords: lisp use-package

;;; Commentary:

;; `use-package'

;;; Code:

(add-to-list 'load-path "~/.emacs.d/hc")

(require 'hc-general)
(require 'hc-general-key)
(require 'hc-ido)
(require 'hc-korean)
(require 'hc-ui)
(require 'hc-shell)
(require 'js-beautify)
(require 'hc-webos)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package jade
  :commands jade-connect-to-chrome
  :ensure t)

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends)))

(use-package org
  :ensure t
  :config
  (require 'hc-org))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "javascript"
  :config
  (defun override-gtags-find-file-hook ()
    (local-set-key (kbd "C-c C-f") 'gtags-find-file)
    (local-set-key (kbd "<f5>") 'js2-mode-toggle-hide-functions))

  (add-hook 'js2-mode-hook 'override-gtags-find-file-hook)
  (setq js2-basic-offset 2)
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-c s" . magit-status)
  :config
  ;; https://github.com/magit/magit/issues/1743
  ;; use 2way ediff like we used to do in version 1.x
  (setq magit-ediff-dwim-show-on-hunks t)

  (when (and (not window-system) (string-match "1\\.4*" (magit-version)))
    (set-face-background 'magit-log-head-label-tags "Grey85")))

(use-package json-mode
  :ensure t
  :config
  (eval-after-load 'flycheck
    `(progn
       (require 'flycheck)
       (flycheck-add-mode 'json-jsonlint 'json-mode)
       (add-hook 'json-mode-hook 'flycheck-mode))))

(use-package bookmark+
  :ensure t)

(use-package clang-format
  :ensure t
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (when (fboundp 'clang-format)
                (define-key c++-mode-map (kbd "C-M-\\") 'clang-format)))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-gcc-language-standard "c++11"
                    flycheck-clang-language-standard "c++11"
                    flycheck-clang-warnings '("all" "extra" "no-c++11-extensions"))))

  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))

(use-package web-mode
  :ensure t
  :mode ("\\.jsx\\'" "\\.html$")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        s-indent-level 2)

  ;; Enable this if .js highlight like .jsx
  ;; (setq web-mode-content-types-alist
  ;;   '(("jsx" . "\\.js[x]?\\'")))
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1))))

(use-package tern
  :ensure t
  :config
  (use-package tern-auto-complete
    :ensure t
    :config
    (setq tern-ac-on-dot t)
    (tern-ac-setup))

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (not (tramp-tramp-file-p (buffer-file-name)))
                (tern-mode t)))))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style))

;; gtags
(use-package gtags
  :load-path "lisp/"
  :init
  (setq gtags-path-style 'relative
        gtags-ignore-case nil)
  :commands (gtags-mode gtags-find-file)
  :bind (("C-c C-f" . gtags-find-file)
         ("C-c g p" . gtags-find-pattern)
         :map gtags-select-mode-map
         ("q" . gtags-pop-stack)
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
      (message "No gtags index found"))))

(defun enable-gtags-mode ()
  (gtags-mode 1))

(dolist (mode (list 'c++-mode-hook
                    'c-mode-hook
                    'dired-mode-hook))
  (add-hook mode 'enable-gtags-mode))

(use-package jedi
  :ensure t
  :init
  (setq jedi:key-goto-definition (kbd "C-c ."))
  :pin melpa)

(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (jedi:ac-setup)
            (jedi:start-server)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tern-auto-complete yasnippet web-mode use-package tern markdown-mode magit json-mode jade google-c-style flycheck clang-format bookmark+))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
