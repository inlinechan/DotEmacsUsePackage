;; ;; Copyright (C) 2013 Hyungchan Kim

;; ;; Author: Hyungchan Kim <inlinechan@gmail.com>
;; ;; Keywords: lisp
;; ;; Version: 0.1

;; ;; This program is free software; you can redistribute it and/or modify
;; ;; it under the terms of the GNU General Public License as published by
;; ;; the Free Software Foundation, either version 3 of the License, or
;; ;; (at your option) any later version.

;; ;; This program is distributed in the hope that it will be useful,
;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; ;; GNU General Public License for more details.

;; ;; You should have received a copy of the GNU General Public License
;; ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ;;; Commentary:

;; ;;; Code:


;; ;; Added by Package.el.  This must come before configurations of
;; ;; installed packages.  Don't delete this line.  If you don't want it,
;; ;; just comment it out by adding a semicolon to the start of the line.
;; ;; You may delete these explanatory comments.
;; (require 'cl)                           ; common lisp goodies, loop

;; (add-to-list 'load-path "~/.emacs.d/lisp")
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (add-to-list 'load-path "~/.emacs.d/hc")

;; (require 'hc-el-get)
;; (require 'hc-local-packages)
;; (require 'hc-general)
;; (require 'hc-general-key)
;; (require 'hc-ido)
;; (require 'hc-korean)
;; (require 'hc-ui)
;; (require 'hc-shell)
;; ;; (require 'hc-cedet)
;; (require 'hc-newsticker)
;; (require 'hc-gnus)
;; (require 'hc-org)
;; (require 'js-beautify)
;; (require 'hc-webos)
;; ;; (require 'flycheck-virtualenv)

;; ;; (require 'package)
;; ;; (add-to-list 'package-archives
;; ;;              '("melpa" . "https://melpa.org/packages/") t)
;; ;; (package-initialize)
;; ;; (package-install 'jade)

;; ;;; init ends here
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(bmkp-last-as-first-bookmark-file "~/.emacs.bmk")
;;  '(package-selected-packages (quote (jade jade-mode "jade" "jade" seq)))
;;  '(safe-local-variable-values (quote ((python-shell-interpreter . "python3")))))



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
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends)))

(use-package org
  :ensure
  t
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
  :ensure t)

(use-package magit
  :ensure t
  :defer t
  :bind ("C-c s" . magit-status)
  :config
  ;; (require 'vc)
  ;; (remove-hook 'find-file-hooks 'vc-find-file-hook)

  ;; (global-set-key (kbd "C-c s") 'magit-status)

  ;; https://github.com/magit/magit/issues/1743
  ;; use 2way ediff like we used to do in version 1.x
  (setq magit-ediff-dwim-show-on-hunks t)

  (when (and (not window-system) (string-match "1\\.4*" (magit-version)))
    (set-face-background 'magit-log-head-label-tags "Grey85")))

(use-package json-mode
  :ensure t
  :defer t
  :config
  (eval-after-load 'flycheck
    `(progn
       (require 'flycheck)
       (flycheck-add-mode 'json-jsonlint 'json-mode)
       (add-hook 'json-mode-hook 'flycheck-mode))))

(use-package bookmark+
  :ensure t
  :defer t)

(use-package clang-format
  :ensure t
  :defer t
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (when (fboundp 'clang-format)
                (define-key c++-mode-map (kbd "C-M-\\") 'clang-format)))))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook
            #'(lambda ()
                (setq flycheck-gcc-language-standard "c++11"
                      flycheck-clang-language-standard "c++11"
                      flycheck-clang-warnings '("all" "extra" "no-c++11-extensions"))))

  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.jsx\\'" "\\.jsx$")
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

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
  :defer t
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook #'(lambda() (yas-minor-mode -1))))

(use-package tern
  :ensure t
  :defer t
  :config
  
  (use-package tern-auto-complete
    :ensure t
    :config
    (setq tern-ac-on-dot t)
    (tern-ac-setup))

  ;; (require 'tern-auto-complete)
  ;; (setq tern-ac-on-dot t)
  ;; (tern-ac-setup)

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (not (tramp-tramp-file-p (buffer-file-name)))
                (tern-mode t)))))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.md\\'")

(use-package google-c-style
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (google-c-style bookmark+ json-mode markdown-mode tern-auto-complete tern yasnippet web-mode flycheck clang-format magit use-package jade))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
