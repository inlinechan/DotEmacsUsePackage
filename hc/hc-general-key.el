;;(global-set-key (kbd "C-c y") 'clipboard-yank)

(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "<f9>") 'compile)))

(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-c m") 'manual-entry)    ;; manpage
(global-set-key (kbd "M-]") 'goto-match-paren)  ;; goto matching parenthesis

;; find from current dir
(global-set-key (kbd "C-c C-g") 'find-name-dired)
;; ask dir to find before
(global-set-key (kbd "C-c C-h") 'find-grep-dired)
(global-set-key (kbd "C-c g g") 'grep-find)

(global-set-key (kbd "C-c C-k") 'isearch-forward-at-point)

(global-set-key (kbd "C-M-o") 'other-window)
(global-set-key (kbd "C-M-m") 'other-window-prev)

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(global-set-key (kbd "<f2> 0") 'shell)
(global-set-key (kbd "<f2> 9") 'visit-term-buffer) ; ansi-term
(global-set-key (kbd "<f2> h") 'grep-find-on-git-root)
(global-set-key (kbd "<f2> -") 'tramp-cleanup-this-connection)

(global-set-key (kbd "<f2> =") 'hc/kill-some-magit-buffer)

(global-set-key (kbd "<f2> ;") 'webos-find-recipes)
(global-set-key (kbd "<f2> '") 'webos-cd)

(set-register ?0 '(file . "~/.emacs.d/init.el"))
(set-register ?9 '(file . "~/.emacs.d/hc/hc-general.el"))
(set-register ?- '(file . "~/Documents/org/webos.org"))

;; Minimize current buffer's height
(global-set-key [f7] (kbd "C-x + C-u - 1 6 C-x ^"))

(provide 'hc-general-key)
