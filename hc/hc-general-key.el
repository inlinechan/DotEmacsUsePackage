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
(global-set-key (kbd "<f2> 9") 'ansi-term)

(global-set-key (kbd "<f5> 0") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f5> 9") (lambda () (interactive) (find-file "~/.emacs.d/hc/hc-general.el")))
(global-set-key (kbd "<f5> 8") (lambda () (interactive) (find-file "~/Documents/org")))

;; Minimize current buffer's height
(global-set-key [f7] (kbd "C-x + C-u - 1 6 C-x ^"))

(provide 'hc-general-key)
