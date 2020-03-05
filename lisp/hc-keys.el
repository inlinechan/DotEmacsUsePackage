;;; hc-keys.el --- my own key map

;;; Commentary:

;;; Code:


(defvar hc-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'isearch-forward-at-point)

    (define-key map (kbd "M-g") 'goto-line)
    (define-key map (kbd "M-]") 'goto-match-paren)
    (define-key map (kbd "C-c C-g") 'find-name-dired)
    (define-key map (kbd "C-c C-h") 'find-grep-dired)
    (define-key map (kbd "C-c g g") 'grep-find)

    (define-key map (kbd "C-M-o") 'other-window)
    ;; (define-key map (kbd "C-M-m") 'other-window-prev)
    (define-key map (kbd "C-x C-r") 'ido-recentf-open)

    (define-key map (kbd "<f2> 0") 'shell)
    (define-key map (kbd "<f2> 9") 'visit-term-buffer) ; ansi-term
    (define-key map (kbd "<f2> h") 'grep-find-on-git-root)
    (define-key map (kbd "<f2> -") 'tramp-cleanup-this-connection)

    (define-key map (kbd "<f2> =") 'hc/kill-some-magit-buffer)
    (define-key map (kbd "<f2> ]") 'frog-jump-buffer)

    (define-key map (kbd "<f9> s") 'magit-status)
    (define-key map (kbd "<f9> f") 'ido-find-file)

    (define-key map (kbd "<f5> -") (lambda () (interactive) (find-file "~/Documents/org/webos.org")))
    (define-key map (kbd "<f7>") (kbd "C-x + C-u - 1 6 C-x ^"))

    (define-key map [remap c-toggle-comment-style] 'isearch-forward-at-point)
    map)
  "A hc-keys-minor-mode keymap.")

(define-minor-mode hc-keys-minor-mode
  "A minor mode so that hc key settings override annoying major modes."
  :init-value t
  :lighter " hc-keys")

(hc-keys-minor-mode 1)

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (hc-keys-minor-mode 0)))

(set-register ?0 '(file . "~/.emacs.d/init.el"))
(set-register ?9 '(file . "~/.emacs.d/hc/hc-general.el"))
(set-register ?8 '(file . "~/.emacs.d/lisp/hc-local.el"))
(set-register ?- '(file . "~/Documents/org/webos.org"))

(provide 'hc-keys)

;;; hc-keys.el ends here
