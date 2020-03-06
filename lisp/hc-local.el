;;; hc-local.el --- my own key map

;;; Commentary:

;;; Code:

;; Put your experimental things in here
;; then move them to init.el when becoming stable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline
(use-package powerline
  :config
  (powerline-default-theme)
  (load "~/.emacs.d/lisp/custom-powerline.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/abo-abo/hydra/wiki/Emacs
(use-package hydra
  ;; (global-set-key "[" #'braket-hydra/body)
  :bind (("[" . braket-hydra/body))
  :config
  (defun braket-hydra-pre ()
    (insert "[")
    (let ((timer (timer-create)))
      (timer-set-time timer (timer-relative-time (current-time) 0.3))
      (timer-set-function timer 'hydra-keyboard-quit)
      (timer-activate timer)))

  (defhydra braket-hydra (:body-pre braket-hydra-pre
                                    :color blue
                                    :hint nil)
    ;; (zap-to-char -1 ?x)
    ("s" (progn (undo) (magit-status)))
    ("f" (progn (undo) (ido-find-file)))))

(provide 'hc-local)

;;; hc-local.el ends here
