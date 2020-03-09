;;; hc-local.el --- configuration before mature

;;; Commentary:

;;; Code:

;; Put your experimental things in here
;; then move them to init.el when becoming stable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline
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
