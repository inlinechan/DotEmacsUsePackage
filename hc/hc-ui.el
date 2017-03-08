;; no splash
(setq inhibit-startup-message t)

;; hide toolbar & menubar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; color theme
;; (setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
               (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; (add-to-list 'default-frame-alist
;;          '(font . "DejaVu Sans Mono-10"))

(when (and window-system (eq system-type 'darwin))
  (set-frame-size (selected-frame) 100 60)
  ;; (text-scale-increase 1)
  ;; (set-frame-font "Menlo-14")
  (set-frame-font "Ubuntu-Mono-14"))

(load-theme 'tango-dark)

(provide 'hc-ui)
