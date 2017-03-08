(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; this setting will force Ido to always create a new buffer (in C-x b) if the name does not exist
(setq ido-create-new-buffer 'always)

(setq ido-file-extensions-order '(".cc" ".cpp" ".c" ".h" ".txt" ".org" ".html"))

(provide 'hc-ido)
