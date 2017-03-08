;; hangul configuration
;; (set-language-environment "korean")
(set-language-environment "UTF-8")
(setq default-input-method "korean-hangul")
;; (global-set-key (kbd "S-SPC") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)

(provide 'hc-korean)
