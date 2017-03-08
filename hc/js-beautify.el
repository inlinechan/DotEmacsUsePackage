;;; js-beautify.el -- beautify some js code
;; From http://sethmason.com/2011/04/28/jsbeautify-in-emacs.html
;; Modified by Hyungchan Kim
(defgroup js-beautify nil
  "Use jsbeautify to beautify some js"
  :group 'editing)

(defcustom js-beautify-args "--jslint-happy --brace-style=end-expand --keep-array-indentation"
  "Arguments to pass to jsbeautify script"
  :type '(string)
  :group 'js-beautify)

;; Prerequiresite
;; js-beautify package for python is required - pip install jsbeautifier

(defun js-beautify ()
  "Beautify a region of javascript using the code from jsbeautify.org"
  (interactive)
  (let ((orig-point (point))
        (js-beautify-path (executable-find "js-beautify")))
    (if (not js-beautify-path)
        (message "js-beautify is not found.")
      (progn
        (unless (mark)
          (mark-defun))
        (shell-command-on-region (point)
                                 (mark)
                                 (concat js-beautify-path
                                         " "
                                         js-beautify-args)
                                 nil t)
        (goto-char orig-point)))))

(provide 'js-beautify)
;;; js-beautify.el ends here
