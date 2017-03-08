;;; gdict.el --- Show first result from google dictionary

;; Copyright (C) 2012 Hyungchan Kim

;; Author: Hyungchan Kim <inlinechan@gmail.com>
;; Keywords: lisp
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'pos-tip)
(require 'popup)

(defvar gdict-show-in-echo t
  "Enable showing resulting text in echo area.")

(defvar gdict-show-in-popup t
  "Enable showing resulting text in popup.")

(defvar gdict-show-in-tooltip nil
  "Enable showing resulting text in tooltip.")

(defvar gdict-tooltip-width 50
  "Enable showing resulting text in tooltip.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple usage

;; (gdict "dictionary")
;; (gdict "emacs")

(defun extract-text (array)
  "extract text from array"
    (cdr (assoc 'text (elt (cdr (assoc 'terms array)) 0))))

(defun find-first-entries (container)
  "Extract alist from google dictionary api's resulting alist"
  (setq alist (or 
             (assoc 'primaries container)
             (assoc 'webDefinitions container)))
  (and (arrayp (cdr alist)) (cdr (assoc 'entries (elt (cdr alist) 0)))))

(defun find-elmt (key value seq)
  "find element having key in list"
  (let ((length (length seq))
        (index 0)
        (found-alist nil))
    (while (and (< index length) (not found-alist))
      (setq alist (elt seq index))
      (setq type (assoc key alist))
      ;; (message "%d: %s %s" index type (cdr type))
      (if (and type (equal (cdr type) value))
          (setq found-alist alist))
      (setq index (1+ index)))
    (setq result found-alist)))

(defun gdict (&optional arg)
  "Search word in google dictionary and then return first result"
  ;; (interactive "sWord: "))
  (interactive (list
                (read-string (format "Word (%s): " (thing-at-point 'word))
                             nil nil (thing-at-point 'word))))
  (setq word (or arg word))
  ;; (setq url (concat (format "http://www.google.com/dictionary/json?callback=dict_api.callbacks.id100&q=%s" word) "&sl=en&tl=en&restrict=pr%2Cde&client=te" ))
  (setq url (format "http://www.google.com/dictionary/json?callback=dict_api.callbacks.id100&q=%s&sl=en&tl=en&restrict=pr%%2Cde&client=te" word))
  (url-retrieve url 'gdict-callback))

(defun gdict-callback (status)
  "Callback of url-retrieve which extract search result from json"
  ;; remove start
  (search-forward "query")
  (backward-word)
  (backward-char 2)
  (setq start-point (point))
  (kill-region (point-min) start-point)

  ;; ;; remove end
  (goto-char (point-max))
  (search-backward "}")
  (forward-char)
  (setq end-point (point))
  (kill-region end-point (point-max))

  ;; (message "%s" (buffer-string))
  (setq json-string (json-read-from-string (buffer-string)))

  (kill-buffer (current-buffer))

  (setq def (find-first-entries json-string))
  (setq text (extract-text (find-elmt 'type "meaning" def)))

  (switch-to-buffer (other-buffer (current-buffer) t))

  ;; will insert resulting text into current buffer
  ;; (insert text)

  (if gdict-show-in-echo
      (message "%s" text))

  ;; (if gdict-show-in-popup
  ;;     (popup-tip text))

  (if (and gdict-show-in-tooltip (display-graphic-p))
      (and
       (setq box-text (mapconcat 'identity (pos-tip-split-string text gdict-tooltip-width) "\n"))
       (pos-tip-show box-text))))

(provide 'gdict)

;;; gdict.el ends here

