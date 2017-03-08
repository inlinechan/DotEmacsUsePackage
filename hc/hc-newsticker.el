(require 'newsticker)
(when (require 'w3m nil t)
  (setq newsticker-html-renderer 'w3m-region))
(setq newsticker-use-full-width t)
(setq newsticker-treeview-treewindow-width 20)

(setq newsticker-url-list
      (quote
       (
        ;; ("BBC News" "http://www.bbc.co.uk/syndication/feeds/news/ukfs_news/front_page/rss091.xml" nil nil nil)
        ;; ("The Inquirer" "http://www.theinquirer.net/inquirer.rss" nil nil nil)
        ("Chromium blog" "http://blog.chromium.org/feeds/posts/default" nil nil nil)
        ("Engadget" "http://www.engadget.com/rss.xml" nil nil nil)
        ("Phronix" "http://www.phoronix.com/rss.php" nil nil nil)
        ("Qt blog" "http://blog.qt.digia.com/feed/" nil nil nil)
        ("WebKit blog" "https://www.webkit.org/blog/feed/" nil nil nil)
        ("emacs help" "http://rss.gmane.org/gmane.emacs.help" nil nil nil)
        ("클리앙 새소식" "http://feeds.feedburner.com/Clien--news" nil nil nil)
        )))

(defun hc/start-newsticker ()
  "Start newsticker and show treeview"
  (interactive)
  (newsticker-start)
  (newsticker-treeview))

;; (global-set-key (kbd "C-c r") 'hc/start-newsticker)

(provide 'hc-newsticker)
