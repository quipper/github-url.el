(defun github-url-decompose (file &optional partial)
  (cond
   ((file-directory-p (expand-file-name ".git" file)) (cons file partial))
   ((equal "/" file) nil)
   (t (github-url-decompose (directory-file-name (file-name-directory file))
                            (if partial 
                                (concat (file-name-nondirectory file) "/" partial)
                              (file-name-nondirectory file))))))

(defun github-url-get-origin (dir)
  (with-temp-buffer
    (insert-file-contents (expand-file-name ".git/config" dir))
    (save-match-data
      (search-forward "[remote \"origin\"]")
      (let ((end (save-excursion (re-search-forward "^\\[" nil 'to-end) (point))))
        (and (re-search-forward "url *= *git@github\\.com:\\(.*\\)\\.git\\>" end)
             (match-string 1))))))

(defun github-url (start end)
  (interactive "r")
  (let ((g (github-url-decompose default-directory))
        rootdir file origin url start-line end-line)
    (unless g
      (error "Not managed by git: %s" default-directory))
    (setq rootdir (car g)
          file (cdr g))
    (setq origin (github-url-get-origin rootdir))
    (setq start-line (line-number-at-pos start))
    (setq end-line (line-number-at-pos end))
    (setq url (format "http://github.com/%s/blob/master/%s#L%d-L%d"
                      origin file start-line end-line))
    (message "URL: %s" url)
    (kill-new url)))


    