;; ------------------------
;; Useful utility functions
;; ------------------------
(defun rename-file-and-buffer ()
  "Renames the current buffer and the file it's visiting"
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
         (progn (rename-file name new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)))))))

(defun delete-file-and-buffer ()
  "Removes file connected to current buffer and kills buffer"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun latex-clean-table (start end)
  "Clean latex table combined with Math elements"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "|l" nil t) (replace-match "| >{$}l<{$} " nil t))
    (goto-char (point-min))
    (while (search-forward "\$\\backslash\$" nil t) (replace-match "" nil t))
    (goto-char (point-min))
    (while (search-forward "$<$" nil t) (replace-match "<" nil t))
    (goto-char (point-min))
    (while (search-forward "$>$" nil t) (replace-match "<" nil t))
    (goto-char (point-min))
    (while (search-forward "implies" nil t) (replace-match "\implies" nil t))
    (goto-char (point-min))
    (while (search-forward "\\^" nil t) (replace-match "^" nil t))
    ))

;;; It is the opposite of fill-paragraph  (Credits to Stefan Monnier)
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; For github issues
(defun sibi-four-space-indent (beginning end)
  (interactive "r")
  (if (use-region-p)
      (save-restriction
        (narrow-to-region beginning end)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^" nil t)
            (replace-match "    "))
          ))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (replace-match "    "))
      )))

(defun sibi-copy-file-path-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun sibi-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))
