(require 'python) ;; from python.el (Don't use python-mode)

(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--colors=Linux")
(setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
(setq python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
(setq python-shell-completion-module-string-code  "';'.join(module_completion('''%s'''))\n")
(setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(add-hook 'python-mode-hook 'sibi-customization)

(setenv "PYTHONPATH" ".:..")

(defun sibi-customization ()
  (define-key python-mode-map (kbd "C-'") 'sibi-python-shell)
  (define-key python-mode-map (kbd "C-c C-l") 'sibi-python-load-buffer))

(defun sibi-python-load-buffer ()
  (interactive)
  (progn
    (setq cur-buf-name (buffer-name))
    (python-shell-switch-to-shell)
    (erase-buffer)
    (switch-to-buffer-other-window cur-buf-name)
    (python-shell-send-buffer)))

(defun sibi-python-shell ()
  (interactive)
  (progn
    (setq cur-buf-name (buffer-name))
    (python-shell-switch-to-shell)
    (switch-to-buffer-other-window cur-buf-name)))
