(require 'js2-mode)
(require 'skewer-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'html-mode-hook 'turn-off-auto-fill)
(add-hook 'html-mode-hook 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)

(define-key skewer-mode-map (kbd "C-'") 'skewer-repl)

