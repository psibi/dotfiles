(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete-1.3.1")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete-1.3.1/dict")
(ac-config-default)

(add-to-list 'load-path "~/.emacs.d/vendor")
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))