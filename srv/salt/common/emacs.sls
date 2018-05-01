copy emacs config:
  file.copy:
    - source: /home/sibi/github/dotfiles/.emacs.d/
    - name: /home/sibi/.emacs.d/
    - makedirs: True
    - preserve: True
    - subdir: True
