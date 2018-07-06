copy emacs config:
  file.copy:
    - source: /home/sibi/github/dotfiles/.emacs.d
    - name: /home/sibi/
    - makedirs: True
    - user: sibi
    - group: sibi
    - subdir: True
