screen config setup:
  file.copy:
    - source: /home/sibi/github/dotfiles/.screenrc
    - name: /home/sibi/.screenrc
    - preserve: True

ghci config setup:
  file.copy:
    - source: /home/sibi/github/dotfiles/.ghci
    - name: /home/sibi/.ghci
    - mode: '0700'

set proper git config:
  cmd.run:
    - name: /home/sibi/github/dotfiles/sibiSpecific.sh
    - shell: '/bin/bash'
    - runas: sibi
    - env:
      - HOME: /home/sibi

x server related setup:
  file.copy:
    - source: /home/sibi/github/dotfiles/
    - names:
        - /home/sibi/.xmobarrc
        - /home/sibi/.Xresources
        - /home/sibi/.xsession
    - preserve: True

x monad setup:
  file.copy:
    - source: /home/sibi/github/dotfiles/.xmonad/
    - name: /home/sibi/.xmonad/xmonad.hs
    - makedirs: True

setup links:
  cmd.run:
    - names:
        - ln /home/sibi/github/dotfiles/.alias /home/sibi/.alias
        - ln /home/sibi/github/dotfiles/.global_ignore /home/sibi/.global_ignore
        - ln /home/sibi/github/dotfiles/.bashrc /home/sibi/.sibi_bashrc
        
  file.append:
    - name: /home/sibi/.bashrc
    - text: source ~/.sibi_bashrc


