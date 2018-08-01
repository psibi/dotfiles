screen config setup:
  file.copy:
    - source: /home/sibi/github/dotfiles/.screenrc
    - name: /home/sibi/.screenrc
    - user: sibi

ghci config setup:
  file.copy:
    - source: /home/sibi/github/dotfiles/.ghci
    - name: /home/sibi/.ghci
    - user: sibi
    - mode: '0700'

set proper git config:
  cmd.run:
    - name: /home/sibi/github/dotfiles/sibiSpecific.sh
    - shell: '/bin/bash'
    - runas: sibi
    - env:
      - HOME: /home/sibi

{% for fname in ['.xmobarrc','.Xresources','.xsession'] %}
{{ fname }}:
  file.copy:
    - source: /home/sibi/github/dotfiles/{{ fname }}
    - name: /home/sibi/{{ fname }}
    - user: sibi
    - group: sibi
{% endfor %}

x monad setup:
  file.copy:
    - source: /home/sibi/github/dotfiles/.xmonad/
    - name: /home/sibi/.xmonad
    - user: sibi
    - makedirs: True

setup links:
  cmd.run:
    - names:
        - ln /home/sibi/github/dotfiles/.alias /home/sibi/.alias
        - ln /home/sibi/github/dotfiles/.global_ignore /home/sibi/.global_ignore
        - ln /home/sibi/github/dotfiles/.bashrc /home/sibi/.sibi_bashrc
    - runas: sibi
        
  file.append:
    - name: /home/sibi/.bashrc
    - text: source ~/.sibi_bashrc


