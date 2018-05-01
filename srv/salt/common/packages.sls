common_packages:
  pkg.installed:
    - pkgs:
      - emacs
      - fish
      - xclip
      - rxvt-unicode
      - xsel
      - xfce4-screenshooter
      - feh
      - screen

stack.deb:
  file.managed:
    - source: 'https://github.com/commercialhaskell/stack/releases/download/v1.7.1/stack-1.7.1-linux-x86_64-gmp4.tar.gz'
    - source_hash: 'sha256=f3f48aee5fbf2b6e862d2a25f7bc1e0193137d39bc028a01dc52c81bb6a26fe3'
    - mode: '0755'
    - name: /usr/bin/stack.tar.gz

install stack:
  cmd.run:
    - name: tar xzf /usr/bin/stack.tar.gz
    - require:
        - file: stack.deb
