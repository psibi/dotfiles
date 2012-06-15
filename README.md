Here you will find few of the configuration which I use in my Linux system. You can tailor it according it to your needs.

Files
------

.alias - Useful alias file of commonly used UNIX commands.
.global_ignore - Global git ignore rules.
apt-fast - Script For fast downloading
.emacs - Emacs configuration file

.alias
-------
In order to use .alias script, just clone that in your $HOME directory and place the following command in your ~/.bashrc file.
    	 source ~/.alias

.emacs
-------
For using the .emacs file, put that under your $HOME directory.

.global_ignore
---------------
For using the .global_ignore file, put that under your $HOME directory and issue the following command:
    	  git config --global core.excludesfile ~/.global_ignore

apt-fast
---------
For using apt-fast, put it under /usr/bin/ directory and make sure it is marked as executable. (chmod +x /usr/bin/apt-fast )

For setting up Emacs IDE:

1. Put .emacs and .emacs.d under your HOME directory.
2. Setup Rope
    	 cd ~/.emacs.d/rope-0.9.3/
    	 sudo python setup.py install
3. Setup Ropemode
    	 cd ~/.emacs.d/ropemode-0.1-rc2/
    	 sudo python setup.py install
4. Setup Pymacs
    	 cd ~/.emacs.d/vendor/pinard-Pymacs-016b0bc/
    	 make
    	 sudo make install
5. Install Ropemacs
    	   cd ~/.emacs.d/ropemacs/
    	   sudo python setup.py install
6. Setup Yasnippet
    	 cd ~/.emacs.d/
    	 ln -s ./vendor/capitaomorte-yasnippet-66e804d/snippets/ .
7. Setup Auto-completion
    	 cd ~/.emacs.d/vendor/auto-complete-1.3.1/
    	 sudo make byte-compile
8. Setup CEDET
    	 cd ~/.emacs.d/cedet-1.0.1/
    	 sudo make  (Just make sure you have installed install-info or texinfo in your system..)
9. Setup ECB
    	 Change the variable of EMACS and CEDET in the Makefile to point to correct location.(Already done)
    	 make
10. Setup Scala-mode
    	  cd ~/emacs.d/scala-mode/
    	  make


