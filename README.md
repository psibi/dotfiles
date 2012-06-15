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
<pre>
cd ~/.emacs.d/rope-0.9.3/
sudo python setup.py install
</pre>
3. Setup Ropemode
<pre>
cd ~/.emacs.d/ropemode-0.1-rc2/
sudo python setup.py install
</pre>
4. Setup Pymacs
<pre>
cd ~/.emacs.d/vendor/pinard-Pymacs-016b0bc/
make
sudo make install
</pre>
5. Install Ropemacs
<pre>
cd ~/.emacs.d/ropemacs/
sudo python setup.py install
</pre>
6. Setup Yasnippet
<pre>
cd ~/.emacs.d/
ln -s ./vendor/capitaomorte-yasnippet-66e804d/snippets/ .
</pre>
7. Setup Auto-completion
<pre>
cd ~/.emacs.d/vendor/auto-complete-1.3.1/
sudo make byte-compile
</pre>
8. Setup CEDET
<pre>
cd ~/.emacs.d/cedet-1.0.1/
sudo make  (Just make sure you have installed install-info or texinfo in your system..)
</pre>
9. Setup ECB
<pre>
Change the variable of EMACS and CEDET in the Makefile to point to correct location.(Already done)
make
</pre>
10. Setup Scala-mode
<pre>
cd ~/emacs.d/scala-mode/
make
</pre>

