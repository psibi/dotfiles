# Devops script 
# Author: Sibi <sibi@psibi.in>

# Do initial cleanup on existing system

[ -f ~/.alias ] && rm ~/.alias
[ -f ~/.global_ignore ] && rm ~/.global_ignore

ln .alias ~/.alias
ln .global_ignore ~/.global_ignore

if [ ! -d ~/.emacs.d ]; then
    mkdir -v ~/.emacs.d
fi

[ -f ~/.emacs.d/init.el ] && rm ~/.emacs.d/init.el

cp -v init.el ~/.emacs.d/init.el

git config --global core.excludefile ~/.global_ignore

ln ./.bashrc ~/.sibi_bashrc
echo "source ~/.sibi_bashrc" >> ~/.bashrc

# echo "Copying New Keyboard Bindings"
# cp -v ./.Xmodmap ~/.Xmodmap

# # And set the new bindings here itself.
# xmodmap ~/.Xmodmap

cp -v ./.Xresources ~/

"echo Setting up GNU Screen configurations"
cp -v ./.screenrc ~/

echo "Setting bindings for Virtual Console"
sudo ./virtual.sh

echo "Copying ghci configuration"
cp -v ./.ghci ~/
chmod 700 ~/.ghci

echo "\nDon't forget to put settings to /etc/rc.local \n
      See virtual.sh for more details."

