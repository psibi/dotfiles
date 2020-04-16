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

ln ./.bashrc ~/.sibi_bashrc
echo "source ~/.sibi_bashrc" >> ~/.bashrc
ln ./.sibi-env ~/.sibi-env
# echo "Copying New Keyboard Bindings"
# cp -v ./.Xmodmap ~/.Xmodmap

# # And set the new bindings here itself.
# xmodmap ~/.Xmodmap

"echo Setting up GNU Screen configurations"
cp -v ./.screenrc ~/

# echo "Setting bindings for Virtual Console"
# sudo ./virtual.sh

echo "Copying ghci configuration"
cp -v ./.ghci ~/
chmod 700 ~/.ghci

# echo "\nDon't forget to put settings to /etc/rc.local \n
#      See virtual.sh for more details."

# https://nixmeal.wordpress.com/2012/07/24/copypaste-text-in-urxvt-rxvt-unicode-using-keyboard/
# echo "Setting up urxvt clipboard config"
