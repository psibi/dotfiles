# Devops script 
# Author: Sibi <sibi@psibi.in>

ln .alias ~/.alias
ln .global_ignore ~/.global_ignore

if [ ! -d ~/.emacs.d ]; then
    mkdir -v ~/.emacs.d
fi

cp -v init.el ~/.emacs.d/init.el

git config --global core.excludefile ~/.global_ignore

ln ./.bashrc ~/.sibi_bashrc
echo "source ~/.sibi_bashrc" >> ~/.bashrc

echo "Copying New Keyboard Bindings"
cp -v ./.Xmodmap ~/.Xmodmap

# And set the new bindings here itself.
xmodmap ~/.Xmodmap

