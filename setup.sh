# Devops script 
# Author: Sibi <sibi@psibi.in>

ln -s .alias ~/.alias
ln -s .global_ignore ~/.global_ignore

cp -v init.el ~/.emacs.d/init.el

git config --global core.excludefile ~/.global_ignore
cat ./.bashrc >> ~/.bashrc

