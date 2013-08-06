# Devops script 
# Author: Sibi <sibi@psibi.in>

cp .alias ~/.alias
cp .global_ignore ~/.global_ignore

git config --global core.excludefile ~/.global_ignore
cat ./.bashrc >> ~/.bashrc

