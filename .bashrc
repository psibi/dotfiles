#My own extra customization :)
source ~/.alias

PS1='\[\033[01;32m\]\u\[\033[01;34m\]::\[\033[01;31m\]\h \[\033[00;34m\]{ \[\033[01;34m\]\w \[\033[00;34m\]}\[\033[01;32m\]-> \[\033[00m\]'

export ALTERNATE_EDITOR=emacs EDITOR=emacsclient VISUAL=emacsclient

export PATH=${PATH}:~/bin:~/.cabal/bin

## http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
export MARKPATH=$HOME/.marks
function jump { 
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark { 
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark { 
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}

_completemarks() {
  local curw=${COMP_WORDS[COMP_CWORD]}
  local wordlist=$(find $MARKPATH -type l -printf "%f\n")
  COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
  return 0
}

complete -F _completemarks jump unmark

## For going to backward directory. (bd home when your pwd is /home/sibi/Documents/java)
function bd () {
  OLDPWD=`pwd`
  NEWPWD=`echo $OLDPWD | sed 's|\(.*/'$1'[^/]*/\).*|\1|'`
  index=`echo $NEWPWD | awk '{ print index($1,"/'$1'"); }'`
  if [ $index -eq 0 ] ; then
    echo "No such occurrence."
  else
    echo $NEWPWD
    cd "$NEWPWD"
  fi
}

## Similar to bd, but the argument is a number
function up () {
    if [[ $# -eq 1 && "$1" -gt 0 ]] ; then
        local i d
        for (( i = 0; i < $1; i++ )) ; do d="../$d" ; done
        cd $d
    else
        echo "Usage: up N"
    fi
}

# mkdir, cd into it
function mkcd () {
    mkdir -p "$*"
    cd "$*"
}

function pii() {
    ping 8.8.8.8
    sleep 3
    pii
}

#Because I hate when the window manager get's confused after a restart
function bindkeys () {
    xmodmap ~/.Xmodmap
    xmodmap -e 'keycode 135=Super_L' #Only for my laptop
}

function extract () {
    if [ -f "$1" ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf  "$1"    ;;
            *.tar.gz)    tar xvzf  "$1"    ;;
            *.bz2)       bunzip2  "$1"     ;;
            *.rar)       unrar x  "$1"     ;;
            *.gz)        gunzip  "$1"      ;;
            *.tar)       tar xvf  "$1"     ;;
            *.tbz2)      tar xvjf  "$1"    ;;
            *.tgz)       tar xvzf  "$1"    ;;
            *.zip)       unzip  "$1"       ;;
            *.Z)         uncompress  "$1"  ;;
            *.7z)        7z x  "$1"        ;;
            *)           echo "don't know how to extract '$1'..." ;;

        esac
    else
        echo "'$1' is not a valid file"
    fi
}

function ndir () {
    if [[ $# -eq 2 && "$1" -eq 0 ]] ; then
        cd $2
    elif [[ $# -eq 1 && "$1" -gt 0 ]] ; then
        local i d
        for (( i = 0; i < $1; i++ )) ; do d="../$d" ; done
        cd $d
    elif [[ $# -eq 2 && "$1" -gt 0 ]] ; then
        local i d
        for (( i = 0; i < $1; i++ )) ; do d="../$d" ; done
        cd $d
        cd $2
    else
        echo "Usage: ndir N directory\n
              Or     ndir N"
    fi
}

alias ..='ndir 1'
alias ...='ndir 2'
alias ....='ndir 3'
alias .....='ndir 4'
alias ......='ndir 5'

# Make bash append rather than override
shopt -s histappend        

PROMPT_COMMAND='history -a'

function c2nix () {
    cabalFile=`ls *.cabal | head -n 1`
    fileName="${cabalFile%.*}"
    nixFile="$fileName.nix"
    cabal2nix . > $nixFile
    defaultFile="{ nixpkgs ? import <nixpkgs> {}, compiler ? \"ghc7103\" }:\n
                   nixpkgs.pkgs.haskell.packages.\${compiler}.callPackage ./$nixFile { }"
    echo -e $defaultFile > default.nix
    shellFile='{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:\n
               (import ./default.nix { inherit nixpkgs compiler; }).env'
    echo -e $shellFile > shell.nix
}

if [ -x /home/sibi/.nix-profile/bin/cowsay -a -x /home/sibi/.nix-profile/bin/fortune ]; then
   fortune | cowsay
fi
