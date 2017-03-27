# Setting zsh home
export ZSH=/home/alex/.oh-my-zsh/

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=5000
SAVEHIST=5000
bindkey -v
# End of lines configured by zsh-newuser-install

# Custom configuration
# Enable plugins
plugins=(git fasd)

ZSH_THEME="gentoo"

# Initialize oh-my-zsh
source ~/.oh-my-zsh/oh-my-zsh.sh

# Virtualenvs home. For virtualenv wrapper
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
