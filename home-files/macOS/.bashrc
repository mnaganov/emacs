# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;
esac

# Update LINES and COLUMNS automatically
shopt -s checkwinsize

unset COLORTERM
case "$TERM" in
    dumb-emacs-ansi) color_prompt=yes;;
    xterm-color) color_prompt=yes;;
    xterm-16color) color_prompt=yes;;
    xterm-256color) color_prompt=yes;;
    screen-16color) color_prompt=yes;;
esac

if [ "$color_prompt" = yes ]; then
    PS1='\[\033[01;35m\]\D{%m/%d|%H:%M}\[\033[00m\] \[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\W\[\033[00m\]\$ '
else
    PS1='\D{%m/%d|%H:%M} \u@\h \W\$ '
fi
unset color_prompt

case "$TERM" in
    xterm*|rxvt*) PS1="\[\e]0;\u@\h: \w\a\]$PS1";;
esac

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

export PAGER=cat

. ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1="[${PS1%\\\$ }"'$(__git_ps1 " (%s)")]\$ '

#export PATH=/Applications/Emacs.app/Contents/MacOS:$PATH

