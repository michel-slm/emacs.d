#!/bin/sh
if [ -f /etc/fedora-release ] && (! (rpm -q emacs-color-theme > /dev/null)); then
    sudo yum install emacs-color-theme
fi
emacs --script init-elpa.el
emacs --script elpa-install.el
