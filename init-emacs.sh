#!/bin/sh
pushd $(dirname $0)
if [ -f /etc/fedora-release ] && (! (rpm -q emacs-color-theme > /dev/null)); then
    sudo yum install emacs-color-theme
fi

if [ ! -d ~/checkouts ]; then
    mkdir ~/checkouts
fi

if [ ! -d ~/checkouts/git-wip ]; then
    pushd ~/checkouts
    git clone git://github.com/bartman/git-wip.git
    popd
fi

if [ ! -d org-mode ]; then
    git clone git://repo.or.cz/org-mode.git
    pushd org-mode
    RELTAG=$(git tag -l | grep release | tail -n 1)
    git checkout -b ${RELTAG} ${RELTAG}
    make
    popd
fi

if [ ! -d ac-slime ]; then
    git clone https://github.com/purcell/ac-slime.git
fi

if [ ! -d elpa ]; then
    emacs --script init-elpa.el
    emacs --script elpa-install.el
fi
popd
