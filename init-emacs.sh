#!/bin/bash
pushd $(dirname $0)
if [ -f /etc/fedora-release ]; then
    if ! rpm -q emacs-auto-complete >/dev/null 2>&1 ; then
        sudo yum install emacs-auto-complete
    fi

    if ! rpm -q emacs-color-theme >/dev/null 2>&1 ; then
        sudo yum install emacs-color-theme
    fi
fi

if [ ! -d ~/checkouts ]; then
    mkdir ~/checkouts
fi

if [ ! -d ~/checkouts/git-wip ]; then
    pushd ~/checkouts
    git clone git://github.com/bartman/git-wip.git
    popd
fi

if [ ! -x ~/.lein/bin/swank-clojure ]; then
    lein plugin install swank-clojure 1.3.2
fi

if [ ! -d ac-slime ]; then
    git clone https://github.com/purcell/ac-slime.git
fi

if [ ! -d coffee-mode ]; then
    git clone git://github.com/defunkt/coffee-mode.git
fi

if [ ! -d org-mode ]; then
    git clone git://repo.or.cz/org-mode.git
    pushd org-mode
    RELTAG=$(git tag -l | grep release | tail -n 1)
    git checkout -b ${RELTAG} ${RELTAG}
    make
    popd
fi

if [ ! -d pkg-el23 ]; then
    mkdir pkg-el23
    (cd pkg-el23 && wget -cN http://bit.ly/pkg-el23)
fi

if [ ! -d elpa ]; then
    emacs --script pkg-install.el
fi
popd
