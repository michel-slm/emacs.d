#!/bin/bash

pushd $(dirname $0)

if [ ! -d elpa ]; then
  emacs --script pkg-install.el
fi

if [ ! -d snippets ]; then
  mkdir snippets
fi

popd
