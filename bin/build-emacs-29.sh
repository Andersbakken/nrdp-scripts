#!/usr/bin/env bash

GCCVER=$(gcc -v 2>&1 | grep "gcc version" | sed -rn 's/gcc version ([0-9]+).*/\1/gp')
test -d emacs || git clone https://git.savannah.gnu.org/git/emacs.git -b emacs-29
sudo apt -y install apt-transport-https ca-certificates curl gnupg-agent software-properties-common texinfo libxpm-dev libjpeg-dev libgif-dev libtiff-dev libgnutls28-dev libtree-sitter-dev
sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
sudo apt -y install "gcc-${GCCVER}" libgccjit0 "libgccjit-${GCCVER}-dev"
sudo apt -y install libjansson4 libjansson-dev
export CC="gcc-${GCCVER}"
if [ ! -x emacs/src/emacs ]; then
    cd emacs
    ./autogen.sh
    ./configure --with-tree-sitter --without-compress-install --with-native-compilation --with-json --with-mailutils
    make -j10
    sudo make install
    cd ..
fi

git clone https://github.com/tree-sitter/tree-sitter-typescript.git
git clone https://github.com/tree-sitter/tree-sitter-javascript.git
git clone https://github.com/tree-sitter/tree-sitter-cpp.git
git clone https://github.com/tree-sitter/tree-sitter-c.git

mkdir -p ~/.emacs.d/tree-sitter
$CC -shared -fPIC -g -O2 -I tree-sitter-typescript/typescript/src tree-sitter-typescript/typescript/src/scanner.c tree-sitter-typescript/typescript/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-typescript.so
$CC -shared -fPIC -g -O2 -I tree-sitter-typescript/typescript/src tree-sitter-typescript/tsx/src/scanner.c tree-sitter-typescript/tsx/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-tsx.so
$CC -shared -fPIC -g -O2 -I tree-sitter-javascript/src tree-sitter-javascript/src/scanner.c tree-sitter-javascript/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-javascript.so
$CC -shared -fPIC -g -O2 -I tree-sitter-cpp/src tree-sitter-cpp/src/scanner.c tree-sitter-cpp/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-cpp.so
$CC -shared -fPIC -g -O2 -I tree-sitter-c/src tree-sitter-c/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-c.so
