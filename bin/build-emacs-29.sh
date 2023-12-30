#!/usr/bin/env bash

test -d emacs || git clone https://git.savannah.gnu.org/git/emacs.git -b emacs-29
if [ $(uname -s) = "Darwin" ]; then
    brew install \
         gnutls \
         gcc \
         libgccjit \
         texinfo \
         tree-sitter \
         jansson \
         librsvg \
         imagemagick \
         jpeg \
         giflib \
         libpng \
         libtiff
    export GCCVER=$(brew unlink --dry-run gcc | grep "$(brew --prefix)/bin/gcc-[0-9]" | head -n 1 | awk -F- '{print $NF}')
    export CPPFLAGS="-I/opt/homebrew/opt/jpeg/include"
    export LDFLAGS="-L/opt/homebrew/opt/jpeg/lib"
    export CONFIGURE_ARGS="--with-native-compilation=aot --with-tree-sitter --with-gif --with-png --with-jpeg --with-rsvg --with-tiff --with-imagemagick --with-x-toolkit=gtk3 --with-xwidgets"
else
    sudo apt -y install \
         apt-transport-https \
         ca-certificates \
         curl \
         gnupg-agent \
         software-properties-common \
         texinfo \
         libxpm-dev \
         libjpeg-dev \
         libgif-dev \
         libtiff-dev \
         libgnutls28-dev \
         libtree-sitter-dev
    sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
    GCCVER=$(gcc -v 2>&1 | grep "gcc version" | sed -rn 's/gcc version ([0-9]+).*/\1/gp')
    sudo apt -y install "gcc-${GCCVER}" libgccjit0 "libgccjit-${GCCVER}-dev"
    sudo apt -y install libjansson4 libjansson-dev
    export CONFIGURE_ARGS="--with-tree-sitter --without-compress-install --with-native-compilation --with-json --with-mailutils"
fi
export CC="gcc-${GCCVER}"
function sync()
{
    if [ -d "$1" ]; then
        cd "$1"
        git pull
        cd ..
    else
        git clone https://github.com/tree-sitter/${1}.git
    fi
}

sync tree-sitter-typescript
sync tree-sitter-javascript
sync tree-sitter-cpp
sync tree-sitter-c

mkdir -p ~/.emacs.d/tree-sitter
$CC -shared -fPIC -g -O2 -I tree-sitter-typescript/typescript/src tree-sitter-typescript/typescript/src/scanner.c tree-sitter-typescript/typescript/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-typescript.so
$CC -shared -fPIC -g -O2 -I tree-sitter-typescript/typescript/src tree-sitter-typescript/tsx/src/scanner.c tree-sitter-typescript/tsx/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-tsx.so
$CC -shared -fPIC -g -O2 -I tree-sitter-javascript/src tree-sitter-javascript/src/scanner.c tree-sitter-javascript/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-javascript.so
$CC -shared -fPIC -g -O2 -I tree-sitter-cpp/src tree-sitter-cpp/src/scanner.c tree-sitter-cpp/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-cpp.so
$CC -shared -fPIC -g -O2 -I tree-sitter-c/src tree-sitter-c/src/parser.c -o ~/.emacs.d/tree-sitter/libtree-sitter-c.so

if [ ! -x emacs/src/emacs ]; then
    cd emacs
    ./autogen.sh
    ./configure $CONFIGURE_ARGS
    make -j10
    sudo make install
    cd ..
    if [ $(uname -s) = "Darwin" ]; then
        echo "If you want to stick this bundle into Applications you might do something like this:"
        echo "mv /Applications/Emacs.app /Applications/EmacsOld.app"
        echo "cp -r $(pwd)/emacs/nextstep/Emacs.app /Applications/"
    fi
fi

