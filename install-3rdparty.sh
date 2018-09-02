#!/usr/bin/env bash

set -xeu

PREFIX=${PREFIX:-$HOME/.local}

#-----------------------------------------------------------------------------
# bats
#-----------------------------------------------------------------------------
(
    [ -f $PREFIX/bin/bat ] && exit

    tmpdir=$(mktemp -d)
    trap "{ [ -d $tmpdir ] && rm -rf $tmpdir ; }" EXIT

    VERSION=v0.6.1
    URL=https://github.com/sharkdp/bat/releases/download/${VERSION}/bat-${VERSION}-x86_64-unknown-linux-gnu.tar.gz

    curl -L ${URL} | tar -xz -C $tmpdir --strip-components=1
    install -m 0660 -D $tmpdir/bat.1 $PREFIX/man/man1/
    install -m 0775 -D $tmpdir/bat $PREFIX/bin/
)


#-----------------------------------------------------------------------------
# tokei
#-----------------------------------------------------------------------------
(
    [ -f $PREFIX/bin/tokei ] && exit

    URL=https://github.com/Aaronepower/tokei/releases/download/v8.0.1/tokei-v8.0.1-x86_64-unknown-linux-gnu.tar.gz
    curl -L $URL | tar xz tokei -O > $PREFIX/bin/tokei
    chmod +x $PREFIX/bin/tokei
)
