#!/bin/bash
# Time-stamp: "2024-12-18 04:16:55 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_emacs.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src


install_emacs_from_source() {
    # Remove apt-installed Emacs
    apt-get remove -y emacs emacs-common emacs-bin-common >/dev/null
    apt-get autoremove -y >/dev/null
    apt-get clean >/dev/null

    # Locale
    locale-gen en_US.UTF-8
    echo "LANG=en_US.UTF-8" | tee /etc/default/locale
    echo "LC_ALL=en_US.UTF-8" | tee -a /etc/default/locale

    # Dependencies
    apt-get update && apt-get install -y \
                              w3m \
                              autoconf \
                              make \
                              gcc \
                              texinfo \
                              libgtk-3-dev \
                              libxpm-dev \
                              libjpeg-dev \
                              libgif-dev \
                              libtiff5-dev \
                              libgnutls28-dev \
                              libncurses5-dev \
                              libjansson-dev \
                              libharfbuzz-dev \
                              libharfbuzz-bin \
                              imagemagick \
                              libmagickwand-dev \
                              libgccjit-10-dev \
                              libgccjit0 \
                              gcc-10 \
                              libjansson4 \
                              libjansson-dev \
                              xaw3dg-dev \
                              texinfo \
                              libx11-dev \
                              libtree-sitter0 \
                              libtree-sitter-dev \
                              libgccjit0 \
                              libgccjit-12-dev \
                              libwebkit2gtk-4.1-dev \
                              libacl1 \
                              libacl1-dev \
                              > /dev/null
    export LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/12:$LIBRARY_PATH

    # Source
    # Cleanup existing
    rm /tmp/emacs-"$version"* -rf 2>&1 >/dev/null
    rm /opt/emacs-"$version"* -rf 2>&1 >/dev/null

    version=29.4
    cd /tmp
    wget -4 https://ftp.gnu.org/gnu/emacs/emacs-"$version".tar.xz || \
        wget -4 https://mirrors.kernel.org/gnu/emacs/emacs-"$version".tar.xz
    tar -xf emacs-"$version".tar.xz
    chown -R root:root emacs-"$version"*
    cd emacs-"$version"

    # Cleanup existing
    rm /opt/emacs-"$version" -rf 2>&1 >/dev/null

    # Configure
    ./configure \
        --prefix=/opt/emacs-"$version" \
        --without-native-compilation \
        --with-json \
        --with-modules \
        --with-harfbuzz \
        --with-compress-install \
        --with-threads \
        --with-included-regex \
        --with-zlib \
        --with-jpeg \
        --with-png \
        --with-imagemagick \
        --with-tiff \
        --with-xpm \
        --with-gnutls \
        --with-xft \
        --with-xml2 \
        --with-mailutils \
        --with-tree-sitter \
        > /dev/null

    # Make Install
    make -j 8 > /dev/null
    export INSTALL_OWNER=""
    make install -j 8 > /dev/null

    # Links
    /opt/emacs-"$version"/bin/emacs --version
    ln -sf /opt/emacs-"$version"/bin/emacs /usr/bin/emacs
    /opt/emacs-"$version"/bin/emacsclient --version
    ln -sf /opt/emacs-"$version"/bin/emacsclient /usr/bin/emacsclient

    # Verification
    cmd='which emacs' && echo $cmd && echo && eval $cmd && echo
    cmd='emacs --version' && echo $cmd && echo && eval $cmd && echo
    cmd='which emacsclient' && echo $cmd && echo && eval $cmd && echo
    cmd='emacsclient --version' && echo $cmd && echo && eval $cmd && echo
}

install_nerd_fonts() {
    # Install Nerd Fonts Symbols
    cd /tmp
    wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/NerdFontsSymbolsOnly.zip
    unzip NerdFontsSymbolsOnly.zip -d NerdFontsSymbolsOnly
    cd NerdFontsSymbolsOnly
    mkdir -p /root/.local/share/fonts > /dev/null
    cp *.ttf /root/.local/share/fonts/ > /dev/null
    fc-cache -f -v

    # Install all-the-icons fonts (non-interactive)
    emacs --batch --eval "(progn (require 'all-the-icons) (all-the-icons-install-fonts t))" > /dev/null

    # Cleanup
    cd /tmp
    rm -rf NerdFontsSymbolsOnly*
}

install_emacs_from_source
install_nerd_fonts

# EOF
