#!/bin/bash
# Time-stamp: "2024-12-09 10:43:35 (ywatanabe)"
# File: ./Semacs/src/shell/install_emacs.sh

# Locale
locale-gen en_US.UTF-8
echo "LANG=en_US.UTF-8" | tee /etc/default/locale
echo "LC_ALL=en_US.UTF-8" | tee -a /etc/default/locale

# Dependencies
apt-get update && apt install -y \
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
     libwebkit2gtk-4.0-dev \
     libacl1 \
     libacl1-dev \
     > /dev/null
export LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/12:$LIBRARY_PATH

# Source
version=29.4
cd /tmp
wget https://ftp.gnu.org/gnu/emacs/emacs-"$version".tar.xz
tar -xf emacs-"$version".tar.xz
cd emacs-"$version"

# Cleanup existing
rm /opt/emacs-"$version" -rf

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
make -j 8
make install -j 8

# Links
/opt/emacs-"$version"/bin/emacs --version
ln -sf /opt/emacs-"$version"/bin/emacs /usr/bin/emacs

# Verification
cmd='which emacs' && echo $cmd && echo && eval $cmd && echo
cmd='emacs --version' && echo $cmd && echo && eval $cmd && echo

# EOF
