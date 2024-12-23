#!/bin/bash
# Time-stamp: "2024-12-23 12:57:12 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_emacs.sh

echo "$0..."

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source /opt/Ninja/config/env/00_all.env


install_emacs_from_source() {
    # Get latest version
    local version=$(curl -s https://ftp.gnu.org/gnu/emacs/ | \
                        grep -o 'emacs-[0-9.]*\.tar\.xz' | \
                        sort -V | \
                        tail -n1 | \
                        sed 's/emacs-\(.*\)\.tar\.xz/\1/')


    # Check if Emacs is already installed
    if command -v emacs >/dev/null && emacs --version | grep -q "$version"; then
        echo "Emacs $version is already installed"
        return 0
    fi

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

    # Main
    cd /tmp
    wget -4 https://ftp.gnu.org/gnu/emacs/emacs-"$version".tar.xz || \
        wget -4 https://mirrors.kernel.org/gnu/emacs/emacs-"$version".tar.xz
    tar -xf emacs-"$version".tar.xz
    chown -R root:root emacs-"$version"*
    cd emacs-"$version"

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
    make -j 8 >/dev/null
    export INSTALL_OWNER=""
    make install -j 8 >/dev/null

    # Links
    /opt/emacs-"$version"/bin/emacs --version
    ln -sf /opt/emacs-"$version"/bin/emacs /usr/bin/emacs
    /opt/emacs-"$version"/bin/emacsclient --version
    ln -sf /opt/emacs-"$version"/bin/emacsclient /usr/bin/emacsclient

    verify_emacs_installation $version
}

verify_emacs_installation() {
    local version="$1"
    local errors=0
    local required_commands=("emacs" "emacsclient")
    local binary_path
    local version_output

    # Verification function
    verify_emacs_installation() {
        local errors=0
        local required_commands=("emacs" "emacsclient")
        local binary_path
        local version_output
        
        # Check binaries exist and are executable
        for cmd in "${required_commands[@]}"; do
            binary_path=$(which $cmd 2>/dev/null)
            if [ -z "$binary_path" ]; then
                echo "ERROR: $cmd not found in PATH" >&2
                ((errors++))
                continue
            fi
            
            if [ ! -x "$binary_path" ]; then
                echo "ERROR: $cmd is not executable" >&2
                ((errors++))
                continue
            fi
            
            echo "Found $cmd at: $binary_path"
            
            # Version check
            version_output=$($cmd --version 2>&1)
            if [ $? -ne 0 ]; then
                echo "ERROR: Failed to get version for $cmd" >&2
                ((errors++))
                continue
            fi
            
            # Verify version matches installed version
            if ! echo "$version_output" | grep -q "$version"; then
                echo "ERROR: Version mismatch for $cmd" >&2
                ((errors++))
                continue
            fi
            
            echo "$version_output" | head -n 1
        done
        
        # Check symlinks
        for cmd in "${required_commands[@]}"; do
            if [ ! -L "/usr/bin/$cmd" ]; then
                echo "ERROR: Symlink missing for $cmd" >&2
                ((errors++))
            fi
        done
        
        if [ $errors -gt 0 ]; then
            echo "Verification failed with $errors errors" >&2
            return 1
        fi
        
        echo "Emacs installation verified successfully"
        return 0
    }

    # Run verification
    if ! verify_emacs_installation; then
        echo "ERROR: Emacs installation verification failed" >&2
        return 1
    fi
}

install_nerd_fonts() {
    # Verify installation
    if fc-list | grep -q "Symbols"; then
        echo "Nerd Fonts is already installed"
        return 0
    fi

    ORIG_DIR="$(pwd)"
    # Install Nerd Fonts Symbols
    cd /tmp
    wget --no-check-certificate https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/NerdFontsSymbolsOnly.zip >/dev/null
    unzip NerdFontsSymbolsOnly.zip -d NerdFontsSymbolsOnly >/dev/null
    cd NerdFontsSymbolsOnly
    mkdir -p /root/.local/share/fonts >/dev/null
    cp *.ttf /root/.local/share/fonts/ >/dev/null
    fc-cache -f -v >/dev/null
    
    # Cleanup
    cd /tmp
    rm -rf NerdFontsSymbolsOnly* >/dev/null
    cd $ORIG_DIR

    # Verify installation
    if fc-list | grep -q "Symbols"; then
        echo "Nerd Fonts installation completed successfully"
        return 0
    else
        echo "Nerd Fonts installation failed"
        return 1
    fi
}

install_emacs_from_source
install_nerd_fonts

# EOF
