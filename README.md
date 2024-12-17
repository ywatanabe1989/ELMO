<!-- ---
!-- title: ./Ninja/README.md
!-- author: ywatanabe
!-- date: 2024-12-17 17:22:55
!-- --- -->


# Ninja — Neural Information Network of Journaling Agents

(THIS REPOSITORY IS CURRENTLY UNDER ACTIVE DEVELOPMENT)

## Introduction
Ninja is an LLM agent system run on Emacs, which offers unique characteristics:
- Full CUI operations with rich GUI
- Interfaces for hacking tools seasoned by history
- Self-evolving potentials inherited from Emacs and Elisp

Here, we reintroduce Emacs — born in MIT's AI Lab in 1970s — as a catalyst for AI agents.

## Quick Start
```bash
git clone https://github.com/user/Ninja.git ~/.emacs.d/Ninja
```

## Apptainer

``` bash
apptainer build \
    --sandbox \
    --fakeroot \
    ./.apptainer/ninja/ninja.sandbox \
    ./.apptainer/ninja/ninja.def \
    2>&1 | tee ./.apptainer/ninja/ninja.sandbox.log

cd
apptainer shell \
    --writable \
    --fakeroot \
    --bind /home/ywatanabe/.emacs.d:/root/.emacs.d \
    /home/ywatanabe/.emacs.d/lisp/Ninja/.apptainer/ninja/ninja.sandbox


# Working to open emacs using the host's .emacs.d
apptainer shell \
    --writable \
    --fakeroot \
    --env USER=root \
    --env LOGNAME=root \
    --bind "/home/ywatanabe/.dotfiles/.emacs.d:/root/.emacs.d" \
    --bind "/home/ywatanabe:/home/ywatanabe" \
    --bind "/home/ywatanabe:/root" \
    --home /root \
    /home/ywatanabe/.emacs.d/lisp/Ninja/.apptainer/ninja/ninja.sandbox
# Apptainer > emacs
# --bind "/tmp/ninja-shared:/tmp/ninja-shared" \


apptainer shell \
    --writable \
    --fakeroot \
    --env USER=root \
    --env LOGNAME=root \
    --bind "/home/ywatanabe/.dotfiles/.emacs.d:/root/.emacs.d" \
    --bind "/home/ywatanabe:/home/ywatanabe" \
    --bind "/home/ywatanabe:/root" \
    --home /root \
    /home/ywatanabe/.emacs.d/lisp/Ninja/.apptainer/ninja/ninja.sandbox


apptainer run \
    --writable \
    --fakeroot \
    --env USER=root \
    --env LOGNAME=root \
    --bind "/home/ywatanabe/.dotfiles/.emacs.d:/root/.emacs.d" \
    --bind "/home/ywatanabe:/home/ywatanabe" \
    --bind "/home/ywatanabe:/root" \
    --home /root \
    /home/ywatanabe/.emacs.d/lisp/Ninja/.apptainer/ninja/ninja.sandbox

kill_grep ninja -f
apptainer run \
    --writable \
    --fakeroot \
    --env USER=root \
    --env LOGNAME=root \
    --env HOME=/root \
    --env DISPLAY=$DISPLAY \
    --bind "/home/ywatanabe/.dotfiles/.emacs.d:/root/.emacs.d" \
    --bind "/home/ywatanabe:/home/ywatanabe" \
    --bind "/home/ywatanabe:/root" \
    --bind /tmp/.X11-unix:/tmp/.X11-unix \
    --home /root \
    /home/ywatanabe/.emacs.d/lisp/Ninja/.apptainer/ninja/ninja.sandbox



emacs --init-directory=/root/.emacs.d

HOME=$MYHOME emacs

apptainer shell \
     --bind /home/$USER:/root \
     --fakeroot \
     --writable \
     ./.apptainer/ninja/ninja.sandbox



apptainer run \
    --fakeroot \
    --writable \
    ./.apptainer/ninja/ninja.sandbox
```

## Contact
ywatanabe@alumni.u-tokyo.ac.jp


# Running Emacs from the Ninja user
./src/shell/launch_emacs.sh
```


arun_sw

# ashell_sw

less /opt/Ninja/src/apptainer_builders/ninja_environments.txt




apptainer exec \
    --fakeroot \
    --writable \
    --bind /home/$USER/.emacs.d:/root/.emacs.d \
    --bind /home/$USER:/root \
    --bind /home/$USER:/home/$USER \
    --bind /tmp/.X11-unix:/tmp/.X11-unix \
    --home /root \
    ./.apptainer/ninja/ninja.sandbox \
    /opt/Ninja/src/apptainer_builders/start_emacs.sh



# (make-frame-on-display)
(use-package crdt)
(crdt-protocol-version)
(crdt-share-buffer)
