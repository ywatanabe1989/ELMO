#!/bin/bash
# Time-stamp: "2024-12-15 13:05:29 (ywatanabe)"
# File: ./Ninja/src/shell/launch_emacs.sh

export DISPLAY=:0
xhost +local:

apptainer shell --writable --fakeroot \
  --no-mount dev \
  --bind "/dev,/dev/pts,/dev/tty,/tmp/.X11-unix,/dev/null:/dev/null" \
  --env DISPLAY=$DISPLAY \
  ./.apptainer/ninja/ninja.sandbox

chmod 666 /dev/null
source /opt/Ninja/src/apptainer_builders/ENVS.sh.src
rm $NINJA_HOME/.emacs.d -rf
cp -r /opt/Ninja/src/apptainer_builders/safe_emacs.d $NINJA_HOME/.emacs.d
chown $NINJA_USER:$NINJA_USER -R $NINJA_HOME/.emacs.d
su - $NINJA_USER -c "DISPLAY=$DISPLAY emacs"


# EOF
