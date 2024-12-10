#!/bin/bash
# Time-stamp: "2024-12-10 16:47:23 (ywatanabe)"
# File: ./ninja/src/shell/launch_emacs.sh

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
# ⛔ Warning (initialization): An error occurred while loading ‘/home/ninja/.emacs.d/init.el’:

# File is missing: Opening output file, No such file or directory, /home/ninja/.sea/logs/history.log

# To ensure normal operation, you should investigate and remove the
# cause of the error in your initialization file.  Start Emacs with
# the ‘--debug-init’ option to view a complete error backtrace.

# EOF
