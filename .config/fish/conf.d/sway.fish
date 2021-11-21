set TTY1 (tty)
if test -z "$DISPLAY"; and test $TTY1 = "/dev/tty1"
    startx;
    i3;
  # exec sway
end
