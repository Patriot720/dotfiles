#!/usr/bin/env fish

if pgrep -x wf-recorder > /dev/null
    killall -s SIGINT wf-recorder
    echo -e "file:///tmp/vid.mp4" | wl-copy -t text/uri-list
    notify-send "Video recorded"
else
    wf-recorder -g (slurp) -m mp4 -f /tmp/vid.mp4;
    notify-send "Started recording"
end
