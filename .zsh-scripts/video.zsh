#!/usr/bin/env zsh

h265(){
    case $1 in
        "-h" | "")
            echo "h265 \$1 File input \$2 output";;
        *)
            echo ffmpeg -i $file -c:v libx265 -c:a copy $out
            ffmpeg -i "$1" -c:v libx265 -c:a copy "$2";;
    esac
}

vapi(){
    case $1 in
        -h | "")
            echo "vapi\n\$1\tFile name\n\$2\toutput";;
        *)
            ffmpeg -i "$1" -c:v libvpx-vp9 -pass 2 -b:v 1000K -threads 8 -speed 1 \
                -tile-columns 6 -frame-parallel 1 -auto-alt-ref 1 -lag-in-frames 25 \
                -c:a libopus -b:a 64k -f webm "$2";;
    esac
}
