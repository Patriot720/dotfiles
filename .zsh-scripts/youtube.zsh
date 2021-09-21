#!/usr/bin/env bash

clip-youtube(){
    pushd /tmp
    youtube-dl --write-auto-sub --embed-subs --no-continue $1 -o tmp_vid.mp4 -f mp4
    clip-file tmp_vid.mp4
    popd
}

# TODO better naming
make-youtube-clip(){
    while getopts ":l" o; do
        case "$o" in
            l) length="$OPTARG"
               ;;
        esac
    done
    case $1 in
        "") echo -e "URL START (END | -l LENGTH)"
            ;;
        *)
            pushd /tmp
            youtube-dl --write-auto-sub --no-continue $1 -o tmp_vid.mp4 -f mp4
            python3 $HOME/.zsh-scripts/clean_youtube_dl_auto_subs.py tmp_vid.en.vtt > tmp_vid.vtt;
            if [ -n "$length"]; then
                ffmpeg -ss $2 -i tmp_vid.mp4 -t $3 -vf subtitles=tmp_vid.vtt:force_style='FontSize=40' -c:v libx264 -x264-params crf=22 -preset fast -profile:v high tmp_vid_out.mp4
            else
                ffmpeg -i tmp_vid.mp4 -ss $2 -to $3 -vf subtitles=tmp_vid.vtt:force_style='FontSize=40' -c:v libx264 -x264-params crf=22 -preset fast -profile:v high tmp_vid_out.mp4
            fi
            mv tmp_vid_out.mp4 tmp_vid.mp4
            clip-file tmp_vid.mp4
            notify-send "Youtube clip created $2 to $3"
            popd
            ;;
    esac
}

cut-tmp-video(){
    pushd /tmp
    ffmpeg  -ss $1 -i tmp_vid.mp4 -to $2 -c copy tmp_vid_out.mp4
    mv tmp_vid_out.mp4 tmp_vid.mp4
    clip-file tmp_vid.mp4
    notify-send "Youtube clip created $1 to $2"
    popd
}
