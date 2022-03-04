function record_screen2
ffmpeg -video_size 1920x1080 -framerate 60 -f x11grab -i :0.0+2560,0 output.mp4
end
