function record_webcam
ffmpeg -framerate 24 -f v4l2 -video_size 1280x720 -i /dev/video1 -strftime 1 (date "+/home/andrew/Videos/%Y-%m-%d").mp4 -c:v vp9_v4l2m2m
end
