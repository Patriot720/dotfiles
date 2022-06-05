function record_webcam
    # - Audio
    # f alsa -ac 1 -ar 48000 -i hw:3 -acodec aac
    ffmpeg -framerate 30  -f v4l2 -video_size 1280x720 -i /dev/video1 -vf 'format=nv12,hwupload' -b:v 1M -vaapi_device /dev/dri/renderD128 -c:v hevc_vaapi  (date "+/home/andrew/Videos/%Y-%m-%d").mp4
end
