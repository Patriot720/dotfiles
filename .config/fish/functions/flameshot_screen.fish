function flameshot_screen
    flameshot (get_current_screen_geometry "full --region={width}x{height}+{x}+{y} -c" | string split -n " ")
end
