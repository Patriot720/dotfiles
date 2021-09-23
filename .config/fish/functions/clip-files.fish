function clip-files -d "Copy file[s] to clipboard to paste to telegram,nautilus etc..."
    set -l list ();
    for file in $argv
        set -a list "file://$PWD/$file\n"
    end
    echo -e $list | wl-copy -t text/uri-list
end
