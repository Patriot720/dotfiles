#!/usr/bin/env zsh

git_update(){
    git add .;
    git commit -m "update $(date "+%H:%M %a, %d %b")";
    git push origin master;
}

sync_repo(){
    cd $1;
    git_update;
    git submodule foreach "git add .;git commit -m 'update $(date "+%H:%M %a, %d %b")';git push origin master;";
}

sync_yadm(){
    fish -c "y"
    yadm add -u
    yadm commit -m "Update $(date "+%H:%M %a, %d %b")";
    yadm push origin master;
}

sync_repos(){
    sync_yadm
    sync_repo $HOME/.doom.d;
    sync_repo $HOME/.dump;
}
