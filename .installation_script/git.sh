#!/usr/bin/env bash

redirect_github_https_to_ssh(){
    git config --global url.ssh://git@github.com/.insteadOf https://github.com/
}

configure_git_credentials(){
    echo "Input git Email:";
    read email;
    echo "Input git Username:"
    read username;
    git config --global user.email "$email";
    git config --global user.name "$username";
}

if prompt "Redirect github https to ssh?"; then
    redirect_github_https_to_ssh;
fi

if prompt "Configure git credentials?"; then
    configure_git_credentials;
fi
