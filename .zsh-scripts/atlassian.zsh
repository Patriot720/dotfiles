#!/usr/bin/env zsh

if [ -d "/opt/atlassian/plugin-sdk/bin" ] ; then
    PATH="/opt/atlassian/plugin-sdk/bin:$PATH"
fi

if [ -d "/opt/toolchains/arm-eabi-4.6/bin" ] ; then
    PATH="/opt/toolchains/arm-eabi-4.6/bin:$PATH"
fi
