#!/usr/bin/env zsh

export PATH="$PATH:/home/$USER/.dotnet/tools"

alias dotnet-test-debug="VSTEST_HOST_DEBUG=1 dotnet test"

if [ -d "/mnt/Home/nuget" ]; then
    export NUGET_PACKAGES="/mnt/Home/nuget"
fi
