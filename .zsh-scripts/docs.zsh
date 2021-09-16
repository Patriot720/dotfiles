#!/usr/bin/env zsh

export_docx(){
    fullname=$(basename $1);
    filename="${fullname%.*}"
    mkdir -p $filename
    mammoth $1 --output-dir=./$filename
}
