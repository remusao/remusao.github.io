#! /usr/bin/env bash

# Remove old files
rm -fr *.html

# Generate blog
stack blog.hs

rm -fr ./deploy/*
mv -v ./*.html ./deploy/
cp -frv ./images ./deploy/

# TODO - push to master
cd ./deploy/ && git add * && git commit -am "Deploy" && git push origin master
