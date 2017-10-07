#! /usr/bin/env bash

# Stash any change
git stash

stack exec site clean
stack exec site build

rm -fr ._site
mv -f _site ._site

stack exec site clean

ls -lsa ./._site

# Clean-up master
git checkout master
echo 'DELETE'
rm -frv ./*

echo 'MOVE CONTENT'
mv ._site/* .
rm -fr ./._site

git add ./*
git commit -am "Deploy"

# Re-apply stashed changes
git checkout hakyll
git stash pop
