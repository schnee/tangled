#! /bin/sh
git checkout master
rscript ./tangled.R
git commit -m "updated" ./docs/
git push
git checkout gh-pages
git merge master
git push
git checkout master