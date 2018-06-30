#! /bin/sh
git checkout master
rscript ./tangled.R
git commit -m "updated" ./docs/ ./data/
git push
git checkout gh-pages
git merge -Xtheirs master
git push
git checkout master