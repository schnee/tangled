#! /bin/sh
git checkout master
rscript ./tangled.R
git commit -m "updated" ./docs/
git push
git checkout gh-pages
git rebase -Xtheirs master
git pull
git push
git checkout master