#! /bin/bash
# execute the tangled.R script and update the github state
# set -e will bail out on any error, such as in the tangled.R script
set -e
git checkout master
Rscript ./tangled.R
git commit -m "updated" ./docs/ ./data/
git push
git checkout gh-pages
git merge -Xtheirs master
git push
git checkout master