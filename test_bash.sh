#!/usr/bin/env Rscript

cd ~/Github/Fish_Group_Models
Rscript test_r_bash_github.r

git add .
git commit -a -m "$1"
git push