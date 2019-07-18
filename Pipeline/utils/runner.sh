#!/usr/bin/env bash
export PATH="$(pwd):$PATH"
pwd
ls -alh
cd Beiwe-Analysis
git fetch
git reset --hard origin/pipeline

bash Pipeline/utils/post-git.sh
