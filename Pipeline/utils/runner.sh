#!/bin/bash
export PATH="$(pwd):$PATH"
cd Beiwe-Analysis
git pull
git status

echo "starting..."
cd ..

python3 Beiwe-Analysis/Pipeline/${FREQ}.py
