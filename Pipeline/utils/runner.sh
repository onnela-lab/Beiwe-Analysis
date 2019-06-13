#!/bin/bash
export PATH="$(pwd):$PATH"
cd Beiwe-Analysis
git pull
cd ..

python3 Beiwe-Analysis/Pipeline/${FREQ}.py
