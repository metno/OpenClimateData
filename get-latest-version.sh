#/bin/bash
## Get the latestversion from github, but make sure to keep updated data files.

mkdir $HOME/TMP.OCD
mv data $HOME/TMP.OCD
git pull
mv $HOME/TMP.OCD/data .
rmdir $HOME/TMP.OCD