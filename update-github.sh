#/bin/bash
# Pass commit message as first parameter
if [ -z "${1}" ]; then
  echo "Warning: No commit message provided. Please see usage below for an example." >>/dev/stderr
  echo "Usage: $0 \"What I changed and why\"" >>/dev/stderr
fi
cd $(dirname $0)
git add ocdp/*/*.R ocdp/*/*.sh *.sh
git commit -m "${1:-further development}"
git pull --ff
git push
