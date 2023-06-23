#/bin/bash
# Pass commit message as first parameter
if [ -z "${1}" ]; then
  echo "ERROR: No commit message provided. Please see usage below for an example." >>/dev/stderr
  echo "Usage: $0 \"What I changed and why\"" >>/dev/stderr
  exit 1
fi
cd $(dirname $0)
git add .
git commit -m "${1}"
git pull --ff
git push
