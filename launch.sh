#/bin/bash
# Build container
# Start application server
# Launch firefox with application

URL="http://localhost:3838"

set -e
cd $(dirname $0) >/dev/null 2>&1 || exit $?
podman play kube --replace podman.yaml
echo -n "shiny-server is starting up "
STATUS="WATING"; I=0; while [[ "$STATUS" != "200" ]]; do
  sleep 1;
  STATUS=$(curl -qsI "$URL" | head -n1 | awk '{print $2}')
  if [ $(($I % 10)) -eq 0 ]; then
    echo -n "."
  fi
  I=$(($I + 1))
done
firefox "${URL}"
