#/bin/bash
set -e
cd $(dirname $0) >/dev/null 2>&1 || exit $?
podman play kube --down podman.yaml
