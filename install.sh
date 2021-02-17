#!/usr/bin/env sh

set -e

## Target installation directory:
REFORG_INSTALL_DIR="${REFORG_INSTALL_DIR:-"/usr/local/bin"}"

## Download the script:
curl -s -o - https://raw.githubusercontent.com/telostat/reforg/main/reforg > "${REFORG_INSTALL_DIR}/reforg"

## Change file permissions:
chmod +x "${REFORG_INSTALL_DIR}/reforg"

## Run:
reforg --help
