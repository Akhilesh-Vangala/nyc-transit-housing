#!/usr/bin/env bash
set -e
cd "$(dirname "$0")"

if [[ -d /opt/homebrew ]]; then
  HOMEBREW_PREFIX="/opt/homebrew"
else
  HOMEBREW_PREFIX="/usr/local"
fi

PC_PATHS=(
  "${HOMEBREW_PREFIX}/opt/gdal/lib/pkgconfig"
  "${HOMEBREW_PREFIX}/opt/geos/lib/pkgconfig"
  "${HOMEBREW_PREFIX}/opt/proj/lib/pkgconfig"
  "${HOMEBREW_PREFIX}/opt/udunits/lib/pkgconfig"
  "${HOMEBREW_PREFIX}/lib/pkgconfig"
)
for d in "${PC_PATHS[@]}"; do [[ -d "$d" ]] && PKG_CONFIG_PATH="${d}:${PKG_CONFIG_PATH:-}"; done
export PKG_CONFIG_PATH
export CPATH="${HOMEBREW_PREFIX}/include:${CPATH:-}"
export LIBRARY_PATH="${HOMEBREW_PREFIX}/lib:${LIBRARY_PATH:-}"
export LD_LIBRARY_PATH="${HOMEBREW_PREFIX}/lib:${LD_LIBRARY_PATH:-}"
export DYLD_LIBRARY_PATH="${HOMEBREW_PREFIX}/lib:${DYLD_LIBRARY_PATH:-}"

Rscript install_deps.R
