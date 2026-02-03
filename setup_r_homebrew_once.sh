#!/usr/bin/env bash
set -e

if [[ -d /opt/homebrew ]]; then
  HOMEBREW_PREFIX="/opt/homebrew"
else
  HOMEBREW_PREFIX="/usr/local"
fi

for pkg in udunits gdal geos pkg-config; do
  if ! brew list "$pkg" &>/dev/null; then
    brew install "$pkg"
  fi
done

PC_PATHS=(
  "${HOMEBREW_PREFIX}/opt/gdal/lib/pkgconfig"
  "${HOMEBREW_PREFIX}/opt/geos/lib/pkgconfig"
  "${HOMEBREW_PREFIX}/opt/proj/lib/pkgconfig"
  "${HOMEBREW_PREFIX}/opt/udunits/lib/pkgconfig"
  "${HOMEBREW_PREFIX}/lib/pkgconfig"
)
PKG_CONFIG_PATH=""
for d in "${PC_PATHS[@]}"; do
  [[ -d "$d" ]] && PKG_CONFIG_PATH="${d}:${PKG_CONFIG_PATH}"
done

R_DIR="$HOME/.R"
RENVIRON="$R_DIR/Renviron.site"
mkdir -p "$R_DIR"

BLOCK="# homebrew-r
LIBRARY_PATH=${HOMEBREW_PREFIX}/lib
CPATH=${HOMEBREW_PREFIX}/include
"
[[ -n "$PKG_CONFIG_PATH" ]] && BLOCK="${BLOCK}
PKG_CONFIG_PATH=${PKG_CONFIG_PATH}
"
if [[ ! -f "$RENVIRON" ]] || ! grep -q "homebrew-r" "$RENVIRON" 2>/dev/null; then
  echo "$BLOCK" >> "$RENVIRON"
fi
