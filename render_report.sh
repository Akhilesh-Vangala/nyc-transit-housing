#!/usr/bin/env bash
set -e
cd "$(dirname "$0")"

if [[ -d /opt/homebrew ]]; then
  HOMEBREW_BIN="/opt/homebrew/bin"
else
  HOMEBREW_BIN="/usr/local/bin"
fi

export PATH="${HOMEBREW_BIN}:${PATH}"
export RSTUDIO_PANDOC="${HOMEBREW_BIN}"

if ! command -v pandoc &>/dev/null; then
  echo "pandoc not found: brew install pandoc"
  exit 1
fi

Rscript -e "rmarkdown::render('report.Rmd')"
