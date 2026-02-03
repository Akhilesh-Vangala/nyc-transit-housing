pkgs <- c("sf", "dplyr", "readr", "ggplot2", "leaflet", "htmlwidgets", "jsonlite", "units", "here", "rmarkdown")
repos <- "https://cloud.r-project.org"
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    tryCatch(install.packages(p, repos = repos, quiet = TRUE), error = function(e) NULL)
}
still_missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(still_missing) > 0) {
  if ("sf" %in% still_missing && Sys.info()[["sysname"]] == "Darwin")
    stop("Missing: ", paste(still_missing, collapse = ", "), ". macOS: run ./install_deps_macos.sh or conda install -c conda-forge r-sf r-s2 r-leaflet r-dplyr r-readr r-ggplot2 r-htmlwidgets r-jsonlite r-units r-here r-rmarkdown")
  else
    stop("Missing: ", paste(still_missing, collapse = ", "), ". Run install.packages() for these.")
}
