if (!file.exists("run_all.R")) setwd("..")
stopifnot(file.exists("run_all.R"))
source("config.R")
stopifnot(dir.exists(RAW))
stopifnot(length(BUFFER_RADIUS_M) >= 1)
stopifnot(CRS_WGS84 == 4326)

code <- system2("Rscript", c("run_all.R", "--dry-run"), stdout = NULL, stderr = NULL)
if (code != 0) stop("run_all.R --dry-run exited with code ", code)
