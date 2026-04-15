## Regenerate the packaged example data file.
## Example: source("data-raw/create_data.R") from the package root.

directsrr_ExampleData <- data.frame(
  age_group  = rep(c("18-39", "40-64", "65+"), 3),
  group      = rep(c("A", "B", "C"), each = 3),
  events     = c(10L, 30L, 60L, 20L, 40L, 50L, 15L, 35L, 55L),
  population = c(2000L, 1500L, 500L, 2200L, 1600L, 600L, 1800L, 1400L, 400L),
  stdpop     = rep(c(0.4, 0.4, 0.2), 3),
  stringsAsFactors = FALSE
)

outfile <- file.path("data", "directsrr_ExampleData.R")
lines <- c(
  'directsrr_ExampleData <- data.frame(',
  '  age_group  = rep(c("18-39", "40-64", "65+"), 3),',
  '  group      = rep(c("A", "B", "C"), each = 3),',
  '  events     = c(10L, 30L, 60L, 20L, 40L, 50L, 15L, 35L, 55L),',
  '  population = c(2000L, 1500L, 500L, 2200L, 1600L, 600L, 1800L, 1400L, 400L),',
  '  stdpop     = rep(c(0.4, 0.4, 0.2), 3),',
  '  stringsAsFactors = FALSE',
  ')'
)
writeLines(lines, con = outfile)
message("Wrote ", outfile)
