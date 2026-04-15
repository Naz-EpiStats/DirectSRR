pkgname <- "directsrr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "directsrr-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('directsrr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("directsrr")
### * directsrr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: directsrr
### Title: Directly Standardised Rates, Rate Ratios, and Rate Differences
### Aliases: directsrr

### ** Examples

data(directsrr_ExampleData)

# Reference group identified by label
directsrr(
  data        = directsrr_ExampleData,
  event       = "events",
  pop         = "population",
  stdpop      = "stdpop",
  standstrata = "age_group",
  by          = "group",
  ref         = "A",
  mult        = 100000,
  digits_rate  = 2,
  digits_ratio = 4
)

# Reference group identified by row position (no L suffix needed)
directsrr(
  data        = directsrr_ExampleData,
  event       = "events",
  pop         = "population",
  stdpop      = "stdpop",
  standstrata = "age_group",
  by          = "group",
  ref_index   = 1,
  mult        = 100000
)

# Single pooled rate, no comparison
directsrr(
  data        = directsrr_ExampleData,
  event       = "events",
  pop         = "population",
  stdpop      = "stdpop",
  standstrata = "age_group",
  mult        = 100000
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("directsrr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("directsrr_ExampleData")
### * directsrr_ExampleData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: directsrr_ExampleData
### Title: Example dataset
### Aliases: directsrr_ExampleData
### Keywords: datasets

### ** Examples

data(directsrr_ExampleData)
head(directsrr_ExampleData)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("directsrr_ExampleData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
