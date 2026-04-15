needed <- c("testthat", "pkgbuild", "rcmdcheck")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Please install required packages: ", paste(missing, collapse = ", "))
}

script_path <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile, mustWork = TRUE))
  }
  NA_character_
})

pkg <- if (!is.na(script_path)) dirname(script_path) else normalizePath(".", mustWork = TRUE)

if (dir.exists(file.path(pkg, "tests", "testthat"))) {
  testthat::test_dir(file.path(pkg, "tests", "testthat"), reporter = "summary")
}

pkgbuild::build(pkg)
rcmdcheck::rcmdcheck(pkg, args = "--as-cran", error_on = "warning")
