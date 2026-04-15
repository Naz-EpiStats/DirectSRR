remove.packages("directsrr")
setwd("C:/Users/email/Downloads/directsrr")

#install.packages("testthat")

devtools::document()

system("R CMD build .")


system("R CMD check --as-cran directsrr_0.1.0.tar.gz")


# tinytex::is_tinytex()
# tinytex::tlmgr_path()
# Sys.which("pdflatex")