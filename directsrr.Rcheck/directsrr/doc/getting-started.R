## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="#>")
library(directsrr)

## -----------------------------------------------------------------------------
data(directsrr_ExampleData)
directsrr_ExampleData

## -----------------------------------------------------------------------------
directsrr(
  data        = directsrr_ExampleData,
  event       = "events",
  pop         = "population",
  stdpop      = "stdpop",       # one weight per age stratum
  standstrata = "age_group",    # single standardisation variable
  by          = "group",        # single grouping variable
  ref         = "A",            # reference group by label
  mult        = 100000
)

## -----------------------------------------------------------------------------
age_sex_data <- data.frame(
  age_group = rep(rep(c("18-39","40-64","65+"), each=2), 2),
  sex       = rep(c("F","M"), 6),
  ethnicity = rep(c("Asian","White"), each=6),
  events    = c( 4, 5, 12, 15,  9, 12,
                10,12, 28, 35, 22, 30),
  population= c(1900,2000,1700,1600,320,300,
                4800,5000,4200,4000,900,800),
  stdpop    = rep(c(0.20,0.20, 0.20,0.20, 0.10,0.10), 2)
)

directsrr(
  data        = age_sex_data,
  event       = "events",
  pop         = "population",
  stdpop      = "stdpop",
  standstrata = c("age_group", "sex"),   # joint standardisation
  by          = "ethnicity",
  ref         = "White",
  mult        = 100000
)

## -----------------------------------------------------------------------------
two_group_data <- data.frame(
  age_group = rep(rep(c("18-39","40-64","65+"), each=2), 4),
  sex       = rep(c("F","M"), 12),
  ethnicity = rep(c("Asian","Asian","White","White"), each=6),
  region    = rep(c("North","South","North","South"), each=6),
  events    = c( 2, 3,  7, 9,  9,12,
                 5, 6, 14,18, 16,22,
                 7, 8, 18,20, 22,30,
                12,15, 32,40, 40,55),
  population= c(1400,1500,1300,1200, 220,200,
                1900,2000,1900,1800, 380,350,
                2900,3000,2600,2500, 550,500,
                3800,4000,3700,3500, 800,700),
  stdpop    = rep(c(0.20,0.20, 0.20,0.20, 0.10,0.10), 4)
)

directsrr(
  data        = two_group_data,
  event       = "events",
  pop         = "population",
  stdpop      = "stdpop",
  standstrata = c("age_group", "sex"),
  by          = c("ethnicity", "region"),
  ref         = list(ethnicity = "White", region = "North"),  # named list
  mult        = 100000,
  digits_rate  = 1,
  digits_ratio = 3
)

## ----eval=FALSE---------------------------------------------------------------
# # Character label
# directsrr(..., ref = "Control")
# 
# # Numeric label — matches the group whose by-column value equals 2,
# # not the second row
# directsrr(..., ref = 2)
# 
# # Named list for multi-column by
# directsrr(..., ref = list(arm = "Placebo", centre = "London"))

## ----eval=FALSE---------------------------------------------------------------
# directsrr(..., ref_index = 1)   # first row  (the default)
# directsrr(..., ref_index = 2)   # second row

## ----eval=FALSE---------------------------------------------------------------
# directsrr(..., notify = "warn")     # default — warning(), catchable with tryCatch()
# directsrr(..., notify = "message")  # message(), suppressible with suppressMessages()
# directsrr(..., notify = "silent")   # no output — useful in loops or simulations

## ----eval=FALSE---------------------------------------------------------------
# std <- data.frame(
#   age_band   = c("18-39", "40-64", "65+"),
#   esp_weight = c(0.4, 0.4, 0.2)
# )
# mydata <- merge(mydata, std, by = "age_band")

## ----eval=FALSE---------------------------------------------------------------
# age_w <- c("18-39"=0.40, "40-64"=0.40, "65+"=0.20)
# sex_w <- c("F"=0.50, "M"=0.50)
# 
# std_joint <- expand.grid(age_band=names(age_w), sex=names(sex_w))
# std_joint$stdpop <- age_w[std_joint$age_band] * sex_w[std_joint$sex]
# mydata <- merge(mydata, std_joint, by = c("age_band","sex"))

## -----------------------------------------------------------------------------
directsrr(
  data        = directsrr_ExampleData,
  event       = "events",
  pop         = "population",
  stdpop      = "stdpop",
  standstrata = "age_group",
  mult        = 100000
)

## ----eval=FALSE---------------------------------------------------------------
# # IPD: one row per participant
# std_pop <- data.frame(age_group = c("18-39","40-64","65+"),
#                       stdpop    = c(0.4, 0.4, 0.2))
# ipd <- merge(my_individual_data, std_pop, by = "age_group")
# # columns needed: age_group, event (0/1), follow_up_years, stdpop, group
# 
# directsrr(ipd,
#          event       = "event",
#          pop         = "follow_up_years",
#          stdpop      = "stdpop",
#          standstrata = "age_group",
#          by          = "group",
#          ref         = "Control",
#          mult        = 100000)

