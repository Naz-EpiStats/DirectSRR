data(directsrr_ExampleData)

# ---- main happy-path result ------------------------------------------------
res <- directsrr(
  data         = directsrr_ExampleData,
  event        = "events",
  pop          = "population",
  stdpop       = "stdpop",
  standstrata  = "age_group",
  by           = "group",
  ref          = "A",
  mult         = 100000,
  level        = 95,
  dobson       = TRUE,
  digits_rate  = 6,
  digits_ratio = 8,
  digits_count = 0,
  verbose      = FALSE
)

tol_r <- 0.01   # per 100,000 — wider than rounding to catch float drift
tol_s <- 1e-4

# reference values derived from Tiwari et al. (2006) formulas directly
test_that("groups are ordered correctly", {
  expect_equal(res$group, c("A", "B", "C"))
})
test_that("event totals", {
  expect_equal(res$events, c(100L, 110L, 105L))
})
test_that("population totals", {
  expect_equal(res$N, c(4000L, 4400L, 3600L))
})
test_that("crude rates", {
  expect_equal(res$crude, c(2500, 2500, 2916.666667), tolerance = tol_r)
})
test_that("adjusted rates", {
  expect_equal(res$rateadj, c(3400, 3030.303030, 4083.333333), tolerance = tol_r)
})
test_that("Tiwari gamma lower CI", {
  expect_equal(res$lb_gam, c(2751.689157, 2479.349065, 3307.945689), tolerance = tol_r)
})
test_that("Tiwari gamma upper CI", {
  expect_equal(res$ub_gam, c(4147.255722, 3663.485526, 4975.688209), tolerance = tol_r)
})
test_that("Tiwari SE", {
  expect_equal(res$se_gam, c(349.624137, 296.405166, 418.029768), tolerance = tol_r)
})
test_that("Dobson lower CI", {
  expect_equal(res$lb_dob, c(2750.853126, 2478.747383, 3306.146142), tolerance = tol_r)
})
test_that("Dobson upper CI", {
  expect_equal(res$ub_dob, c(4153.324694, 3665.925282, 4981.994080), tolerance = tol_r)
})
test_that("SRR point estimates", {
  expect_equal(res$srr[1], 1)
  expect_equal(res$srr[2], 0.89126560, tolerance = tol_s)
  expect_equal(res$srr[3], 1.20098039, tolerance = tol_s)
})
test_that("SRR confidence limits", {
  expect_true(is.na(res$lb_srr[1]) && is.na(res$ub_srr[1]))
  expect_equal(res$lb_srr[2], 0.67039561, tolerance = tol_s)
  expect_equal(res$ub_srr[2], 1.18724804, tolerance = tol_s)
  expect_equal(res$lb_srr[3], 0.89686722, tolerance = tol_s)
  expect_equal(res$ub_srr[3], 1.60812101, tolerance = tol_s)
})
test_that("SRD point estimates", {
  expect_equal(res$srd[1], 0)
  expect_equal(res$srd[2], -369.696970, tolerance = tol_r)
  expect_equal(res$srd[3],  683.333333, tolerance = tol_r)
})
test_that("SRD confidence limits", {
  expect_true(is.na(res$lb_srd[1]) && is.na(res$ub_srd[1]))
  expect_equal(res$lb_srd[2], -1268.064287, tolerance = tol_r)
  expect_equal(res$ub_srd[2],   528.670347, tolerance = tol_r)
  expect_equal(res$lb_srd[3],  -384.776773, tolerance = tol_r)
  expect_equal(res$ub_srd[3],  1751.443439, tolerance = tol_r)
})

# ---- ref / ref_index API ---------------------------------------------------
test_that("ref_index selects reference by row position (no L suffix needed)", {
  r2 <- directsrr(directsrr_ExampleData, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group", by="group",
                 ref_index=1, mult=100000, digits_rate=6, digits_ratio=8,
                 verbose=FALSE)
  expect_equal(res$srr, r2$srr)
  expect_equal(res$srd, r2$srd)
})

test_that("ref matches by value; ref=2 selects the group whose value is 2", {
  d_num <- directsrr_ExampleData
  d_num$gnum <- c(1L,1L,1L, 2L,2L,2L, 3L,3L,3L)
  rn <- directsrr(d_num, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group", by="gnum",
                 ref=2, mult=100000, digits_rate=6, digits_ratio=8,
                 verbose=FALSE)
  expect_equal(rn$srr[rn$gnum == 2], 1)
  expect_equal(rn$srd[rn$gnum == 2], 0)
  expect_false(rn$srr[rn$gnum == 1] == 1)
})

test_that("ref_index=2 selects the second row regardless of group values", {
  d_num <- directsrr_ExampleData
  d_num$gnum <- c(10L,10L,10L, 20L,20L,20L, 30L,30L,30L)
  rn <- directsrr(d_num, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group", by="gnum",
                 ref_index=2, mult=100000, verbose=FALSE)
  # second row (group 20) should be the reference
  expect_equal(rn$srr[rn$gnum == 20], 1)
  expect_equal(rn$srd[rn$gnum == 20], 0)
})

test_that("supplying both ref and ref_index errors clearly", {
  expect_error(
    directsrr(directsrr_ExampleData, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", by="group",
             ref="A", ref_index=1, verbose=FALSE),
    "exactly one"
  )
})

test_that("named-list ref works for multi-column by", {
  d2 <- directsrr_ExampleData; d2$sex <- "M"
  df <- rbind(d2, transform(d2, sex="F", events=events-2L))
  r_list <- directsrr(df, event="events", pop="population",
                     stdpop="stdpop", standstrata="age_group",
                     by=c("group","sex"),
                     ref=list(group="A", sex="M"),
                     mult=100000, verbose=FALSE)
  ref_row <- r_list$group == "A" & r_list$sex == "M"
  expect_equal(r_list$srr[ref_row], 1)
  expect_equal(r_list$srd[ref_row], 0)
})

test_that("ref not matching any row gives clear error", {
  expect_error(
    directsrr(directsrr_ExampleData, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", by="group",
             ref="NONEXISTENT", verbose=FALSE),
    "did not match any row"
  )
})

test_that("ref_index out of range gives clear error", {
  expect_error(
    directsrr(directsrr_ExampleData, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", by="group",
             ref_index=99, verbose=FALSE),
    "out of range"
  )
})

test_that("named-list ref with wrong column name gives a clear error", {
  d2 <- directsrr_ExampleData; d2$sex <- "M"
  df <- rbind(d2, transform(d2, sex="F", events=pmax(events-2L, 0L)))
  expect_error(
    directsrr(df, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group",
             by=c("group","sex"),
             ref=list(group="A", WRONG="M"),
             verbose=FALSE),
    "not found in 'by'"
  )
})

test_that("partial named-list ref missing a by column gives clear error", {
  d2 <- directsrr_ExampleData; d2$sex <- "M"
  df <- rbind(d2, transform(d2, sex = "F", events = pmax(events - 2L, 0L)))
  expect_error(
    directsrr(df, event = "events", pop = "population",
             stdpop = "stdpop", standstrata = "age_group",
             by = c("group", "sex"),
             ref = list(group = "A"),
             verbose = FALSE),
    "missing by column"
  )
})

# ---- zero-event group -------------------------------------------------------
test_that("zero-event group: Tiwari lb is 0, ub is finite", {
  d0 <- directsrr_ExampleData
  d0$events[d0$group == "C"] <- 0L
  r0 <- directsrr(d0, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group", by="group",
                 ref="A", mult=100000, verbose=FALSE)
  expect_equal(r0$lb_gam[r0$group == "C"], 0)
  expect_true(is.finite(r0$ub_gam[r0$group == "C"]))
})

test_that("zero-event group: Dobson lb is 0, ub is NA (undefined)", {
  d0 <- directsrr_ExampleData
  d0$events[d0$group == "C"] <- 0L
  r0 <- directsrr(d0, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group", by="group",
                 ref="A", mult=100000, verbose=FALSE)
  expect_equal(r0$lb_dob[r0$group == "C"], 0)
  expect_true(is.na(r0$ub_dob[r0$group == "C"]))
})

test_that("zero-event group does not produce NaN anywhere", {
  d0 <- directsrr_ExampleData
  d0$events[d0$group == "B"] <- 0L
  r0 <- directsrr(d0, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group", by="group",
                 ref="A", mult=100000, verbose=FALSE)
  num_cols <- sapply(r0, is.numeric)
  for (col in names(r0)[num_cols]) {
    vals <- r0[[col]]
    expect_false(any(is.nan(vals)),
                 label = paste("NaN in column", col))
  }
})

# ---- zero reference-group rate ----------------------------------------------
test_that("zero reference rate: SRR is NA, SRD is still computed", {
  d_zref <- directsrr_ExampleData
  d_zref$events[d_zref$group == "A"] <- 0L
  expect_warning(
    rz <- directsrr(d_zref, event="events", pop="population",
                   stdpop="stdpop", standstrata="age_group", by="group",
                   ref="A", mult=100000, verbose=FALSE),
    "Reference group"
  )
  # SRR undefined (reference rate is zero)
  expect_true(all(is.na(rz$srr)))
  # SRD still defined: ra_k - 0 = ra_k
  expect_false(all(is.na(rz$srd)))
  expect_equal(rz$srd[rz$group == "A"], 0)        # reference row
  expect_true(is.na(rz$lb_srd[rz$group == "A"]))  # reference CI is NA
  # other groups should have finite SRD equal to their rateadj
  expect_equal(rz$srd[rz$group == "B"],
               rz$rateadj[rz$group == "B"],
               tolerance = 0.01)
})

# ---- input validation -------------------------------------------------------
test_that("missing column raises error", {
  expect_error(
    directsrr(directsrr_ExampleData, event="no_col", pop="population",
             stdpop="stdpop", standstrata="age_group", verbose=FALSE),
    "Column\\(s\\) not found"
  )
})

test_that("negative events raises error", {
  bad <- directsrr_ExampleData; bad$events[1] <- -1L
  expect_error(
    directsrr(bad, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", verbose=FALSE),
    "negative"
  )
})

test_that("zero population raises error", {
  bad <- directsrr_ExampleData; bad$population[1] <- 0L
  expect_error(
    directsrr(bad, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", verbose=FALSE),
    "strictly positive"
  )
})

test_that("negative population raises error", {
  bad <- directsrr_ExampleData; bad$population[1] <- -100L
  expect_error(
    directsrr(bad, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", verbose=FALSE),
    "strictly positive"
  )
})

test_that("negative stdpop raises error", {
  bad <- directsrr_ExampleData; bad$stdpop[1] <- -0.1
  expect_error(
    directsrr(bad, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", verbose=FALSE),
    "non-negative"
  )
})

test_that("all-zero stdpop in a group raises error", {
  bad <- directsrr_ExampleData; bad$stdpop[bad$group == "B"] <- 0
  expect_error(
    directsrr(bad, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", by="group",
             verbose=FALSE),
    "all-zero"
  )
})

test_that("inconsistent stdpop within stratum raises error", {
  bad <- directsrr_ExampleData; bad$stdpop[1] <- 0.99
  expect_error(
    directsrr(bad, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", verbose=FALSE),
    "not constant"
  )
})

test_that("missing values in event column raise error", {
  bad <- directsrr_ExampleData; bad$events[1] <- NA_integer_
  expect_error(
    directsrr(bad, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", verbose=FALSE),
    "missing"
  )
})

test_that("check_pop = TRUE errors when events > pop", {
  bad <- directsrr_ExampleData; bad$events[1] <- bad$population[1] + 1L
  expect_error(
    directsrr(bad, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group",
             check_pop=TRUE, verbose=FALSE),
    "check_pop"
  )
})

test_that("events > pop allowed by default (person-time use case)", {
  ok <- directsrr_ExampleData; ok$events[1] <- ok$population[1] + 1L
  expect_no_error(
    directsrr(ok, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", verbose=FALSE)
  )
})

# ---- structural options -----------------------------------------------------
test_that("NULL by returns one row without ratio/difference columns", {
  r0 <- directsrr(directsrr_ExampleData, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group",
                 mult=100000, verbose=FALSE)
  expect_equal(nrow(r0), 1L)
  expect_false(any(c("srr","srd","lb_srr","lb_srd") %in% names(r0)))
})

test_that("dobson = FALSE removes Dobson columns", {
  rnd <- directsrr(directsrr_ExampleData, event="events", pop="population",
                  stdpop="stdpop", standstrata="age_group", by="group",
                  ref="A", mult=100000, dobson=FALSE, verbose=FALSE)
  expect_false("lb_dob" %in% names(rnd))
  expect_false("ub_dob" %in% names(rnd))
})

test_that("digits_rate rounds rate columns correctly", {
  r1 <- directsrr(directsrr_ExampleData, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group", by="group",
                 ref="A", mult=100000, digits_rate=1, verbose=FALSE)
  expect_equal(r1$rateadj, round(r1$rateadj, 1))
  expect_equal(r1$srd,     round(r1$srd,     1))
})

test_that("digits_rate rejects non-integer value", {
  expect_error(
    directsrr(directsrr_ExampleData, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group",
             digits_rate=2.5, verbose=FALSE),
    "whole number"
  )
})


# ---- ifelse NaN hygiene (review issue 5) ------------------------------------
test_that("zero-event groups produce no NaN warnings (only expected notify warning)", {
  d0 <- directsrr_ExampleData
  d0$events[d0$group == "C"] <- 0L
  # expect_warning catches the Dobson notify="warn" signal; no other warnings
  expect_warning(
    directsrr(d0, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", by="group",
             ref="A", mult=100000, verbose=FALSE),
    "zero events"
  )
  # with notify="silent" there should be no warnings at all
  expect_no_warning(
    directsrr(d0, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group", by="group",
             ref="A", mult=100000, notify="silent", verbose=FALSE)
  )
})

test_that("zero-event non-reference group: lb_srr=0, ub_srr finite, no NaN", {
  d0 <- directsrr_ExampleData
  d0$events[d0$group == "B"] <- 0L
  r0 <- directsrr(d0, event="events", pop="population",
                 stdpop="stdpop", standstrata="age_group", by="group",
                 ref="A", mult=100000, verbose=FALSE)
  b  <- r0[r0$group == "B", ]
  expect_equal(b$lb_srr, 0)
  expect_true(is.finite(b$ub_srr))
  expect_false(is.nan(b$srr))
  expect_false(is.nan(b$lb_gam))
})

# ---- individual participant data (IPD) and float person-time ----------------
test_that("IPD (one row per person) aggregates correctly to same result as aggregate input", {
  # Aggregate form (reference)
  agg <- data.frame(
    age_group  = c("18-39","65+", "18-39","65+"),
    group      = c("A","A",      "B","B"),
    events     = c(3L, 7L,       1L, 5L),
    population = c(8.5, 4.3,     7.2, 3.8),
    stdpop     = c(0.6, 0.4,     0.6, 0.4)
  )
  res_agg <- directsrr(agg, event="events", pop="population",
                      stdpop="stdpop", standstrata="age_group",
                      by="group", ref="A", mult=100000, verbose=FALSE)

  # IPD form: expand each stratum row to individual participants
  set.seed(1)
  make_ipd <- function(age, grp, ev, py, sp) {
    n <- max(round(py * 10), ev)
    data.frame(
      age_group = age, group = grp,
      events    = c(rep(1L, ev), rep(0L, n - ev)),
      population = py / n,        # person-time split equally
      stdpop    = sp
    )
  }
  ipd <- do.call(rbind, list(
    make_ipd("18-39","A", 3, 8.5, 0.6),
    make_ipd("65+",  "A", 7, 4.3, 0.4),
    make_ipd("18-39","B", 1, 7.2, 0.6),
    make_ipd("65+",  "B", 5, 3.8, 0.4)
  ))

  res_ipd <- directsrr(ipd, event="events", pop="population",
                      stdpop="stdpop", standstrata="age_group",
                      by="group", ref="A", mult=100000, verbose=FALSE)

  expect_equal(res_agg$rateadj, res_ipd$rateadj, tolerance=0.01)
  expect_equal(res_agg$srr,     res_ipd$srr,     tolerance=1e-4)
})

test_that("float person-time: N column preserved as numeric, not silently truncated", {
  d_float <- data.frame(
    age_group  = c("18-39","65+"),
    events     = c(3L, 7L),
    population = c(8.5, 4.3),   # non-integer person-time
    stdpop     = c(0.6, 0.4)
  )
  r <- directsrr(d_float, event="events", pop="population",
                stdpop="stdpop", standstrata="age_group",
                mult=100000, verbose=FALSE)
  # N should be 12.8, not 13
  expect_equal(r$N, 12.8)
  expect_false(is.integer(r$N))   # should NOT be coerced to integer type
})

test_that("integer person-time: N column stays integer when values are whole", {
  r <- directsrr(directsrr_ExampleData, event="events", pop="population",
                stdpop="stdpop", standstrata="age_group",
                by="group", ref="A", mult=100000, verbose=FALSE)
  expect_true(is.integer(r$N))
})

test_that("non-integer event totals warn about Dobson CI unreliability", {
  # Simulate survey-weighted data where events sum to a non-integer
  d_frac <- data.frame(
    age_group  = c("18-39","65+"),
    events     = c(2.7, 5.3),   # fractional survey-weighted counts
    population = c(8.5, 4.3),
    stdpop     = c(0.6, 0.4)
  )
  expect_warning(
    directsrr(d_frac, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group",
             dobson=TRUE, mult=100000, verbose=FALSE),
    "Non-integer event totals"
  )
  # Tiwari CI should still work without warning
  expect_no_warning(
    directsrr(d_frac, event="events", pop="population",
             stdpop="stdpop", standstrata="age_group",
             dobson=FALSE, mult=100000, verbose=FALSE)
  )
})
