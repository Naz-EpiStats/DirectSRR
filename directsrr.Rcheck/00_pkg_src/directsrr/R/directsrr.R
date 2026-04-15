# Exact Poisson CI for count k via the gamma quantile identity.
# Equivalent to Stata's _crccip (Newton-Raphson on the incomplete gamma).
.pois_ci <- function(k, level) {
  a  <- (100 - level) / 100
  lo <- if (k == 0) 0 else qgamma(a / 2,     shape = k,     rate = 1)
  hi <-                    qgamma(1 - a / 2,  shape = k + 1, rate = 1)
  c(lo = lo, hi = hi)
}

# Match a reference group by value against by-columns.
# ref: character scalar, numeric scalar, or named list (multi-column by).
.ref_by_value <- function(ref, s, by) {
  if (is.list(ref)) {
    bad_names     <- setdiff(names(ref), by)
    missing_names <- setdiff(by, names(ref))
    if (length(bad_names))
      stop("names(ref) contains column(s) not found in 'by': ",
           paste(bad_names, collapse = ", "), ".")
    if (length(missing_names))
      stop("ref list is missing by column(s): ",
           paste(missing_names, collapse = ", "),
           ". All by columns must be specified.")
    mat <- mapply(`==`, s[, names(ref), drop = FALSE], ref)
    ri  <- which(if (is.matrix(mat)) apply(mat, 1, all) else as.logical(mat))
  } else {
    if (length(by) > 1)
      stop("With multiple 'by' columns supply ref as a named list, ",
           "e.g. list(", by[1], " = value, ", by[2], " = value).")
    ri <- which(s[[by]] == ref)
  }
  if (length(ri) == 0)
    stop("'ref = ", deparse(ref), "' did not match any row in the output table.")
  if (length(ri) > 1)
    stop("'ref' matched ", length(ri), " rows; it must match exactly one.")
  as.integer(ri)
}

# Select a reference group by row position.
# ref_index: a single positive whole number (1, 2, 3 ... — no L suffix required).
.ref_by_index <- function(ref_index, n_rows) {
  if (!is.numeric(ref_index) || length(ref_index) != 1L ||
      ref_index != floor(ref_index) || ref_index < 1)
    stop("'ref_index' must be a single positive whole number (e.g. 1, 2, 3).")
  ri <- as.integer(ref_index)
  if (ri > n_rows)
    stop("'ref_index = ", ri, "' is out of range; the output table has ",
         n_rows, " row(s).")
  ri
}


#' Directly Standardised Rates, Rate Ratios, and Rate Differences
#'
#' @description
#' Calculates directly age-standardised rates (DSR) and supports pairwise
#' comparison of groups via standardised rate ratios (SRR) and standardised
#' rate differences (SRD).
#'
#' Confidence intervals for the DSR use the gamma-distribution method of
#' Tiwari, Clegg, and Zou (2006), a refinement of Fay and Feuer (1997) that
#' gives empirical coverage closer to the nominal level for rare outcomes.
#' The Dobson et al. (1991) interval can also be requested. SRR confidence
#' limits use the F-distribution approach from Tiwari et al.; SRD limits use
#' a normal approximation based on the same Tiwari variance.
#'
#' An R adaptation of the Stata command \code{distrate} (Consonni et al.
#' 2012), with the standard population as a column in \code{data} rather than
#' a separate file, flexible column naming, and no run-time dependencies
#' beyond base R.
#'
#' @param data A data frame in aggregate form. Multiple rows per stratum-group
#'   combination are summed before any calculation.
#' @param event Column name for event counts. Must be non-negative.
#'   Non-integer values are accepted; when \code{dobson = TRUE} the function
#'   warns that the Dobson CI may be unreliable. Default \code{"events"}.
#' @param pop Column name for person-time or population size (denominator).
#'   Must be strictly positive. Default \code{"population"}.
#' @param stdpop Column name for the standard-population weight. Must be
#'   constant within each stratum and non-negative in every row; the total
#'   weight sum within each group must be strictly positive. Absolute counts
#'   and proportions are both accepted; values are normalised within each group.
#'   Default \code{"stdpop"}.
#' @param standstrata Character vector of column name(s) defining the
#'   standardisation strata. Default \code{"age_group"}.
#' @param by Character vector of grouping column name(s), or \code{NULL} for
#'   a single pooled rate. Default \code{NULL}.
#' @param ref Reference group for SRR and SRD, specified as a **value** to
#'   match against the \code{by} column(s). Accepts:
#'   \itemize{
#'     \item A \strong{character or numeric scalar} matched against the single
#'       \code{by} column, e.g. \code{ref = "Control"} or \code{ref = 2}.
#'     \item A \strong{named list} for multi-column \code{by}, supplying a
#'       value for every column, e.g.
#'       \code{ref = list(arm = "Placebo", centre = "London")}.
#'   }
#'   Supply either \code{ref} or \code{ref_index}, not both. When both are
#'   \code{NULL} and \code{by} is not \code{NULL}, the first row of the
#'   output table is used as reference. Ignored when \code{by = NULL}.
#'   Default \code{NULL}.
#' @param ref_index Reference group for SRR and SRD, specified as a
#'   **row position** in the sorted output table. A single positive whole
#'   number (e.g. \code{1}, \code{2}, \code{3} — no \code{L} suffix
#'   required). Supply either \code{ref} or \code{ref_index}, not both.
#'   Ignored when \code{by = NULL}. Default \code{NULL}.
#' @param mult Rate multiplier. Default \code{1}.
#' @param level Confidence level as a percentage. Default \code{95}.
#' @param dobson Logical. Include Dobson et al. (1991) limits? Default \code{TRUE}.
#' @param check_pop Logical. Error if \code{event > pop} in any row? Appropriate
#'   for binomial-style data; leave \code{FALSE} for person-time denominators.
#'   Default \code{FALSE}.
#' @param digits_rate Decimal places for rate and SRD columns. Default \code{2}.
#' @param digits_ratio Decimal places for SRR columns. Default \code{4}.
#' @param digits_count Decimal places for count columns. Default \code{0}.
#' @param notify Character. Controls how the function signals that Dobson
#'   limits are unavailable for zero-event groups (when \code{dobson = TRUE}).
#'   \itemize{
#'     \item \code{"warn"} (default) — issues a \code{warning()}, catchable
#'       with \code{tryCatch()}. Recommended because \code{ub_dob = NA}
#'       changes how the interval should be interpreted.
#'     \item \code{"message"} — emits a \code{message()}, suppressible with
#'       \code{suppressMessages()}.
#'     \item \code{"silent"} — no output. Useful in loops or simulations.
#'   }
#'   Has no effect when there are no zero-event groups or \code{dobson = FALSE}.
#' @param verbose Print a summary table? Default \code{TRUE}.
#'
#' @return An invisible data frame with one row per group containing:
#'   \code{events}, \code{N}, \code{crude}, \code{rateadj},
#'   \code{lb_gam}/\code{ub_gam}, \code{se_gam},
#'   \code{lb_dob}/\code{ub_dob} (if \code{dobson = TRUE}),
#'   \code{srr}/\code{lb_srr}/\code{ub_srr},
#'   \code{srd}/\code{lb_srd}/\code{ub_srd}
#'   (last six only when \code{by} is not \code{NULL}).
#'   Rate and SRD columns are on the multiplied scale; SRR is dimensionless.
#'
#' @references
#' Consonni D, Coviello E, Buzzoni C, Mensi C (2012). A command to calculate
#' age-standardized rates with efficient interval estimation.
#' \emph{Stata Journal}, \strong{12}(4), 688--701.
#' \doi{10.1177/1536867X1201200408}
#'
#' Tiwari RC, Clegg LX, Zou Z (2006). Efficient interval estimation for
#' age-adjusted cancer rates. \emph{Statistical Methods in Medical Research},
#' \strong{15}(6), 547--569. \doi{10.1191/1740774506cn143oa}
#'
#' Fay MP, Feuer EJ (1997). Confidence intervals for directly standardized
#' rates: A method based on the gamma distribution.
#' \emph{Statistics in Medicine}, \strong{16}(7), 791--801.
#'
#' Dobson AJ, Kuulasmaa K, Eberle E, Scherer J (1991). Confidence intervals
#' for weighted sums of Poisson parameters.
#' \emph{Statistics in Medicine}, \strong{10}(3), 457--462.
#' \doi{10.1002/sim.4780100317}
#'
#' @examples
#' data(directsrr_ExampleData)
#'
#' # Reference group identified by label
#' directsrr(
#'   data        = directsrr_ExampleData,
#'   event       = "events",
#'   pop         = "population",
#'   stdpop      = "stdpop",
#'   standstrata = "age_group",
#'   by          = "group",
#'   ref         = "A",
#'   mult        = 100000,
#'   digits_rate  = 2,
#'   digits_ratio = 4
#' )
#'
#' # Reference group identified by row position (no L suffix needed)
#' directsrr(
#'   data        = directsrr_ExampleData,
#'   event       = "events",
#'   pop         = "population",
#'   stdpop      = "stdpop",
#'   standstrata = "age_group",
#'   by          = "group",
#'   ref_index   = 1,
#'   mult        = 100000
#' )
#'
#' # Single pooled rate, no comparison
#' directsrr(
#'   data        = directsrr_ExampleData,
#'   event       = "events",
#'   pop         = "population",
#'   stdpop      = "stdpop",
#'   standstrata = "age_group",
#'   mult        = 100000
#' )
#'
#' @importFrom stats aggregate ave qchisq qf qgamma qnorm
#' @export
directsrr <- function(data,
                     event        = "events",
                     pop          = "population",
                     stdpop       = "stdpop",
                     standstrata  = "age_group",
                     by           = NULL,
                     ref          = NULL,
                     ref_index    = NULL,
                     mult         = 1,
                     level        = 95,
                     dobson       = TRUE,
                     check_pop    = FALSE,
                     digits_rate  = 2,
                     digits_ratio = 4,
                     digits_count = 0,
                     notify       = c("warn", "message", "silent"),
                     verbose      = TRUE) {

  ## ---- argument checks -----------------------------------------------------
  notify <- match.arg(notify)

  if (!is.data.frame(data) || nrow(data) == 0)
    stop("'data' must be a non-empty data frame.")

  needed <- c(event, pop, stdpop, standstrata, by)
  bad    <- setdiff(needed, names(data))
  if (length(bad))
    stop("Column(s) not found in data: ", paste(bad, collapse = ", "))

  if (level <= 0 || level >= 100) stop("'level' must be in (0, 100).")

  for (arg in c("digits_rate", "digits_ratio", "digits_count")) {
    val <- get(arg)
    if (!is.numeric(val) || length(val) != 1L || val < 0 || val != floor(val))
      stop("'", arg, "' must be a single non-negative whole number (e.g. 2, not 2.5).")
  }

  ## ---- data-level checks ---------------------------------------------------
  ev_raw <- data[[event]]
  pt_raw <- data[[pop]]
  sp_raw <- data[[stdpop]]

  if (anyNA(ev_raw) || anyNA(pt_raw) || anyNA(sp_raw))
    stop("'", event, "', '", pop, "', and '", stdpop,
         "' must not contain missing values.")

  if (any(ev_raw < 0))
    stop("'", event, "' contains negative values.")

  if (any(pt_raw <= 0))
    stop("'", pop, "' must be strictly positive in every row.")

  if (any(sp_raw < 0))
    stop("'", stdpop, "' must be non-negative in every row.")

  if (check_pop && any(ev_raw > pt_raw))
    stop("'", event, "' exceeds '", pop, "' in at least one row. ",
         "If your denominator is person-time, set check_pop = FALSE.")

  raw_nonint <- any(ev_raw != floor(ev_raw))
  if (raw_nonint && !dobson) {
    message("Non-integer values found in '", event,
            "'. Proceeding, but verify this is intentional.")
  }

  alpha <- (100 - level) / 100

  ## ---- guard against all-zero stdpop per group (early check on raw data) ----
  {
    gid_raw <- if (!is.null(by))
      interaction(data[, by, drop = FALSE], drop = TRUE)
    else
      rep(1L, nrow(data))
    wp_sums_raw <- tapply(sp_raw, gid_raw, sum)
    if (any(wp_sums_raw == 0))
      stop("At least one group has all-zero standard-population weights.")
  }

  ## ---- stdpop: must be constant within each stratum (not just a warning) --
  sid    <- interaction(data[, standstrata, drop = FALSE], drop = TRUE)
  sp_var <- tapply(sp_raw, sid, function(x) length(unique(x)))
  if (any(sp_var > 1))
    stop("'", stdpop, "' is not constant within at least one stratum. ",
         "Ensure every row in the same age stratum carries the same weight.")

  ## ---- collapse events and person-time to (by x standstrata) --------------
  grp <- c(by, standstrata)

  ep <- aggregate(data[, c(event, pop)],
                  by  = data[, grp, drop = FALSE],
                  FUN = sum)
  names(ep)[names(ep) == event] <- "ev"
  names(ep)[names(ep) == pop]   <- "pt"

  # stdpop is stratum-level — must not be summed across rows
  wlu <- aggregate(data[, stdpop, drop = FALSE],
                   by  = data[, standstrata, drop = FALSE],
                   FUN = function(x) x[1])
  names(wlu)[names(wlu) == stdpop] <- "wp"

  d <- merge(ep, wlu, by = standstrata)
  d <- d[do.call(order, d[, grp, drop = FALSE]), ]
  rownames(d) <- NULL

  ## ---- strata completeness -------------------------------------------------
  gid <- if (!is.null(by))
    interaction(d[, by, drop = FALSE], drop = TRUE) else rep(1L, nrow(d))

  nc <- tapply(seq_len(nrow(d)), gid, length)
  if (length(unique(nc)) > 1)
    warning("Not all strata are present in every group. ",
            "Weights renormalised within available strata; ",
            "adjusted rates are not directly comparable across such groups.")

  ## ---- guard against all-zero stdpop within a group -----------------------
  wp_sums <- tapply(d$wp, gid, sum)
  if (any(wp_sums == 0))
    stop("At least one group has all-zero standard-population weights.")

  ## ---- normalised weights and per-group J ----------------------------------
  d$sw <- ave(d$wp, gid, FUN = sum)
  d$w  <- d$wp / d$sw
  d$J  <- ave(rep(1L, nrow(d)), gid, FUN = length)

  ## ---- stratum components --------------------------------------------------
  d$r  <- d$ev / d$pt
  d$wf <- d$w  / d$pt
  d$v  <- d$ev * d$wf^2
  d$vt <- (d$ev + 1/d$J) * d$wf^2
  d$ra <- d$w  * d$r

  ## ---- collapse to group level ---------------------------------------------
  bylist <- if (!is.null(by)) as.list(d[, by, drop = FALSE]) else
    list(.g. = rep(1L, nrow(d)))

  s     <- aggregate(d[, c("ev","pt","v","vt","ra")], by = bylist, FUN = sum)
  wfm   <- aggregate(d[, "wf", drop = FALSE],          by = bylist, FUN = mean)
  s$wfm <- wfm$wf

  if (!is.null(by))
    s <- s[do.call(order, s[, by, drop = FALSE]), ]
  rownames(s) <- NULL

  ## ---- rates and CIs (unscaled) --------------------------------------------
  ra <- s$ra;  v <- s$v;  vt <- s$vt
  ev <- s$ev;  pt <- s$pt
  rt <- ra + s$wfm
  cr <- ev / pt

  # Tiwari / gamma CI
  # Use indexed assignment rather than ifelse() to avoid evaluating v/(2*ra)
  # and ra^2/v when ev == 0 (which gives 0/0 = NaN with a warning in R,
  # even though ifelse() would have returned 0 in the end).
  nz   <- ev > 0
  lb_g <- rep(0, length(ev))
  lb_g[nz] <- (v[nz] / (2*ra[nz])) *
    qchisq(alpha/2, df = 2*ra[nz]^2 / v[nz])
  ub_g <- (vt / (2*rt)) * qchisq(1-alpha/2, df = 2*rt^2/vt)  # vt,rt > 0 always
  se_g <- sqrt(vt)

  # Dobson CI — exact Poisson limits scaled to the adjusted rate.
  # The scaling factor sqrt(v/ev) is algebraically undefined when ev == 0,
  # so ub_dob is set to NA for zero-event groups. lb_dob is set to 0 (the
  # natural lower bound). For zero-event groups the Tiwari gamma upper bound
  # (ub_gam) is well-defined and is the recommended CI to report instead.
  # Indexed assignment is used rather than ifelse() to avoid evaluating
  # sqrt(v/ev) = sqrt(0/0) when ev == 0, which would emit a warning in R.
  #
  # The Dobson method assumes integer event counts (it derives Poisson exact
  # limits). When events are non-integer — e.g. from survey-weighted data —
  # qgamma() is called with a non-integer shape, which is not the intended
  # use. The Tiwari gamma CI (ub_gam) has no such constraint and is preferred
  # in that case.
  agg_nonint <- any(ev != floor(ev))
  if (dobson && (raw_nonint || agg_nonint)) {
    warning("Non-integer event totals detected. ",
            "The Dobson CI (lb_dob / ub_dob) assumes integer counts and is ",
            "unreliable here. Use the Tiwari gamma CI (lb_gam / ub_gam) instead.",
            call. = FALSE)
  }
  pc   <- t(vapply(ev, .pois_ci, numeric(2), level = level))
  lb_d <- rep(0,        length(ev))
  ub_d <- rep(NA_real_, length(ev))
  if (any(nz)) {
    se_dob   <- sqrt(v[nz] / ev[nz])
    lb_d[nz] <- ra[nz] + (pc[nz, "lo"] - ev[nz]) * se_dob
    ub_d[nz] <- ra[nz] + (pc[nz, "hi"] - ev[nz]) * se_dob
  }
  if (dobson && any(!nz)) {
    txt <- paste("One or more groups have zero events: lb_dob = 0, ub_dob = NA",
                 "for those groups. Use ub_gam (Tiwari) as the upper bound instead.")
    if      (notify == "warn")    warning(txt, call. = FALSE)
    else if (notify == "message") message(txt)
  }

  ## ---- SRR and SRD ---------------------------------------------------------
  srr <- lb_srr <- ub_srr <- rep(NA_real_, nrow(s))
  srd <- lb_srd <- ub_srd <- rep(NA_real_, nrow(s))

  if (!is.null(by)) {
    # Resolve reference group row index from ref (value match) or ref_index
    if (!is.null(ref) && !is.null(ref_index))
      stop("Supply exactly one of 'ref' or 'ref_index', not both.")
    if (is.null(ref) && is.null(ref_index))
      ref_index <- 1L   # default: first row of the sorted output table
    ri <- if (!is.null(ref)) .ref_by_value(ref, s, by) else
                              .ref_by_index(ref_index, nrow(s))

    # SRD is always defined: srd = ra - ra_ref, even when ra_ref == 0.
    # SRR requires ra_ref > 0 (dividing by zero is undefined).
    se_srd <- sqrt(vt + vt[ri])
    srd    <- ra - ra[ri]
    lb_srd <- srd - qnorm(1-alpha/2) * se_srd
    ub_srd <- srd + qnorm(1-alpha/2) * se_srd
    srd[ri] <- 0; lb_srd[ri] <- NA_real_; ub_srd[ri] <- NA_real_

    if (ra[ri] == 0) {
      warning("Reference group adjusted rate is 0; SRR set to NA for all groups. ",
              "SRD is still computed.")
    } else {
      # lb_srr uses df1 = 2*ra^2/v, which is 0/0 = NaN when ra == 0 (zero
      # events). Use indexed assignment: default lb_srr = 0 for zero-rate
      # groups, compute normally only for ra > 0. ub_srr is safe for all
      # groups because rt > 0 and vt > 0 even when ra == 0.
      nzr    <- ra > 0
      srr    <- ra / ra[ri]
      lb_srr <- rep(0, nrow(s))
      lb_srr[nzr] <- (ra[nzr] / rt[ri]) *
        qf(alpha/2, df1 = 2*ra[nzr]^2/v[nzr], df2 = 2*rt[ri]^2/vt[ri])
      ub_srr <- (rt / ra[ri]) *
        qf(1-alpha/2, df1 = 2*rt^2/vt, df2 = 2*ra[ri]^2/v[ri])
      srr[ri] <- 1; lb_srr[ri] <- NA_real_; ub_srr[ri] <- NA_real_
    }
  }

  ## ---- apply multiplier ----------------------------------------------------
  cr     <- cr     * mult
  ra_out <- ra     * mult
  lb_g   <- lb_g   * mult;  ub_g   <- ub_g   * mult
  se_g   <- se_g   * mult
  lb_d   <- lb_d   * mult;  ub_d   <- ub_d   * mult
  srd    <- srd    * mult;  lb_srd <- lb_srd * mult;  ub_srd <- ub_srd * mult

  ## ---- assemble output -----------------------------------------------------
  out <- if (!is.null(by)) {
    s[, by, drop = FALSE]
  } else {
    data.frame(.placeholder = rep(NA_integer_, nrow(s)))
  }

  # events: always whole numbers after summing 0/1 (or integer count) data.
  # N (person-time): may be non-integer for IPD with fractional follow-up.
  # Only coerce to integer type when the aggregated values are genuinely whole,
  # to avoid silently truncating e.g. 12.8 person-years to 13.
  out$events <- if (digits_count == 0 && all(ev == floor(ev))) {
    as.integer(ev)
  } else {
    round(ev, digits_count)
  }
  out$N <- if (all(pt == floor(pt))) {
    if (digits_count == 0) as.integer(pt) else round(pt, digits_count)
  } else {
    pt
  }

  out$crude   <- round(cr,     digits_rate)
  out$rateadj <- round(ra_out, digits_rate)
  out$lb_gam  <- round(lb_g,   digits_rate)
  out$ub_gam  <- round(ub_g,   digits_rate)
  out$se_gam  <- round(se_g,   digits_rate)

  if (dobson) {
    out$lb_dob <- round(lb_d, digits_rate)
    out$ub_dob <- round(ub_d, digits_rate)
  }

  if (!is.null(by)) {
    out$srr    <- round(srr,    digits_ratio)
    out$lb_srr <- round(lb_srr, digits_ratio)
    out$ub_srr <- round(ub_srr, digits_ratio)
    out$srd    <- round(srd,    digits_rate)
    out$lb_srd <- round(lb_srd, digits_rate)
    out$ub_srd <- round(ub_srd, digits_rate)
  }

  if (is.null(by)) out$.placeholder <- NULL

  rownames(out) <- NULL

  ## ---- print ---------------------------------------------------------------
  if (verbose) {
    mp_str <- if (mult != 1)
      paste0(" (per ", formatC(mult, format="fg", big.mark=","), ")") else ""
    cat("\nDirectly standardised rates", mp_str, "\n", sep="")
    cat("Gamma CI: Tiwari, Clegg & Zou (2006) / Fay & Feuer (1997)")
    if (dobson) cat("  |  Dobson et al. (1991)")
    cat("\nLevel:", level, "%\n\n")
    print(out, row.names=FALSE)
    cat("\n")
  }

  invisible(out)
}
