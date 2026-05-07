#' DI futures notional and tick value from rates
#'
#' Computes DI futures notional price (PU) and tick value given an annualized
#' DI rate in percent and the time to expiry. The tick size depends on the
#' time to maturity.
#'
#' @param rates Numeric, annualized DI rate in percent.
#' @param maturity_date Either a `Date` (maturity date) or the number of business
#'   days to expiry as numeric/integer.
#' @param basis_date Reference `Date` from which to compute business days.
#' @param cal Optional `bizdays` calendar. If `NULL`, uses `"Brazil/ANBIMA"`.
#' @return A list with elements: `valid_days`, `pu`, `tick_size`, `tick_value`.
#' @keywords internal
.calculate_futures_di_notional <- function(
  rates,
  maturity_date,
  basis_date = Sys.Date(),
  cal = NULL,
  rule_change_date = as.Date("2025-08-28")
) {
  # 0) Calendar
  if (is.null(cal)) {
    cal <- .generate_calendar("Brazil/ANBIMA")
  }
  basis_date <- as.Date(basis_date)

  # 1) Business days (n) and months to maturity (mm)
  if (inherits(maturity_date, "Date")) {
    md <- maturity_date
    n <- bizdays::bizdays(basis_date, md, cal)
    mm <- .bt_months_between_floor(basis_date, md)
  } else if (is.numeric(maturity_date)) {
    md <- NULL
    n <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    stop("'maturity_date' must be a number of business days or Date.")
  }

  if (n <= 0) stop("Number of business days to maturity (n) must be positive.")

  # 2) Tick-size (depends on regime at basis_date)
  tick_size <- .get_di_tick_size(mm, basis_date, rule_change_date)

  # 3) PU from rate
  rates <- as.numeric(rates)
  if (any(!is.finite(rates) | rates <= -100)) stop("'rates' must be finite and > -100%.")
  pu <- 1e5 / (1 + rates / 100)^(n / 252)

  # 4) Tick-value (magnitude)
  deriv_pp <- -(n / 252) * pu / (100 * (1 + rates / 100))
  tick_value <- abs(deriv_pp) * tick_size

  # 5) Return (no rounding for precision)
  list(
    valid_days = n,
    pu         = as.numeric(pu), # PU
    tick_size  = tick_size, # percent points per tick
    tick_value = as.numeric(tick_value) # PU points per tick
  )
}

#' DI futures rate and tick value from PU
#'
#' Computes the annualized DI rate (percent) and tick value from a given PU
#' price and time to expiry. The tick size depends on the time to maturity.
#'
#' @param pu Numeric, DI futures price (PU).
#' @param maturity_date Either a `Date` (maturity date) or the number of business
#'   days to expiry as numeric/integer.
#' @param basis_date Reference `Date` from which to compute business days.
#' @param cal Optional `bizdays` calendar. If `NULL`, uses `"Brazil/ANBIMA"`.
#' @return A list with elements: `valid_days`, `rates`, `tick_size`, `tick_value`.
#' @keywords internal
.calculate_futures_di_rates <- function(
  pu,
  maturity_date,
  basis_date = Sys.Date(),
  cal = NULL,
  rule_change_date = as.Date("2025-08-01")
) {
  # 0) Calendar
  if (is.null(cal)) {
    cal <- .generate_calendar("Brazil/ANBIMA")
  }
  basis_date <- as.Date(basis_date)

  # 1) Business days (n) and months to maturity (mm)
  if (inherits(maturity_date, "Date")) {
    md <- maturity_date
    n <- bizdays::bizdays(basis_date, md, cal)
    mm <- .bt_months_between_floor(basis_date, md)
  } else if (is.numeric(maturity_date)) {
    md <- NULL
    n <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    md <- try(as.Date(maturity_date), silent = TRUE)
    if (inherits(md, "try-error") || is.na(md)) {
      stop("'maturity_date' must be Date, a number of business days, or coercible to Date.")
    }
    n <- bizdays::bizdays(basis_date, md, cal)
    mm <- .bt_months_between_floor(basis_date, md)
  }

  if (n <= 0) stop("Number of business days to maturity (n) must be positive.")

  # 2) Tick-size (depends on regime at basis_date)
  tick_size <- .get_di_tick_size(mm, basis_date, rule_change_date)

  # 3) Rate from PU
  pu <- as.numeric(pu)
  if (any(!is.finite(pu) | pu <= 0)) stop("'pu' must be positive and finite.")

  rates <- 100 * ((1e5 / pu)^(252 / n) - 1) # percent

  # 4) Tick-value (magnitude), using dPU/d(rate in percentage points)
  # dPU/d(r%) = -(n/252) * PU / (100 * (1 + r%/100))
  deriv_pp <- -(n / 252) * pu / (100 * (1 + rates / 100))
  tick_value <- abs(deriv_pp) * tick_size

  # 5) Return (no rounding for precision)
  list(
    valid_days = n,
    rates      = as.numeric(rates), # percent
    tick_size  = tick_size, # percent points per tick
    tick_value = as.numeric(tick_value) # PU points per tick
  )
}
.get_di_tick_size <- function(mm, basis_date, rule_change_date = as.Date("2025-08-28")) {
  basis_date <- as.Date(basis_date)
  if (basis_date < rule_change_date) {
    # Old rule (pre-change): 0–3m: 0.001; 3–60m: 0.005; >60m: 0.010
    if (mm <= 3) 0.001 else if (mm <= 60) 0.005 else 0.010
  } else {
    # New rule (post-change): 0–3m: 0.001; >3m: 0.005 (no 0.010 tier)
    if (mm <= 3) 0.001 else 0.005
  }
}

.bt_months_between_floor <- function(basis_date, maturity_date) {
  basis_date <- as.Date(basis_date)
  maturity_date <- as.Date(maturity_date)
  len <- max(length(basis_date), length(maturity_date))
  if (len == 0L) {
    return(integer())
  }
  basis_date <- rep_len(basis_date, len)
  maturity_date <- rep_len(maturity_date, len)
  out <- rep(NA_integer_, len)
  valid <- !is.na(basis_date) & !is.na(maturity_date)
  if (!any(valid)) {
    return(out)
  }
  bd <- basis_date[valid]
  md <- maturity_date[valid]
  year_diff <- as.integer(format(md, "%Y")) - as.integer(format(bd, "%Y"))
  month_diff <- as.integer(format(md, "%m")) - as.integer(format(bd, "%m"))
  diff <- year_diff * 12L + month_diff
  diff <- diff - as.integer(as.integer(format(md, "%d")) < as.integer(format(bd, "%d")))
  diff[diff < 0L] <- 0L
  out[valid] <- diff
  out
}
