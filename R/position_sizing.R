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
    cal <- bizdays::create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = bizdays::holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  basis_date <- as.Date(basis_date)

  # 1) Business days (n) and months to maturity (mm)
  if (inherits(maturity_date, "Date")) {
    md <- maturity_date
    n  <- bizdays::bizdays(basis_date, md, cal)
    mm <- lubridate::interval(basis_date, md) %/% months(1)
  } else if (is.numeric(maturity_date)) {
    md <- NULL
    n  <- as.integer(maturity_date)
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
  pu <- 1e5 / (1 + rates/100)^(n/252)

  # 4) Tick-value (magnitude)
  deriv_pp   <- -(n/252) * pu / (100 * (1 + rates/100))
  tick_value <- abs(deriv_pp) * tick_size

  # 5) Return (no rounding for precision)
  list(
    valid_days = n,
    pu         = as.numeric(pu),        # PU
    tick_size  = tick_size,             # percent points per tick
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
    cal <- bizdays::create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = bizdays::holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  basis_date <- as.Date(basis_date)

  # 1) Business days (n) and months to maturity (mm)
  if (inherits(maturity_date, "Date")) {
    md <- maturity_date
    n  <- bizdays::bizdays(basis_date, md, cal)
    mm <- lubridate::interval(basis_date, md) %/% lubridate::months(1)
  } else if (is.numeric(maturity_date)) {
    md <- NULL
    n  <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    md <- try(as.Date(maturity_date), silent = TRUE)
    if (inherits(md, "try-error") || is.na(md)) {
      stop("'maturity_date' must be Date, a number of business days, or coercible to Date.")
    }
    n  <- bizdays::bizdays(basis_date, md, cal)
    mm <- lubridate::interval(basis_date, md) %/% months(1)
  }

  if (n <= 0) stop("Number of business days to maturity (n) must be positive.")

  # 2) Tick-size (depends on regime at basis_date)
  tick_size <- .get_di_tick_size(mm, basis_date, rule_change_date)

  # 3) Rate from PU
  pu <- as.numeric(pu)
  if (any(!is.finite(pu) | pu <= 0)) stop("'pu' must be positive and finite.")

  rates <- 100 * ((1e5 / pu)^(252 / n) - 1)  # percent

  # 4) Tick-value (magnitude), using dPU/d(rate in percentage points)
  # dPU/d(r%) = -(n/252) * PU / (100 * (1 + r%/100))
  deriv_pp   <- -(n/252) * pu / (100 * (1 + rates/100))
  tick_value <- abs(deriv_pp) * tick_size

  # 5) Return (no rounding for precision)
  list(
    valid_days = n,
    rates      = as.numeric(rates),     # percent
    tick_size  = tick_size,             # percent points per tick
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



#' Fixed contracts order-sizing function
#'
#' Quantstrat `osFUN` that returns a fixed number of contracts unless there is
#' already an open position in the same direction, in which case it returns 0.
#'
#' @param timestamp Signal timestamp.
#' @param buyorderqty Fixed quantity to buy when orderside is long.
#' @param sellorderqty Fixed quantity to sell when orderside is short.
#' @param orderside Character, one of `"long"` or `"short"`.
#' @param portfolio Portfolio name.
#' @param symbol Instrument symbol.
#' @param ... Not used.
#' @return Numeric order quantity (positive for long, negative for short) or 0.
#' @keywords internal
.psFixedContractsQty <- function(timestamp,buyorderqty,sellorderqty,orderside,portfolio,symbol,...) {
  pos <- getPosQty(portfolio, symbol, timestamp)
  if (orderside == "short" && pos < 0) {
    return(0)
  } else if (orderside == "long" && pos > 0) {
    return(0)
  } else {
    if (orderside == "short") {
      return(sellorderqty)
    } else {
      return(buyorderqty)
    }
  }
}

#' Percentage-of-equity order-sizing function
#'
#' Simple quantstrat `osFUN` that targets approximately 2% of equity per trade,
#' estimating equity as `initEq + cumulative Net.Trading.PL`, and divides by the
#' current price to determine quantity. Provided primarily as a baseline.
#'
#' @param data Market data `xts`.
#' @param timestamp Signal timestamp.
#' @param orderqty Ignored; required by signature.
#' @param ordertype Order type string; required by signature.
#' @param orderside One of `"long"` or `"short"`.
#' @param portfolio Portfolio name.
#' @param symbol Instrument symbol.
#' @param tradeSize Max trade size; required by signature.
#' @param maxSize Max position size; required by signature.
#' @param integerQty Logical; if `TRUE`, quantities are floored to integers.
#' @param initEq Initial equity used for the percentage sizing.
#' @param ... Additional arguments ignored by this function.
#' @return Numeric order quantity (positive for long, negative for short) or 0.
#' @keywords internal
.psEquityPercentage <- function(data,timestamp,orderqty,ordertype,orderside,portfolio,symbol,tradeSize,maxSize,integerQty = TRUE,initEq=NULL,...) {
  pos <- getPosQty(portfolio, symbol, timestamp)
  datePos <- format(timestamp, "%Y-%m-%d")
  if (orderside == "short" && pos < 0) {
    return(0)
  } else if (orderside == "long" && pos > 0) {
    return(0)
  } else {
    if (orderside == "short") {
      updatePortf(
        Portfolio = portfolio,
        Symbols = symbol,
        Dates = paste0(start(data), "/", datePos)
      )
      pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
    } else {
      updatePortf(
        Portfolio = portfolio,
        Symbols = symbol,
        Dates = paste0(start(data), "/", datePos)
      )
      pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
    }
    qty <- ((initEq + pl) * 0.02) / as.numeric(Cl(data[datePos, ]))
    return(qty)
  }
}

#' Donchian risk-based order-sizing function
#'
#' Quantstrat `osFUN` that sizes positions based on a user-specified risk
#' percentage and Donchian channel stops. The per-contract risk is computed as
#' the distance between entry price and the opposite channel, adjusted by the
#' instrument multiplier and a minimum risk threshold.
#'
#' @param data Market data `xts` containing columns `X.el` (upper) and `Y.el` (lower).
#' @param timestamp Signal timestamp.
#' @param orderqty Ignored; required by signature.
#' @param ordertype Order type string; required by signature.
#' @param orderside One of `"long"` or `"short"`.
#' @param portfolio Portfolio name.
#' @param symbol Instrument symbol.
#' @param tradeSize Max trade size; required by signature.
#' @param maxSize Max position size.
#' @param integerQty Logical; if `TRUE`, quantities are floored to integers.
#' @param prefer Column name preferred for price lookup; defaults to `"Close"`.
#' @param risk Numeric, percent of equity (or capital) to risk per trade.
#' @param reinvest Logical; if `TRUE`, uses current equity from blotter; otherwise
#'   uses `start_capital`.
#' @param start_capital Numeric starting capital when `reinvest = FALSE`.
#' @param maxQty Optional absolute cap on quantity.
#' @param maxQtyBySymbol Optional named vector with per-symbol quantity caps.
#' @param minRiskPct Minimum risk as a fraction of price if tick size is unknown.
#' @param ... Additional arguments passed by quantstrat.
#' @return Numeric order quantity (positive for long, negative for short) or 0.
#' @keywords internal
.psEquityPercentageDonchian <- function(data, timestamp,
                                        orderqty, ordertype, orderside,
                                        portfolio, symbol,
                                        tradeSize , maxSize,
                                        integerQty = TRUE, prefer = "Close", risk,
                                        reinvest = FALSE, start_capital = 10000000,
                                        maxQty = NA,
                                        maxQtyBySymbol = NULL,
                                        minRiskPct = 0.0005,
                                        ...){

  # -------- current pos
  pos <- getPosQty(portfolio, symbol, timestamp)
  if ((orderside=="long"  && pos>0) ||
      (orderside=="short" && pos<0))
    return(0)

  # -------- prefer
  prc <- tryCatch(as.numeric(data[timestamp, prefer]),
                  error = function(e) NA)
  if (is.na(prc)) prc <- as.numeric(Cl(data[timestamp, ]))

  # -------- donchian upper/lower
  upper <- tryCatch(as.numeric(data[timestamp,"X.el"]), error=function(e) NA)
  lower <- tryCatch(as.numeric(data[timestamp,"Y.el"]), error=function(e) NA)
  if (anyNA(c(prc, upper, lower))) return(0)

  # -------- high/low bar (to detect when both are touched)
  hi <- suppressWarnings(tryCatch(as.numeric(Hi(data[timestamp, ])), error=function(e) NA))
  lo <- suppressWarnings(tryCatch(as.numeric(Lo(data[timestamp, ])), error=function(e) NA))
  bothTouched <- (!is.na(hi) && !is.na(lo) && hi >= upper && lo <= lower)

  # -------- stoploss and multiplier
  stopPrice <- if (orderside=="long") lower else upper
  mult <- tryCatch({
    instr <- getInstrument(symbol)
    if (!is.null(instr$multiplier)) instr$multiplier else 1
  }, error = function(e) 1)

  # -------- floor to min number of contracts
  riscoContr <- abs(prc - stopPrice) * mult

  tick <- tryCatch({
    instr <- getInstrument(symbol)
    if (!is.null(instr$tick_size)) instr$tick_size else NA_real_
  }, error = function(e) NA_real_)

  minRiskAbs <- if (is.finite(tick) && !is.na(tick) && tick > 0) {
    tick * mult
  } else {
    prc * minRiskPct * mult
  }

  riscoContrEff <- max(riscoContr, minRiskAbs, na.rm = TRUE)
  if (!is.finite(riscoContrEff) || riscoContrEff <= 0) return(0)

  # -------- equity and allowed risk
  if (isTRUE(reinvest)) {
    updatePortf(portfolio)
    updateAcct(portfolio)
    updateEndEq(portfolio)
    datePos <- format(timestamp, "%Y-%m-%d")
    eqty <- getEndEq(portfolio, datePos)
    allowedRisk  <- (risk/100) * eqty
  } else {
    allowedRisk  <- (risk/100) * start_capital
  }

  # -------- raw qty
  qtyRaw <- allowedRisk / riscoContrEff
  qtyAbs <- if (isTRUE(integerQty)) floor(qtyRaw) else as.numeric(qtyRaw)
  if (qtyAbs <= 0) return(0)

  # -------- actual max qty
  effMaxQty <- {
    symCap <- NA_real_
    if (!is.null(maxQtyBySymbol) && !is.null(names(maxQtyBySymbol))) {
      if (symbol %in% names(maxQtyBySymbol)) {
        symCap <- suppressWarnings(as.numeric(maxQtyBySymbol[[symbol]]))
      }
    }
    if (is.finite(symCap) && !is.na(symCap) && symCap > 0) {
      symCap
    } else if (is.finite(maxQty) && !is.na(maxQty) && maxQty > 0) {
      maxQty
    } else {
      Inf
    }
  }

  # -------- max qty cap
  qtyAbs <- min(qtyAbs, effMaxQty)

  # -------- max size cap
  if (is.finite(maxSize) && !is.na(maxSize) && maxSize > 0) {
    allowed <- max(0, maxSize - abs(pos))
    if (allowed <= 0) return(0)
    qtyAbs <- min(qtyAbs, allowed)
  }

  if (qtyAbs <= 0) return(0)

  qty <- if (orderside=="short") -qtyAbs else qtyAbs
  return(qty)
}

#' DI futures risk-based order-sizing function
#'
#' Variant of Donchian risk-based sizing tailored to DI futures. It estimates
#' contract notional and tick value from the DI rate and time to maturity to
#' compute risk per contract, then allocates contracts based on the risk budget.
#'
#' @param data Market data `xts` with Donchian columns and instrument `maturity`
#'   stored as an attribute.
#' @param timestamp Signal timestamp.
#' @param orderqty Ignored; required by signature.
#' @param ordertype Order type string; required by signature.
#' @param orderside One of `"long"` or `"short"`.
#' @param portfolio Portfolio name.
#' @param symbol Instrument symbol.
#' @param tradeSize Max trade size; required by signature.
#' @param maxSize Max position size.
#' @param integerQty Logical; if `TRUE`, quantities are floored to integers.
#' @param prefer Column name preferred for price/rate lookup.
#' @param risk Numeric, percent of capital to risk per trade.
#' @param reinvest Logical; if `TRUE`, use blotter equity; otherwise `start_capital`.
#' @param start_capital Numeric starting capital when not reinvesting.
#' @param verbose Logical; print intermediate sizing details.
#' @param ... Additional arguments passed by quantstrat.
#' @return Numeric order quantity (positive for long, negative for short) or 0.
#' @keywords internal
.psEquityPercentageDonchian_DI <- function(data, timestamp,
                                           orderqty, ordertype, orderside,
                                           portfolio, symbol,
                                           tradeSize , maxSize,
                                           integerQty = TRUE, prefer = "Close", risk, reinvest = FALSE, start_capital = 10000000,verbose=FALSE,...){
  pos <- getPosQty(portfolio, symbol, timestamp)
  if ((orderside=="long"  && pos>0) ||
      (orderside=="short" && pos<0))
    return(0)

  # -------- entry and loss rates
  taxaEnt <- as.numeric(data[timestamp,prefer])
  upper   <- as.numeric(data[timestamp,"X.el"])
  lower   <- as.numeric(data[timestamp,"Y.el"])
  if(verbose) {
    print(paste("entry rates:", taxaEnt))
    print(paste("upper:", upper))
    print(paste("lower:", lower))
  }
  if (anyNA(c(taxaEnt, upper, lower))) return(0)

  type_of_entry <- if (orderside=="long") lower else upper
  if(verbose) print(type_of_entry)

  # -------- valid days till maturity
  vencimento_di <- attr(data, "maturity")
  cal_b3 <- .generate_calendar()
  dados_di <- .calculate_futures_di_notional(type_of_entry,
                                             maturity_date = as.Date(vencimento_di),
                                             basis_date = timestamp,
                                             cal       = cal_b3)
  days_till_mat <- dados_di$valid_days
  pu_entrada <- round(dados_di$pu,2)
  ticksize <- dados_di$tick_size
  tickvalue <- round(dados_di$tick_value,2)

  if(verbose) {
    print(paste("Entry notional:", pu_entrada))
    print(paste("Days to Maturity:", days_till_mat))
    print(paste("Tick Size:", ticksize))
    print(paste("Tick Value:", tickvalue))
  }

  qty <- (((start_capital*(risk/100))/(upper-lower)/100)/tickvalue)/2
  qty <- floor(qty)
  if (qty <= 0) return(0)
  if (orderside=="short") qty <- -qty
  if(verbose)     print(paste("Contracts:", qty))
  return(qty)
}

#' Transaction fees and slippage calculator
#'
#' Computes transaction costs for a given trade. If instrument metadata is
#' available via `FinancialInstrument::getInstrument`, it tries to use stored
#' `slippage`, `fees`, and `multiplier` identifiers; otherwise falls back to 0.
#'
#' Note: current implementation returns 0 by default.
#'
#' @param TxnQty Numeric transaction quantity (signed).
#' @param TxnPrice Numeric transaction price.
#' @param Symbol Character instrument identifier.
#' @return Numeric cost (negative value). Defaults to 0 when information is
#'   unavailable.
#' @keywords internal
.calculate_fees <- function(TxnQty, TxnPrice, Symbol) {
  # Return zero when price or quantity are missing.
  if (is.na(TxnPrice) || is.na(TxnQty)) {
    return(0)
  }

  # Attempt to locate the market data for metadata fallbacks.
  data_obj <- NULL
  data_env <- .get_bt_data_env()
  if (exists(Symbol, envir = data_env, inherits = FALSE)) {
    data_obj <- get(Symbol, envir = data_env)
    data_obj <<- get(Symbol, envir = data_env)

  } else if (exists(Symbol, envir = .GlobalEnv, inherits = FALSE)) {
    data_obj <- get(Symbol, envir = .GlobalEnv)
  }

  drop_suffixes <- function(sym) {
    if (is.null(sym) || length(sym) == 0 || !nzchar(sym[1])) return(character())
    parts <- strsplit(sym[1], "_", fixed = TRUE)[[1]]
    if (length(parts) <= 1) return(character())
    vapply(seq_along(parts[-length(parts)]), function(i) {
      paste(parts[seq_len(length(parts) - i)], collapse = "_")
    }, character(1L))
  }

  gather_candidates <- function(sym, data_xts) {
    attrs <- character()
    if (!is.null(data_xts)) {
      attrs <- c(attrs,
                 attr(data_xts, "bt_original_symbol"),
                 attr(data_xts, "bt_root_symbol"))
    }
    cand <- c(sym, attrs)
    extras <- unlist(lapply(unique(c(sym, attrs)), drop_suffixes), use.names = FALSE)
    cand <- unique(c(cand, extras))
    cand <- cand[!is.na(cand) & nzchar(cand)]
    cand
  }

  fetch_instrument <- function(candidates) {
    for (cand in candidates) {
      inst <- tryCatch(getInstrument(cand, silent = TRUE), error = function(e) NULL)
      if (FinancialInstrument::is.instrument(inst)) {
        return(list(symbol = cand, instrument = inst))
      }
    }
    NULL
  }

  candidates <- gather_candidates(Symbol, data_obj)
  inst_lookup <- fetch_instrument(candidates)

  if (is.null(inst_lookup) && !is.null(data_obj)) {
    .register_future_from_data(Symbol, data_obj, overwrite = FALSE)
    inst_lookup <- fetch_instrument(gather_candidates(Symbol, data_obj))
  }

  if (!is.null(inst_lookup) && !identical(inst_lookup$symbol, Symbol)) {
    inst <- inst_lookup$instrument
    ids_copy <- inst$identifiers
    if (is.environment(ids_copy)) ids_copy <- as.list(ids_copy)
    tick_copy <- .sanitize_scalar_numeric(inst$tick_size, default = 0.01)
    mult_copy <- .sanitize_scalar_numeric(inst$multiplier, default = 1)
    currency_copy <- inst$currency
    try(
      FinancialInstrument::future(
        primary_id = Symbol,
        tick_size = tick_copy,
        multiplier = mult_copy,
        maturity = inst$maturity,
        currency = currency_copy,
        identifiers = ids_copy,
        overwrite = FALSE
      ),
      silent = TRUE
    )
    inst_lookup <- fetch_instrument(c(Symbol, inst_lookup$symbol))
  }

  instrument_info <- if (!is.null(inst_lookup)) inst_lookup$instrument else NULL
  symbol_id <- if (!is.null(inst_lookup)) inst_lookup$symbol else Symbol

  candidate_others <- setdiff(candidates, symbol_id)
  donor_lookup <- NULL
  if (length(candidate_others)) {
    donor_lookup <- fetch_instrument(candidate_others)
  }
  donor_info <- if (!is.null(donor_lookup)) donor_lookup$instrument else NULL

  ids_from_instrument <- list()
  multiplier <- NA_real_
  if (!is.null(instrument_info)) {
    symbol_id <- instrument_info$primary_id %||% Symbol
    ids_from_instrument <- instrument_info$identifiers
    if (is.environment(ids_from_instrument)) {
      ids_from_instrument <- as.list(ids_from_instrument)
    } else if (!is.list(ids_from_instrument)) {
      ids_from_instrument <- list()
    }
    multiplier <- suppressWarnings(as.numeric(instrument_info$multiplier))
  }

  ids_from_donor <- list()
  donor_multiplier <- NA_real_
  if (!is.null(donor_info)) {
    ids_from_donor <- donor_info$identifiers
    if (is.environment(ids_from_donor)) {
      ids_from_donor <- as.list(ids_from_donor)
    } else if (!is.list(ids_from_donor)) {
      ids_from_donor <- list()
    }
    donor_multiplier <- suppressWarnings(as.numeric(donor_info$multiplier))
  }

  ids_from_data <- list()
  data_multiplier <- NA_real_
  if (!is.null(data_obj)) {
    meta <- .collect_instrument_metadata(data_obj)
    ids_from_data <- meta$identifiers
    if (is.environment(ids_from_data)) {
      ids_from_data <- as.list(ids_from_data)
    } else if (!is.list(ids_from_data)) {
      ids_from_data <- list()
    }
    data_multiplier <- suppressWarnings(as.numeric(meta$multiplier))
    if (is.null(symbol_id)) {
      symbol_id <- Symbol
    }
  }

  first_numeric <- function(values) {
    vals <- suppressWarnings(as.numeric(values))
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) NA_real_ else vals[1]
  }

  pick_identifier <- function(keys) {
    for (nm in keys) {
      if (!is.null(ids_from_instrument[[nm]])) {
        return(first_numeric(ids_from_instrument[[nm]]))
      }
      if (!is.null(ids_from_data[[nm]])) {
        return(first_numeric(ids_from_data[[nm]]))
      }
      if (!is.null(ids_from_donor[[nm]])) {
        return(first_numeric(ids_from_donor[[nm]]))
      }
    }
    NA_real_
  }
  slippage <- pick_identifier(c("slippage", "Slippage", "spread", "Spread"))
  fees <- pick_identifier(c("fees", "fee", "Fees", "commission", "commission_per_contract"))
  multiplier <- first_numeric(c(multiplier, data_multiplier, donor_multiplier))

  if (is.na(slippage) && is.na(fees)) {
    warning(sprintf("Transaction cost identifiers not found for symbol: %s. Returning fees 0.", Symbol))
    return(0)
  }

  if (is.na(multiplier)) {
    warning(sprintf("Multiplier not found for symbol: %s. Returning fees 0.", Symbol))
    return(0)
  }

  slippage <- if (is.na(slippage)) 0 else slippage
  fees <- if (is.na(fees)) 0 else fees

  startsWith_any <- function(string, patterns) {
    if (!is.character(string) || length(string) == 0) return(NULL)
    for (pattern in patterns) {
      if (startsWith(string, pattern)) return(pattern)
    }
    NULL
  }

  patterns1 <- c("CCM", "BGI", "DOL", "GOLD", "WDO", "WIN", "IND", "COCOA", "CORN", "NATURAL_GAS")
  patterns2 <- c("DI1")

  matched_pattern_a <- startsWith_any(symbol_id, patterns1)
  matched_pattern_b <- startsWith_any(symbol_id, patterns2)

  qty <- abs(as.numeric(TxnQty))
  price <- as.numeric(TxnPrice)

  if (!is.null(matched_pattern_a) && matched_pattern_a %in% c("BGI")) {
    #print("BGI detected.")
    return(-1 * (((0.1 * price) * (multiplier / 100)) + fees) * qty)
  } else if (!is.null(matched_pattern_a) && matched_pattern_a %in% c("CCM")) {
      #print("CCM detected.")
      return(-1 * (((0.07 * price) * (multiplier / 100)) + fees) * qty)
  } else if (!is.null(matched_pattern_a) && matched_pattern_a %in% c("WDO")) {
    #print("WDO detected.")
    return(-1 * (((fees * 3)+10)*qty))
  } else if (!is.null(matched_pattern_a) && matched_pattern_a %in% c("WIN")) {
    #print("WIN detected.")
    return(-1 * (((fees * 3)+10)*qty))
  } else if (!is.null(matched_pattern_b) && matched_pattern_b %in% c("DI1")) {
    #print("DI1 futures detected.")
    return(-1 * (qty * fees * 1.25))

  }
  #print("Other detected.")
  -1 * (slippage * price * qty)
}

#' Fee calculator wrapper exposed for quantstrat callbacks
#'
#' Quantstrat resolves `TxnFees` arguments by name from the search path. When
#' `rBacktestTools` is loaded as a package, internal helpers (prefixed with a
#' dot) are not visible on the search path, so this thin wrapper delegates to the
#' internal implementation while remaining available for strategy execution.
#'
#' @keywords internal
#' @export
calculate_fees <- function(TxnQty, TxnPrice, Symbol) {
  .calculate_fees(TxnQty, TxnPrice, Symbol)
}
