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
.calculate_futures_di_notional <- function(rates,maturity_date,basis_date = Sys.Date(),cal = NULL) {
  if (is.null(cal)) {
    cal <- create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  # 1) n of valid days
  if (inherits(maturity_date, "Date")) {
    n  <- bizdays(basis_date, maturity_date, cal)
    mm <- interval(basis_date, maturity_date) %/% months(1)
  } else if (is.numeric(maturity_date)) {
    n  <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    stop("'maturity_date' has to be a number of days or Date.")
  }
  # 2) calculate di rates
  tick_size <- if      (mm <=  3) 0.001
  else if (mm <= 60) 0.005
  else               0.010
  # 3) PU
  pu <- 1e5 / (1 + rates/100)^(n/252)
  # 4) tick-value
  deriv <- (n/252) * 1e5/100 * (1 + rates/100)^(-(n/252) - 1)
  tick_value <- deriv * tick_size
  # 5) return
  return(list(
    valid_days  = n,
    pu          = as.numeric(pu),
    tick_size   = tick_size,
    tick_value  = as.numeric(tick_value)
  ))
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
.calculate_futures_di_rates <- function(pu, maturity_date, basis_date = Sys.Date(), cal = NULL) {
  # 0) standard calendar

  if (is.null(cal)) {
    cal <- create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }


  # 1) valid days
  if (inherits(maturity_date, "Date")) {
    n  <- bizdays(basis_date, maturity_date, cal)
    mm <- lubridate::interval(basis_date, maturity_date) %/% months(1)
  } else if (is.numeric(maturity_date)) {
    n  <- as.integer(maturity_date)
    mm <- n / 21
  } else {
    stop("'maturity_date' has to be a number of days or Date.")
  }

  # 2) tick-size
  tick_size <- if      (mm <=  3) 0.001
  else if (mm <= 60) 0.005
  else               0.010

  rates <- 100 * ( (1e5 / pu)^(252 / n) - 1 )

  deriv       <- (n/252) * 1e5/100 * (1 + rates/100)^(-(n/252) - 1)
  tick_value  <- deriv * tick_size

  # 5) return
  return(list(
    valid_days  = n,
    rates        = round(as.numeric(rates),3),
    tick_size   = tick_size,
    tick_value  = round(as.numeric(tick_value),3)
  ))
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
                                        reinvest = FALSE, start_capital = 1000000,
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
                                           integerQty = TRUE, prefer = "Close", risk, reinvest = FALSE, start_capital = 1000000,verbose=TRUE,...){
  pos <- getPosQty(portfolio, symbol, timestamp)
  if ((orderside=="long"  && pos>0) ||
      (orderside=="short" && pos<0))
    return(0)
  verbose = TRUE
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
  print(vencimento_di)
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
  } else if (exists(Symbol, envir = .GlobalEnv, inherits = FALSE)) {
    data_obj <- get(Symbol, envir = .GlobalEnv)
  }

  # Try to get the instrument definition registered with FinancialInstrument.
  instrument_info <- tryCatch(
    getInstrument(Symbol, silent = TRUE),
    error = function(e) NULL
  )

  if (!FinancialInstrument::is.instrument(instrument_info)) {
    instrument_info <- NULL
    # Attempt late registration from cached market data if available.
    if (!is.null(data_obj)) {
      .register_future_from_data(Symbol, data_obj, overwrite = FALSE)
      instrument_info <- tryCatch(
        getInstrument(Symbol, silent = TRUE),
        error = function(e) NULL
      )
      if (!FinancialInstrument::is.instrument(instrument_info)) {
        instrument_info <- NULL
      }
    }
  }

  ids_from_instrument <- list()
  symbol_id <- Symbol
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
    }
    NA_real_
  }

  slippage <- pick_identifier(c("slippage", "Slippage", "spread", "Spread"))
  fees <- pick_identifier(c("fees", "fee", "Fees", "commission", "commission_per_contract"))
  multiplier <- first_numeric(c(multiplier, data_multiplier))

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
  matched_pattern <- startsWith_any(symbol_id, patterns1)

  qty <- abs(as.numeric(TxnQty))
  price <- as.numeric(TxnPrice)

  if (!is.null(matched_pattern) && matched_pattern %in% c("BGI", "CCM")) {
    return(-1 * (((0.1 * price) * (multiplier / 100)) + fees) * qty)
  }

  -1 * (slippage * price * qty)
}
