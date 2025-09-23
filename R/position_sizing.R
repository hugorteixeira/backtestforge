.calculate_futures_di_notional <- function(rates,expiry_date,basis_date = Sys.Date(),cal = NULL) {
  if (is.null(cal)) {
    cal <- create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }
  # 1) n of valid days
  if (inherits(expiry_date, "Date")) {
    n  <- bizdays(basis_date, expiry_date, cal)
    mm <- interval(basis_date, expiry_date) %/% months(1)
  } else if (is.numeric(expiry_date)) {
    n  <- as.integer(expiry_date)
    mm <- n / 21
  } else {
    stop("'expiry_date' has to be a number of days or Date.")
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
.calculate_futures_di_rates <- function(pu, expiry_date, basis_date = Sys.Date(), cal = NULL) {
  # 0) standard calendar

  if (is.null(cal)) {
    cal <- create.calendar(
      name      = "Brazil/ANBIMA",
      holidays  = holidays("Brazil/ANBIMA"),
      weekdays  = c("saturday", "sunday")
    )
  }


  # 1) valid days
  if (inherits(expiry_date, "Date")) {
    n  <- bizdays(basis_date, expiry_date, cal)
    mm <- lubridate::interval(basis_date, expiry_date) %/% months(1)
  } else if (is.numeric(expiry_date)) {
    n  <- as.integer(expiry_date)
    mm <- n / 21
  } else {
    stop("'expiry_date' has to be a number of days or Date.")
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
.psEquityPercentageDonchian_DI <- function(data, timestamp,
                                           orderqty, ordertype, orderside,
                                           portfolio, symbol,
                                           tradeSize , maxSize,
                                           integerQty = TRUE, prefer = "Close", risk, reinvest = FALSE, start_capital = 10000000,verbose=TRUE,...){
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
                                             expiry_date = as.Date(vencimento_di),
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
.calculate_fees <- function(TxnQty, TxnPrice, Symbol) {
  # 1. Safety check for NA inputs
  # If price or quantity are not available, there are no fees.
  if (is.na(TxnPrice) || is.na(TxnQty)) {
    return(0)
  }
  return(0)
  # Tries to get instrument information using getInstrument
  instrument_info <- tryCatch({
    suppressWarnings(getInstrument(Symbol))
  }, error = function(e) {
    NULL  # If getInstrument fails, returns NULL
  })
  # If getInstrument returns valid information, calculates the fees
  if (!is.null(instrument_info) && !is.na(instrument_info)) {
    # Extracts relevant information
    print(instrument_info)
    print(str(instrument_info))
    symbol_id <- instrument_info$primary_id
    # Helper function to check string beginning
    startsWith_any <- function(string, patterns) {
      if(!is.character(string) || length(string) == 0) return(NULL)
      for (pattern in patterns) {
        if (startsWith(string, pattern)) return(pattern)
      }
      return(NULL)
    }
    # List of patterns to be checked
    patterns1 <- c("CCM","BGI","DOL","GOLD","WDO","WIN","IND","COCOA","CORN","NATURAL_GAS")
    # Find the matching pattern
    matched_pattern <- startsWith_any(symbol_id, patterns1)
    slippage <- instrument_info$identifiers$slippage
    fees <- instrument_info$identifiers$fees
    multiplier <- instrument_info$identifiers$multiplier
    # Check if identifiers were loaded correctly
    if (is.null(slippage) || is.null(fees) || is.null(multiplier)) {
      warning(paste("Identifiers (slippage, fees, multiplier) not found for symbol:", Symbol, ". Returning fees 0."))
      return(0)
    }
    # Define fees based on the found pattern
    if (!is.null(matched_pattern) && matched_pattern == "BGI") { # Added to be more specific
      # Use as.numeric to ensure values are numbers
      return(-1 * (as.numeric(slippage) * as.numeric(TxnPrice) * (as.numeric(multiplier)/100) + as.numeric(fees) * abs(as.numeric(TxnQty))))
    } else {
      # Fallback for other asset types
      return(-1 * (as.numeric(slippage) * as.numeric(TxnPrice) * abs(as.numeric(TxnQty))))
    }
  } else {
    # 2. Correction of return value
    # If getInstrument fails, the fee is 0, not 1.
    warning(paste("Instrument not found:", Symbol, ". Returning fees 0."))
    return(0)
  }
}
