run_bt_eldoc <- function(ticker, up = 40, down = 40, ps_risk_value = 2, ps = "pct", fee = "normal", start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, geometric = TRUE, verbose = FALSE, only_returns = FALSE, hide_details = TRUE, plot = FALSE) {

  # Remove todos os objetos dos ambientes
  if(exists('.blotter')) rm(list = ls(envir = .blotter), envir = .blotter)
  if(exists('.strategy')) rm(list = ls(envir = .strategy), envir = .strategy)

  if(!exists(".strategy")) .strategy <<- new.env()
  if(!exists(".blotter")) .blotter <<- new.env()

  if(exists(ticker, envir = .GlobalEnv)) {
    ticker_data <- get(ticker)
    future(ticker, tick_size = attr(ticker_data, "fut_tick_size"), multiplier = attr(ticker_data, "fut_multiplier"), maturity = attr(ticker_data, "maturity"),currency ="USD")
  } else {
    ticker_data <- sm_get_data(ticker,start_date=start_date,end_date=end_date,future_history=FALSE, single_xts = TRUE, local_data=FALSE)
  }
  ticker_data <- .use_close_only(ticker_data)
  original_ticker <- ticker

  up_str <- as.character(up)
  down_str <- as.character(down)

  bt_ticker <- NULL

  if(!hide_details){
    if(long && short) {
      bt_ticker <- paste0(ticker, "_eD_", up_str, "_", down_str, "_",ps,"_",ps_risk_value)
    } else if(long && !short) {
      bt_ticker <- paste0(ticker, "_eD_", up_str, "_", down_str, "_",ps,"_",ps_risk_value)
    } else if(!long && short) {
      bt_ticker <- paste0(ticker, "_eD_", up_str, "_", down_str, "_",ps,"_",ps_risk_value)
    } else {
      stop("You need to be long, short or both!")
    }
  } else {
    if(long && short) {
      bt_ticker <- paste0(ticker, "_eD_nodets")
    } else if(long && !short) {
      bt_ticker <- paste0(ticker, "_eD_nodets")
    } else if(!long && short) {
      bt_ticker <- paste0(ticker, "_eD_nodets")
    } else {
      stop("You need to be long, short or both!")
    }
  }

  instrument_attr(ticker, "primary_id", bt_ticker)
  ticker <- bt_ticker

  verbose = TRUE
  initEq <- 10000000
  path.dependence <- TRUE

  portfolio.st = 'elDoc'
  account.st = 'elDoc'
  initPortf(portfolio.st, symbols = ticker)
  initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
  initOrders(portfolio = portfolio.st)
  my_strategy <- strategy(portfolio.st)

  tradeSize <- 9999999
  bcontracts <- 1
  scontracts <- -1

  con <- quote(.psFixedContractsQty)
  per <- quote(.psEquityPercentage)
  pct <- quote(.psEquityPercentageDonchian)
  di <- quote(.psEquityPercentageDonchian_DI)

  isDI <- startsWith(ticker,"DI1")

  if(isDI){
    PositionSizing <- function(data, timestamp, orderqty, ordertype, orderside,
                               portfolio, symbol, tradeSize, maxSize, ...) {
      return(.psEquityPercentageDonchian_DI(data, timestamp, orderqty, ordertype, orderside,
                                   portfolio, symbol, tradeSize, maxSize,
                                   risk = ps_risk_value, ...))
    }
    print("Position Sizing set to 'DI'")
  } else {
    PositionSizing <- function(data, timestamp, orderqty, ordertype, orderside,
                               portfolio, symbol, tradeSize, maxSize, ...) {
      return(.psEquityPercentageDonchian(data, timestamp, orderqty, ordertype, orderside,
                                portfolio, symbol, tradeSize, maxSize,
                                risk = ps_risk_value, ...))
    }
  }

  OrderType <- 'market'

  LongEnabled <- long
  ShortEnabled <- short

  HighCol <- "High"
  LowCol  <- "Low"
  if ("PU_o" %in% colnames(ticker_data)) Preference <- "PU_o" else Preference <- "Open"

  assign(bt_ticker, ticker_data, envir = parent.frame())
  ReplaceBuy <- FALSE
  ReplaceSell <- FALSE
  ReplaceShort <- FALSE
  ReplaceCover <- FALSE

  docx = up
  docy = down

  if(fee == "nofee"){
    TxnFeesVal <- 0
  } else {
    TxnFeesVal <- ".calculate_fees"
  }

  my_strategy <- add.indicator(
    my_strategy, "eldoc",
    arguments = list(
      ticker = quote(mktdata),
      x      = docx,
      y      = docy,
      hi.col = HighCol,    # <- PU_h if DI
      lo.col = LowCol,     # <- PU_l if DI
      type   = "data"),
    label = "el")

  coisa <- applyIndicators(strategy = my_strategy, mktdata = get(ticker))

  my_strategy <-   add.signal(strategy = my_strategy,
                             name = "sigCrossover",
                             arguments = list(
                               data = quote(mktdata),
                               columns = c(HighCol, "X.el"),
                               relationship = "gte"),
                             label = "Entry")

  my_strategy <-  add.signal(strategy = my_strategy,
                            name = "sigCrossover",
                            arguments = list(
                              data = quote(mktdata),
                              columns = c(LowCol, "Y.el"),
                              relationship = "lte"),
                            label = "Exit")
  coisa <- applySignals(strategy = my_strategy, mktdata = coisa)

  n.ent  <- sum(coisa$Entry == 1, na.rm = TRUE)
  n.saida<- sum(coisa$Exit   == 1, na.rm = TRUE)
  .dbg("signals   Entry:", n.ent, " Exit:", n.saida)

  my_strategy <-  add.rule(strategy = my_strategy,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Entry",
                            sigval = TRUE,
                            datax = mktdata,
                            initEq = initEq,
                            orderqty = tradeSize,
                            portfolio = portfolio.st,
                            ordertype = OrderType,
                            orderside = if(!isDI) 'long' else 'short',
                            osFUN = PositionSizing,
                            tradeSize = tradeSize,
                            buyorderqty = bcontracts,
                            sellorderqty = scontracts,
                            maxSize = 9999999,
                            prefer = Preference,
                            replace = ReplaceBuy,
                            TxnFees = TxnFeesVal
                          ),
                          type = if(invert_signals) 'exit' else 'enter',
                          label = if(invert_signals) 'exitLong' else 'enterLong',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = LongEnabled)

  my_strategy <-  add.rule(strategy = my_strategy,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Exit",
                            sigval = TRUE,
                            orderqty = "all",
                            ordertype = OrderType,
                            orderside = if(!isDI) 'long' else 'short',
                            prefer = Preference,
                            replace = ReplaceSell,
                            TxnFees = TxnFeesVal
                          ),
                          type = if(invert_signals) 'enter' else 'exit',
                          label = if(invert_signals) 'enterLong' else 'exitLong',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = LongEnabled)

  my_strategy <-  add.rule(strategy = my_strategy,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Exit",
                            sigval = TRUE,
                            datax = mktdata,
                            initEq = initEq,
                            orderqty = tradeSize,
                            portfolio = portfolio.st,
                            ordertype = OrderType,
                            orderside = if(isDI) 'long' else 'short',
                            osFUN = PositionSizing,
                            tradeSize = -tradeSize,
                            buyorderqty = bcontracts,
                            sellorderqty = scontracts,
                            maxSize = -9999999,
                            prefer = Preference,
                            replace = ReplaceShort,
                            TxnFees = TxnFeesVal
                          ),
                          type = if(invert_signals) 'exit' else 'enter',
                          label = if(invert_signals) 'exitShort' else 'enterShort',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = ShortEnabled)

  my_strategy <-  add.rule(strategy = my_strategy,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Entry",
                            sigval = TRUE,
                            orderqty = "all",
                            ordertype = OrderType,
                            orderside = if(isDI) 'long' else 'short',
                            prefer = Preference,
                            replace = ReplaceCover,
                            TxnFees = TxnFeesVal
                          ),
                          type = if(invert_signals) 'enter' else 'exit',
                          label = if(invert_signals) 'enterShort' else 'exitShort',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = ShortEnabled)

  start_t <- Sys.time()
  getInstrument(ticker)
  # run Backtest
  applyStrategy(strategy = my_strategy, portfolios = portfolio.st, verbose = FALSE, initEq = initEq, mdenv = parent.frame())

  # aftere applyStrategy
  table(mktdata$Entrada, mktdata$Saida)
  tx <- getTxns(portfolio.st, ticker)
  .dbg("generated orders:", nrow(tx))
  if (nrow(tx) == 0) {
    warning("No generated order. Check column passed to 'prefer' ",
            "and if the instrument has a multiplier/tick_size defined.")
    return(invisible(NULL))
  }
  updatePortf(Portfolio = 'elDoc', prefer = Preference)
  updateAcct(name = 'elDoc')
  updateEndEq(Account = 'elDoc')

  getTxns('elDoc', ticker)

  port = getPortfolio('elDoc')
  book    = getOrderBook('elDoc')
  stats   = tradeStats('elDoc')
  ptstats = perTradeStats('elDoc')
  ptrets  = PortfReturns('elDoc')
  acrets  = AcctReturns('elDoc')
  txns    = getTxns('elDoc', ticker)
  Fee.n.Slip <- sum(txns$Txn.Fees)
  stats$Fee.n.Slip <- Fee.n.Slip

  # show results
  cat(paste0("Results for ", ticker, " - elDoc ", up,"/",down,"\n\n"))
  if(verbose == TRUE) {
    print(stats)
    cat("\n")
    print(txns)
    cat("\n")
  }
  tab <- .table_monthly_returns(ptrets, return_data = TRUE, geometric = geometric)
  print(tab)
  tab_rs <- .table_monthly_profit(port)
  print(tab_rs)

  cat("\nAnnualized Returns:", Return.annualized(ptrets, geometric = geometric), "\n")
  cat("Cumulative Returns:", Return.cumulative(ptrets, geometric = geometric), "\n")
  index(ptrets) <- .convert_posixct(ptrets)
  index(txns) <- .convert_posixct(txns)
  index(acrets) <- .convert_posixct(acrets)
  print(ptrets)
  colnames(ptrets) <- "Discrete"
  ptrets$Log <- log(1 + ptrets$Discrete)
  print(ptrets)
  colnames(ptrets) <- c("Discrete","Log")

  stop_t <- Sys.time()
  cat(paste("\nRuntime:", stop_t - start_t))
  cat("\n----------------------------------------\n\n")

  if(only_returns) {
    return(ptrets)
  }

  stats$elDoc <- paste0(up,"/",down)
  stats$PosSiz <- ps
  stats$Multiplier <- ticker_data$multiplier
  stats$TickSize <- ticker_data$tick_size
  if(fee == "nofee"){
    stats$Slippage <- 0
    stats$Fees <- 0
  } else {
    stats$Slippage <- ticker_data$identifiers$slippage
    stats$Fees <- ticker_data$identifiers$fees
  }

  attr(ptrets, "backtest") <- TRUE
  attr(ptrets, "local") <- TRUE

  elements_names <- c("rets","stats","trades","rets_acct","mktdata")

  results <- setNames(
    list(ptrets, stats, txns, acrets, mktdata),
    elements_names
  )
  #rm(list = ticker, envir = .GlobalEnv)  # limpa o dado grande
  #rm(mktdata, envir = .GlobalEnv)
  #  assign("results",results, envir = .GlobalEnv)
  # assign("results",results, envir = .GlobalEnv)
  if(plot) tplot(portfolio.st,original_ticker)

  #tplot(results[[1]],benchs="DOLAR")

  return(results)
}
