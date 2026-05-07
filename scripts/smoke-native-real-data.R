#!/usr/bin/env Rscript

if (!requireNamespace("finharvest", quietly = TRUE)) {
  stop("Package 'finharvest' is required for real-data smoke checks.", call. = FALSE)
}

if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(backtestforge)
}

tickers <- strsplit(
  Sys.getenv("BT_SMOKE_TICKERS", "CCMFUT_1H_AGG,BGIFUT_1H_AGG,WDOFUT_1H_AGG,DI1F28"),
  ",",
  fixed = TRUE
)[[1]]
tickers <- trimws(tickers[nzchar(trimws(tickers))])
executions <- c("breakout", "same_close", "next_open", "next_close", "next_avg")
strict_metadata <- isTRUE(tolower(Sys.getenv("BT_SMOKE_STRICT_METADATA", "false")) %in% c("true", "1", "yes"))
fee_value <- if (strict_metadata) NULL else as.numeric(Sys.getenv("BT_SMOKE_FEE_VALUE", "4"))
fee_type <- if (strict_metadata) NULL else Sys.getenv("BT_SMOKE_FEE_TYPE", "contract")
slippage <- if (strict_metadata) NULL else as.numeric(Sys.getenv("BT_SMOKE_SLIPPAGE", "7"))
slippage_type <- if (strict_metadata) NULL else Sys.getenv("BT_SMOKE_SLIPPAGE_TYPE", "bps")

fetch_one <- function(ticker) {
  out <- tryCatch(
    finharvest::finget(
      ticker,
      assign = FALSE,
      single_xts = TRUE,
      consolidate = FALSE,
      mode = "raw",
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  if (xts::is.xts(out) && NROW(out) > 0) {
    return(out)
  }
  finharvest::finget(ticker)
}

run_one <- function(ticker, data, execution) {
  if (!xts::is.xts(data) || NROW(data) == 0) {
    stop(sprintf("No xts data returned for %s.", ticker), call. = FALSE)
  }
  res <- bt_eldoc(
    data,
    up = 25,
    down = 25,
    ps_value = 2,
    ps_type = "eldoc",
    execution = execution,
    fee = "normal",
    fee_value = fee_value,
    fee_type = fee_type,
    slippage = slippage,
    slippage_type = slippage_type,
    hide_details = TRUE
  )
  stopifnot(inherits(res, "bt_native_result"))
  stopifnot(xts::is.xts(res$rets))
  stopifnot(all(is.finite(as.numeric(res$rets))))
  stopifnot(all(is.finite(as.numeric(res$equity$Equity))))
  stopifnot(all(res$trades$fees >= 0, na.rm = TRUE))
  stopifnot(all(res$trades$slippage >= 0, na.rm = TRUE))
  data.frame(
    ticker = ticker,
    execution = execution,
    trades = NROW(res$trades),
    total_return = as.numeric(res$stats$total_return),
    max_drawdown = as.numeric(res$stats$max_drawdown),
    fees = as.numeric(res$stats$fees),
    slippage = as.numeric(res$stats$slippage),
    stringsAsFactors = FALSE
  )
}

rows <- list()
for (ticker in tickers) {
  data <- fetch_one(ticker)
  for (execution in executions) {
    rows[[length(rows) + 1L]] <- run_one(ticker, data, execution)
  }
}

out <- do.call(rbind, rows)
print(out, row.names = FALSE)
