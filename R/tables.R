#' Build a monthly returns table (percent)
#'
#' Aggregates a returns `xts` into a calendar-style monthly table with
#' year rows and month columns, plus a `Total` column. Values are formatted
#' as percentages for display.
#'
#' @param object_name An `xts` vector or 1-column `xts` of periodic returns.
#' @param return_data Logical; if `TRUE` returns the data.frame, otherwise prints.
#' @param geometric Logical; whether to compute geometric returns in totals.
#' @return If `return_data = TRUE`, a data.frame with year rows and monthly
#'   percent strings. Otherwise, printed output.
#' @keywords internal
.table_monthly_returns <- function(object_name, return_data = TRUE, geometric = TRUE) {
  return_man_mensal <- apply.monthly(object_name, colSums)
  colnames(return_man_mensal) <- "Year"
  return_man_mensal <- table.CalendarReturns(return_man_mensal, digits = 1, geometric = geometric)

  return_man_mensal_tabela <- as.data.frame(return_man_mensal)

  return_man_mensal_tabela[is.na(return_man_mensal_tabela)] <- ""

  for (col in colnames(return_man_mensal_tabela)) {
    return_man_mensal_tabela[[col]] <- sapply(return_man_mensal_tabela[[col]], function(x) {
      if (x != "") {
        paste0(x, "")
      } else {
        x
      }
    })
  }

  nomes_meses_pt <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")
  colnames(return_man_mensal_tabela) <- nomes_meses_pt

  if (return_data) {
    cat("--- Monthly Returns (Geometric) ---\n")
    return(return_man_mensal_tabela)
  }
}
.table_quarterly_returns <- function(returns_xts, return_data = TRUE, geometric = TRUE) {
  # Ensure input is xts
  if (!xts::is.xts(returns_xts)) stop("Input must be an xts object.")

  # 1. Calculate Quarterly Returns
  # We use apply.quarterly to aggregate based on geometric or simple logic
  q_ret <- xts::apply.quarterly(returns_xts, function(x) {
    if (geometric) prod(1 + x) - 1 else sum(x)
  })

  # 2. Calculate Annual Returns (for the Total column)
  a_ret <- xts::apply.yearly(returns_xts, function(x) {
    if (geometric) prod(1 + x) - 1 else sum(x)
  })

  # 3. Structure the Data for the Table
  years <- unique(format(zoo::index(q_ret), "%Y"))
  mat <- matrix("", nrow = length(years), ncol = 5)
  rownames(mat) <- years
  colnames(mat) <- c("Q1", "Q2", "Q3", "Q4", "Year")

  # Fill the matrix
  for (yr in years) {
    # Get quarters for this specific year
    this_year_q <- q_ret[yr]

    # Map xts quarters to matrix columns (1 to 4)
    # .indexmon returns 0-11. Q1=(0,1,2), Q2=(3,4,5), etc.
    # integer division by 3 gives 0,1,2,3 -> +1 gives 1,2,3,4.
    q_indices <- (as.numeric(format(zoo::index(this_year_q), "%m")) - 1) %/% 3 + 1

    vals <- as.numeric(this_year_q)

    # Format and place in matrix
    for (i in seq_along(vals)) {
      mat[yr, q_indices[i]] <- sprintf("%.2f%%", vals[i] * 100)
    }

    # Fill Total Year column
    if (!is.null(a_ret[yr]) && length(a_ret[yr]) > 0) {
      mat[yr, 5] <- sprintf("%.2f%%", as.numeric(a_ret[yr]) * 100)
    }
  }

  # 4. Visual Output (Console)
  cat(paste0("\n--- Quarterly Returns (", if (geometric) "Geometric" else "Simple", ") ---\n"))
  print(mat, quote = FALSE, right = TRUE)
  cat("\n")

  # 5. Return data if requested
  if (return_data) {
    return(as.data.frame(mat))
  }
}
#' Build a monthly profit table from blotter portfolio
#'
#' Sums daily Net.Trading.PL into monthly totals and returns a year-by-month
#' table with a `Total` column. Useful with `blotter` portfolio objects.
#'
#' @param object_name A blotter portfolio object with `$summary` and
#'   `Net.Trading.PL` column.
#' @param return_data Logical; if `TRUE` returns the data.frame, otherwise prints.
#' @return If `return_data = TRUE`, a data.frame of monthly profit totals.
#' @keywords internal
.table_monthly_profit <- function(object_name, return_data = TRUE) {
  daily_profit <- object_name$summary[, "Net.Trading.PL"]
  daily_profit <- daily_profit[-1]

  monthly_profit <- apply.monthly(daily_profit, sum)

  anos <- format(index(monthly_profit), "%Y")
  meses <- as.numeric(format(index(monthly_profit), "%m"))

  anos_unicos <- unique(anos)
  table_profit <- matrix(NA, nrow = length(anos_unicos), ncol = 13)
  rownames(table_profit) <- anos_unicos
  colnames(table_profit) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")

  for (i in 1:length(monthly_profit)) {
    ano_idx <- which(anos_unicos == anos[i])
    mes_idx <- meses[i]
    table_profit[ano_idx, mes_idx] <- as.numeric(monthly_profit[i])
  }

  for (i in 1:nrow(table_profit)) {
    table_profit[i, 13] <- sum(table_profit[i, 1:12], na.rm = TRUE)
  }

  lucro_mensal_tabela <- as.data.frame(table_profit)

  for (col in colnames(lucro_mensal_tabela)) {
    lucro_mensal_tabela[[col]] <- sapply(lucro_mensal_tabela[[col]], function(x) {
      if (is.na(x)) {
        ""
      } else {
        format(round(x), big.mark = ".", decimal.mark = ",", nsmall = 0)
      }
    })
  }

  if (return_data) {
    return(lucro_mensal_tabela)
  }
}
.table_quarterly_profit <- function(object_name, return_data = TRUE) {
  # 1. Extract P&L
  # Check if summary table and specific column exist
  if (is.null(object_name$summary) || !"Net.Trading.PL" %in% colnames(object_name$summary)) {
    stop("Object must contain $summary with 'Net.Trading.PL' column.")
  }

  daily_profit <- object_name$summary[, "Net.Trading.PL"]
  # Remove initial row if it is 0 or NA (common in backtest objects initialization)
  if (length(daily_profit) > 1) {
    daily_profit <- daily_profit[-1]
  }

  # 2. Aggregate to Quarterly
  # Sum daily profits by quarter
  q_profit <- xts::apply.quarterly(daily_profit, sum)

  # 3. Structure the Numeric Matrix
  years <- unique(format(zoo::index(q_profit), "%Y"))

  # Initialize a numeric matrix with NA
  mat_num <- matrix(NA_real_, nrow = length(years), ncol = 5)
  rownames(mat_num) <- years
  colnames(mat_num) <- c("Q1", "Q2", "Q3", "Q4", "Total")

  # Fill the numeric matrix
  for (yr in years) {
    # Extract data for this year
    this_year_q <- q_profit[yr]

    if (length(this_year_q) > 0) {
      # Calculate quarter index (1, 2, 3, or 4) based on month
      # Month 01-03 -> Q1, 04-06 -> Q2, etc.
      q_indices <- (as.numeric(format(zoo::index(this_year_q), "%m")) - 1) %/% 3 + 1
      vals <- as.numeric(this_year_q)

      # Assign values to the correct column
      for (i in seq_along(vals)) {
        mat_num[yr, q_indices[i]] <- vals[i]
      }
    }
  }

  # Calculate Total column (Row sums), ignoring NAs
  mat_num[, 5] <- rowSums(mat_num[, 1:4], na.rm = TRUE)

  # 4. Format the Matrix (Numeric -> String)
  # We create a character matrix for display
  mat_char <- matrix("", nrow = nrow(mat_num), ncol = ncol(mat_num))
  rownames(mat_char) <- rownames(mat_num)
  colnames(mat_char) <- colnames(mat_num)

  # Apply formatting cell by cell to avoid vector errors
  # Using standard loop for absolute safety against dimension drops
  for (r in 1:nrow(mat_num)) {
    for (c in 1:ncol(mat_num)) {
      val <- mat_num[r, c]
      if (!is.na(val)) {
        # Format: Round to integer, use dot for thousands, comma for decimal
        mat_char[r, c] <- format(round(val, 0), big.mark = ".", decimal.mark = ",", nsmall = 0, scientific = FALSE)
      } else {
        mat_char[r, c] <- ""
      }
    }
  }

  # 5. Visual Output (Console)
  cat("\n--- Quarterly Net Profit ---\n")
  print(mat_char, quote = FALSE, right = TRUE)
  cat("\n")

  # 6. Return data if requested
  if (return_data) {
    return(as.data.frame(mat_char))
  }
}

#' Print comparative stats table across backtest objects
#'
#' Recomputes and displays a concise table of performance and trade statistics
#' for one or more backtest result objects, aligning windows by the shortest
#' active trading period across inputs when requested.
#'
#' @param objects A list of result objects or a single object; trades are
#'   detected from common transactions/trades elements or `xts` inputs.
#' @param colored_lines List of row indices or ranges to color in the output.
#' @param lines_colors Character vector of color names matching `colored_lines`.
#'   Supported: `gray`, `red`, `green`, `blue`, `cyan`, `yellow`, `magenta`.
#' @param col Integer; number of symbol columns per printed block (paging).
#' @param align_to_shortest Logical; align all stats to the shortest active
#'   trading window across inputs.
#' @param sharpe_scale Numeric; scaling factor for daily Sharpe (e.g. 252/365).
#' @param verbose Logical; print alignment diagnostics.
#'
#' @return Invisibly returns a wide data.frame of statistics used for display.
#' @keywords internal
#' @export
bt_stats <- function(objects, colored_lines = list(1:3, 4:7, 8:10, 11:13, 14:16, 17:26), lines_colors = c("gray", "gray", "red", "green", "blue", "gray"), col = 8, align_to_shortest = TRUE, sharpe_scale = 365, verbose = TRUE) {
  if (!is.list(objects)) objects <- as.list(objects)

  # Find trades xts inside the object
  get_trades_xts <- function(obj) {
    if (!is.null(obj$trades) && xts::is.xts(obj$trades)) {
      return(obj$trades)
    }
    for (nm in c("txn", "transactions", "Trades", "TXN")) {
      if (!is.null(obj[[nm]]) && xts::is.xts(obj[[nm]])) {
        return(obj[[nm]])
      }
    }
    if (xts::is.xts(obj)) {
      return(obj)
    }
    NULL
  }

  # Optional: get stats df for constants
  get_stats_df <- function(obj) {
    if (!is.null(obj$stats)) {
      return(as.data.frame(obj$stats))
    }
    st_attr <- attr(obj, "stats", exact = TRUE)
    if (!is.null(st_attr)) {
      return(as.data.frame(st_attr))
    }
    NULL
  }

  # Detect columns in trades (case-insensitive)
  detect_trade_cols <- function(tr) {
    cn <- colnames(tr)
    lc <- tolower(cn)
    pick <- function(cands) {
      for (nm in cands) {
        w <- which(lc == tolower(nm))
        if (length(w) == 1) {
          return(cn[w])
        }
      }
      NULL
    }
    list(
      qty = pick(c("Txn.Qty", "Qty", "Quantity", "Pos.Qty", "Position.Qty", "Position", "Pos", "Units", "Size", "Order.Qty")),
      pl = pick(c("Net.Txn.Realized.PL", "Net.Txn.PL", "Txn.Realized.PL", "Realized.PL", "Net.Realized.PL", "RealizedPL", "PL")),
      fees = pick(c("Txn.Fees", "Txn.Fee", "Fees", "Commission", "CommissionAmt")),
      price = pick(c("Txn.Price", "Price")),
      value = pick(c("Txn.Value", "Value"))
    )
  }

  # Build qty vector (exact or fallback from Value/Price)
  get_qty_full <- function(tr, cols, tol = 1e-10) {
    n <- NROW(tr)
    if (!is.null(cols$qty)) {
      q <- as.numeric(tr[, cols$qty])
      q[is.na(q)] <- 0
      return(q)
    }
    if (!is.null(cols$value) && !is.null(cols$price)) {
      val <- as.numeric(tr[, cols$value])
      val[is.na(val)] <- 0
      prc <- as.numeric(tr[, cols$price])
      prc[is.na(prc) | abs(prc) < tol] <- NA_real_
      q <- val / prc
      q[is.na(q)] <- 0
      return(q)
    }
    rep(0, n)
  }

  # Find active window of trades (ignore pure-zero placeholders)
  find_active_range <- function(tr, cols) {
    idx <- zoo::index(tr)
    has <- rep(FALSE, NROW(tr))
    if (!is.null(cols$qty)) has <- has | (abs(as.numeric(tr[, cols$qty])) > 0)
    if (!is.null(cols$pl)) has <- has | (abs(as.numeric(tr[, cols$pl])) != 0)
    if (!is.null(cols$value)) has <- has | (abs(as.numeric(tr[, cols$value])) != 0)
    if (!any(has)) {
      return(c(NA, NA))
    }
    where <- which(has)
    c(idx[min(where)], idx[max(where)])
  }

  # Labels
  get_symbol <- function(obj, tr) {
    st <- get_stats_df(obj)
    if (!is.null(st) && "Symbol" %in% names(st)) {
      sym <- unique(as.character(st$Symbol))
      if (length(sym) > 0) {
        return(sym[1])
      }
    }
    sattr <- attr(tr, "symbol")
    if (!is.null(sattr)) {
      return(as.character(sattr))
    }
    cn <- colnames(tr)
    if (!is.null(cn) && length(cn) > 0) {
      return(as.character(cn[1]))
    }
    "Symbol"
  }
  get_portfolio <- function(obj) {
    st <- get_stats_df(obj)
    if (!is.null(st) && "Portfolio" %in% names(st)) {
      pf <- unique(as.character(st$Portfolio))
      if (length(pf) > 0) {
        return(pf[1])
      }
    }
    NA_character_
  }

  # Constants to pass through
  constant_fields <- c("elDoc", "PosSiz", "Multiplier", "TickSize", "Slippage", "Fees", "Fee.n.Slip")
  grab_constant_fields <- function(obj) {
    st <- get_stats_df(obj)
    if (is.null(st)) {
      return(NULL)
    }
    have <- intersect(names(st), constant_fields)
    if (length(have) == 0) {
      return(NULL)
    }
    as.list(st[1, have, drop = FALSE])
  }

  # Sign with tolerance
  sgn <- function(x, tol = 1e-10) {
    z <- rep(0L, length(x))
    z[x > tol] <- 1L
    z[x < -tol] <- -1L
    z
  }

  # Build trade segments across the FULL series, closing on flat or reversal
  build_trade_segments_full <- function(tr, qty_full, pl_full) {
    n <- length(qty_full)
    if (n == 0) {
      return(data.frame(
        start_idx = integer(0), end_idx = integer(0),
        start_time = as.POSIXct(character(0)),
        end_time = as.POSIXct(character(0)),
        pl = numeric(0)
      ))
    }
    qty_full[is.na(qty_full)] <- 0
    pl_full[is.na(pl_full)] <- 0

    pos <- cumsum(qty_full)
    segs <- list()
    open_idx <- NA_integer_

    for (i in seq_len(n)) {
      cur_pos <- pos[i]
      prev_pos <- if (i > 1) pos[i - 1] else 0
      cur_s <- sgn(cur_pos)
      prev_s <- sgn(prev_pos)

      if (is.na(open_idx)) {
        if (cur_s != 0L) open_idx <- i
      } else {
        if (cur_s == 0L) {
          segs[[length(segs) + 1]] <- list(start = open_idx, end = i)
          open_idx <- NA_integer_
        } else if (prev_s != 0L && cur_s != prev_s) {
          segs[[length(segs) + 1]] <- list(start = open_idx, end = i)
          open_idx <- i
        }
      }
    }

    if (length(segs) == 0) {
      return(data.frame(
        start_idx = integer(0), end_idx = integer(0),
        start_time = as.POSIXct(character(0)),
        end_time = as.POSIXct(character(0)),
        pl = numeric(0)
      ))
    }

    idx_time <- zoo::index(tr)
    start_idx <- vapply(segs, `[[`, integer(1), "start")
    end_idx <- vapply(segs, `[[`, integer(1), "end")
    seg_pl <- vapply(seq_along(start_idx), function(k) {
      i1 <- start_idx[k]
      i2 <- end_idx[k]
      sum(pl_full[i1:i2], na.rm = TRUE)
    }, numeric(1))

    data.frame(
      start_idx = start_idx,
      end_idx = end_idx,
      start_time = idx_time[start_idx],
      end_time = idx_time[end_idx],
      pl = seg_pl,
      stringsAsFactors = FALSE
    )
  }

  # Compute stats from trades for a given window
  compute_stats_from_trades <- function(obj, start_date, end_date) {
    tr <- get_trades_xts(obj)
    if (is.null(tr)) {
      return(NULL)
    }
    cols <- detect_trade_cols(tr)

    qty_full <- get_qty_full(tr, cols)
    pl_full <- if (!is.null(cols$pl)) as.numeric(tr[, cols$pl]) else rep(0, NROW(tr))
    pl_full[is.na(pl_full)] <- 0

    seg_full <- build_trade_segments_full(tr, qty_full, pl_full)

    idx_time <- zoo::index(tr)
    in_win <- (idx_time >= as.POSIXct(start_date)) & (idx_time <= as.POSIXct(end_date))

    pl_txn_win <- pl_full[in_win]
    qty_win <- qty_full[in_win]
    n_txn_win <- if (any(qty_full != 0)) sum(abs(qty_win) > 1e-10) else sum(abs(pl_txn_win) > 0)

    # Window equity/drawdown
    equity <- cumsum(pl_txn_win)
    max_eq <- if (length(equity)) max(equity, na.rm = TRUE) else 0
    min_eq <- if (length(equity)) min(equity, na.rm = TRUE) else 0
    end_eq <- if (length(equity)) tail(equity, 1) else 0
    dd_series <- equity - cummax(replace(equity, is.na(equity), 0))
    max_dd <- if (length(dd_series)) min(dd_series, na.rm = TRUE) else 0
    profit_to_maxdd <- if (!is.na(max_dd) && max_dd != 0) end_eq / abs(max_dd) else NA_real_

    # Daily PL inside the window
    if (length(pl_txn_win) > 0) {
      daily_pl <- tapply(pl_txn_win, as.Date(idx_time[in_win]), sum, na.rm = TRUE)
      daily_pl <- as.numeric(daily_pl)
    } else {
      daily_pl <- numeric(0)
    }
    avg_daily <- if (length(daily_pl) > 0) mean(daily_pl) else NA_real_
    med_daily <- if (length(daily_pl) > 0) stats::median(daily_pl) else NA_real_
    sd_daily <- if (length(daily_pl) > 1) stats::sd(daily_pl) else NA_real_
    se_daily <- if (length(daily_pl) > 1) sd_daily / sqrt(length(daily_pl)) else NA_real_
    ann_sharpe <- if (!is.na(sd_daily) && sd_daily > 0 && !is.na(avg_daily)) (avg_daily / sd_daily) * sqrt(sharpe_scale) else NA_real_

    # Trade-level metrics (segments fully inside window)
    if (nrow(seg_full) > 0) {
      seg_win <- seg_full[seg_full$start_time >= as.POSIXct(start_date) & seg_full$end_time <= as.POSIXct(end_date), , drop = FALSE]
      trade_pnl <- seg_win$pl
    } else {
      trade_pnl <- numeric(0)
    }

    num_trades <- length(trade_pnl)
    gross_profits <- sum(trade_pnl[trade_pnl > 0], na.rm = TRUE)
    gross_losses <- sum(trade_pnl[trade_pnl < 0], na.rm = TRUE)
    largest_winner <- if (num_trades > 0) max(trade_pnl, na.rm = TRUE) else NA_real_
    largest_loser <- if (num_trades > 0) min(trade_pnl, na.rm = TRUE) else NA_real_
    avg_trade_pl <- if (num_trades > 0) mean(trade_pnl, na.rm = TRUE) else NA_real_
    med_trade_pl <- if (num_trades > 0) stats::median(trade_pnl, na.rm = TRUE) else NA_real_
    sd_trade_pl <- if (num_trades > 1) stats::sd(trade_pnl, na.rm = TRUE) else NA_real_
    se_trade_pl <- if (num_trades > 1) sd_trade_pl / sqrt(num_trades) else NA_real_

    wins <- trade_pnl[trade_pnl > 0]
    losses <- trade_pnl[trade_pnl < 0]
    pct_pos <- if (num_trades > 0) 100 * (length(wins) / num_trades) else NA_real_
    pct_neg <- if (num_trades > 0) 100 * (length(losses) / num_trades) else NA_real_
    profit_factor <- if (gross_losses < 0) gross_profits / abs(gross_losses) else NA_real_
    avg_win <- if (length(wins) > 0) mean(wins) else NA_real_
    med_win <- if (length(wins) > 0) stats::median(wins) else NA_real_
    avg_loss <- if (length(losses) > 0) mean(losses) else NA_real_
    med_loss <- if (length(losses) > 0) stats::median(losses) else NA_real_
    avg_wl_ratio <- if (!is.na(avg_win) && !is.na(avg_loss) && avg_loss < 0) avg_win / abs(avg_loss) else NA_real_
    med_wl_ratio <- if (!is.na(med_win) && !is.na(med_loss) && med_loss < 0) med_win / abs(med_loss) else NA_real_

    sym <- get_symbol(obj, tr)
    pf <- get_portfolio(obj)

    out <- data.frame(
      Portfolio = pf,
      Symbol = sym,
      Num.Txns = as.integer(n_txn_win),
      Num.Trades = as.integer(num_trades),
      Net.Trading.PL = sum(pl_txn_win, na.rm = TRUE),
      Avg.Trade.PL = avg_trade_pl,
      Med.Trade.PL = med_trade_pl,
      Largest.Winner = largest_winner,
      Largest.Loser = largest_loser,
      Gross.Profits = gross_profits,
      Gross.Losses = gross_losses,
      Std.Dev.Trade.PL = sd_trade_pl,
      Std.Err.Trade.PL = se_trade_pl,
      Percent.Positive = pct_pos,
      Percent.Negative = pct_neg,
      Profit.Factor = profit_factor,
      Avg.Win.Trade = avg_win,
      Med.Win.Trade = med_win,
      Avg.Losing.Trade = avg_loss,
      Med.Losing.Trade = med_loss,
      Avg.Daily.PL = avg_daily,
      Med.Daily.PL = med_daily,
      Std.Dev.Daily.PL = sd_daily,
      Std.Err.Daily.PL = se_daily,
      Ann.Sharpe = ann_sharpe,
      Max.Drawdown = max_dd,
      Profit.To.Max.Draw = profit_to_maxdd,
      Avg.WinLoss.Ratio = avg_wl_ratio,
      Med.WinLoss.Ratio = med_wl_ratio,
      Max.Equity = max_eq,
      Min.Equity = min_eq,
      End.Equity = end_eq,
      stringsAsFactors = FALSE
    )

    # Merge constants and fees
    cf <- grab_constant_fields(obj)
    if (!is.null(cf)) out[names(cf)] <- cf
    if (!("Fee.n.Slip" %in% names(out)) && !is.null(cols$fees)) {
      out$Fee.n.Slip <- sum(as.numeric(tr[in_win, cols$fees]), na.rm = TRUE)
    }

    out
  }

  # --- Determine alignment window from trades (shortest active duration
  target_start <- NULL
  target_end <- NULL
  if (align_to_shortest) {
    ranges <- list()
    for (i in seq_along(objects)) {
      tr <- get_trades_xts(objects[[i]])
      if (is.null(tr)) next
      cols <- detect_trade_cols(tr)
      rng <- find_active_range(tr, cols)
      if (!any(is.na(rng))) {
        ranges[[length(ranges) + 1]] <- data.frame(
          obj_id = i,
          start = as.POSIXct(rng[1]),
          end = as.POSIXct(rng[2]),
          duration_days = as.numeric(difftime(as.POSIXct(rng[2]), as.POSIXct(rng[1]), units = "days")),
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(ranges) > 0) {
      ranges_df <- dplyr::bind_rows(ranges)
      shortest <- ranges_df[which.min(ranges_df$duration_days), ]
      target_start <- shortest$start
      target_end <- shortest$end
      if (verbose) {
        message(sprintf(
          "Aligning to shortest trades window: [%s -> %s] (~%.0f days)",
          as.character(target_start), as.character(target_end), shortest$duration_days
        ))
      }
    } else if (verbose) {
      message("No trades ranges detected. Skipping alignment.")
    }
  }

  # --- Recompute stats for each object on target window (or on its own window)
  stats_list <- list()
  for (i in seq_along(objects)) {
    tr <- get_trades_xts(objects[[i]])
    if (is.null(tr)) next
    cols <- detect_trade_cols(tr)
    if (!is.null(target_start) && !is.null(target_end)) {
      st <- compute_stats_from_trades(objects[[i]], target_start, target_end)
    } else {
      rng <- find_active_range(tr, cols)
      if (any(is.na(rng))) next
      st <- compute_stats_from_trades(objects[[i]], as.POSIXct(rng[1]), as.POSIXct(rng[2]))
    }
    if (!is.null(st)) stats_list[[length(stats_list) + 1]] <- st
  }
  if (length(stats_list) == 0) stop("No stats could be recomputed from trades.")

  stats_df <- dplyr::bind_rows(stats_list)

  # --- Display pipeline (simplified and consistent)

  # Long -> wide directly (one value per stat_name x Symbol). No list/unnest.
  stats_long <- stats_df %>%
    tidyr::pivot_longer(
      cols = -c(Symbol, Portfolio),
      names_to = "stat_name",
      values_to = "value",
      values_transform = list(value = as.character) # force single type
    ) %>%
    dplyr::select(Symbol, stat_name, value)

  stats_wide <- stats_long %>%
    tidyr::pivot_wider(names_from = Symbol, values_from = value)

  # Remove items not to display and rename labels
  stats_wide <- stats_wide %>%
    dplyr::filter(
      stat_name != "Ann.Sharpe",
      stat_name != "Ann.Calmar",
      stat_name != "Avg.Daily.PL",
      stat_name != "Avg.WinLoss.Ratio",
      stat_name != "Med.Daily.PL",
      stat_name != "Med.Losing.Trade",
      stat_name != "Med.Trade.PL",
      stat_name != "Med.Win.Trade",
      stat_name != "Med.WinLoss.Ratio",
      stat_name != "Num.Txns",
      stat_name != "Profit.Factor",
      stat_name != "Net.Trading.PL",
      stat_name != "Std.Dev.Trade.PL",
      stat_name != "Std.Err.Daily.PL",
      stat_name != "Std.Err.Trade.PL"
    ) %>%
    dplyr::mutate(
      stat_name = dplyr::case_when(
        stat_name == "Avg.Win.Trade" ~ "Avg.Win",
        stat_name == "Avg.Losing.Trade" ~ "Avg.Loss",
        stat_name == "Avg.Trade.PL" ~ "Avg.Trade",
        stat_name == "End.Equity" ~ "Profit",
        stat_name == "Std.Dev.Daily.PL" ~ "Daily.Std.Dev",
        stat_name == "Max.Drawdown" ~ "Max.DD",
        stat_name == "Profit.To.Max.Draw" ~ "Profit/Max.DD",
        stat_name == "Percent.Negative" ~ "% Loss",
        stat_name == "Percent.Positive" ~ "% Win",
        TRUE ~ stat_name
      )
    )

  # Order rows
  desired_order <- c(
    "elDoc", "PosSiz", "Multiplier", "TickSize", "Slippage",
    "Fees", "Fee.n.Slip", "Profit", "Max.DD", "Profit/Max.DD",
    "Avg.Win", "Avg.Loss", "Avg.Trade", "Num.Trades", "% Win", "% Loss"
  )
  stats_wide <- stats_wide %>%
    dplyr::mutate(
      order_key = dplyr::case_when(
        stat_name %in% desired_order ~ match(stat_name, desired_order),
        TRUE ~ length(desired_order) + 1
      )
    ) %>%
    dplyr::arrange(order_key, stat_name) %>%
    dplyr::select(-order_key)

  # Format numbers with 2 decimals consistently (except for elDoc/PosSiz/Slippage)
  format_numeric_cells <- function(x, stat_name) {
    # x is a scalar; stat_name is the corresponding row label
    if (is.null(x) || is.na(x)) {
      return("")
    }
    x_chr <- as.character(x)
    num <- suppressWarnings(as.numeric(x_chr))
    if (!is.na(num) && !grepl("^(PosSiz|Slippage|elDoc)$", stat_name)) {
      return(formatC(num, format = "f", digits = 2))
    }
    x_chr
  }

  stat_names_vec <- stats_wide$stat_name
  for (cname in setdiff(names(stats_wide), "stat_name")) {
    stats_wide[[cname]] <- mapply(
      FUN = format_numeric_cells,
      x = stats_wide[[cname]],
      stat_name = stat_names_vec,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
  }

  # ANSI colors
  colors_ansi <- c(
    red = "\033[31m",
    green = "\033[32m",
    blue = "\033[34m",
    gray = "\033[90m",
    cyan = "\033[36m",
    yellow = "\033[33m",
    magenta = "\033[35m",
    reset = "\033[0m"
  )

  # Row colors
  if (!is.null(colored_lines)) {
    expanded_lines <- list()
    for (i in seq_along(colored_lines)) {
      item <- colored_lines[[i]]
      if (is.numeric(item)) {
        expanded_lines[[i]] <- item
      } else if (is.integer(item) && length(item) > 1) {
        expanded_lines[[i]] <- item[1]:item[length(item)]
      } else if (is.character(item) && grepl(":", item)) {
        intervalo <- as.integer(strsplit(item, ":")[[1]])
        expanded_lines[[i]] <- intervalo[1]:intervalo[2]
      } else {
        stop("Invalid format in 'colored_lines'. Use numbers, ranges (e.g., 1:5), or integer vectors.")
      }
    }
    lines_to_color <- unlist(expanded_lines)
    if (!is.null(lines_colors)) {
      if (length(lines_colors) > length(expanded_lines)) {
        stop("Number of colors cannot be greater than the number of provided intervals.")
      }
      expanded_colors <- rep(lines_colors, sapply(expanded_lines, length))
      color_to_lines <- expanded_colors
    } else {
      color_to_lines <- rep("cyan", length(lines_to_color))
    }
  } else {
    lines_to_color <- numeric()
    color_to_lines <- character()
  }

  # Column widths and printing
  all_data_for_width <- rbind(names(stats_wide), as.matrix(stats_wide))
  global_width <- apply(all_data_for_width, 2, function(col) {
    max(nchar(as.character(col)), na.rm = TRUE)
  })

  print_table_block <- function(df_block) {
    header <- paste(sapply(names(df_block), function(nome_col) {
      format(nome_col, width = global_width[nome_col], justify = "right")
    }), collapse = "  ")
    cat(header, "\n")
    for (i in 1:nrow(df_block)) {
      line <- df_block[i, ]
      line_text <- paste(
        sapply(1:length(line), function(j) {
          valor <- if (is.na(line[[j]]) || is.null(line[[j]])) "" else as.character(line[[j]])
          format(valor, width = global_width[names(df_block)[j]], justify = "right")
        }),
        collapse = "  "
      )
      current_color <- colors_ansi["reset"]
      if (i %in% lines_to_color) {
        color_index <- which(lines_to_color == i)
        color_name <- color_to_lines[color_index]
        current_color <- colors_ansi[color_name]
      }
      cat(current_color, line_text, colors_ansi["reset"], "\n", sep = "")
    }
  }

  if (is.null(col)) {
    print_table_block(stats_wide)
  } else {
    all_cols <- names(stats_wide)
    fixed_col <- "stat_name"
    other_cols <- setdiff(all_cols, fixed_col)
    num_blocks <- ceiling(length(other_cols) / col)
    for (block_i in 1:num_blocks) {
      start_idx <- (block_i - 1) * col + 1
      end_idx <- min(block_i * col, length(other_cols))
      block_cols <- other_cols[start_idx:end_idx]
      df_block <- stats_wide[, c(fixed_col, block_cols), drop = FALSE]
      print_table_block(df_block)
      cat("\n")
    }
  }

  invisible(stats_wide)
}
