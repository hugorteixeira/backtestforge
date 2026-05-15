# 📈 backtestforge

> **⚠️ WORK IN PROGRESS - EXPECT BUGS!** ⚠️

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=flat&logo=r&logoColor=white)
![Status: Beta](https://img.shields.io/badge/status-beta-orange)

A fast R library for trend-following backtests on stocks, futures, and crypto.
Backtests run through the native in-memory simulator; there is no external
portfolio/instrument engine to configure.

## 🚀 Features

- **Native In-Memory Simulator** - Runs directly from `xts` data and attrs
- **Basic ElDoc / Donchian Strategy** - Conservative breakout wrapper for production comparisons
- **Experimental ElDoc Research Surface** - Pyramiding and trade-excursion diagnostics live in `bt_eldoc_exp()`
- **EMA/SMA Crossover Strategies** - Lightweight moving-average wrappers
- **Time-Series Momentum** - Native `bt_tsmom()` wrapper for trailing-return sign systems
- **Smart Position Sizing** - Risk-based position sizing with multiple methods
- **Multi-Asset Support** - Works with stocks, futures, and crypto
- **Brazilian Futures Support** - Optional `brfutures` integration for DI PU conversion; contract metadata is read from `xts` attrs
- **Agent-Friendly Search** - `bt_search_native()` evaluates serializable parameter grids
- **Visual Backtesting** - Optional plotting with `tradeplotr`
- **Performance Reporting** - Built-in monthly, quarterly, and summary tables

## Current Native Engine Notes

The current implementation plan, benchmark snapshot, parity findings, and next
session handoff are documented in
[`docs/native-engine-status-and-handoff.md`](docs/native-engine-status-and-handoff.md).
Read that file before continuing engine, DI futures, or server-job work.

## ⚠️ Important Disclaimer

> This library is **HEAVILY WORK IN PROGRESS** and contains **LOTS OF BUGS**! 
> 
> 🔧 The native wrappers are functional for basic ElDoc/Donchian, EMA/SMA, and TSMOM strategies, but the package is still being actively developed.
> 
> 🐛 You WILL encounter bugs, strange behaviors, and missing features.
> 
> 💣 Use at your own risk - don't trust results without verification!

## 🎯 Current Capabilities

### Strategies
- ✅ Basic Donchian Channel Breakout (ElDoc)
- ✅ Experimental ElDoc research surface with pyramiding diagnostics
- ✅ Time-Series Momentum (TSMOM)
- ✅ EMA/SMA crossover wrappers
- ✅ Long/Short entry and exit signals

### Position Sizing Functions
- ✅ Fixed Contracts Quantity
- ✅ Percentage of Equity (Simple)
- ✅ Donchian-based Risk Position Sizing
- ✅ ATR-risk Position Sizing
- ✅ DI Futures Specialized Position Sizing

### Asset Classes
- ✅ Stocks (Generic)
- ✅ Futures (Generic)
- ✅ DI Futures (Brazilian Interest Rate Futures)
- ✅ Crypto (Generic)

## 📦 Installation

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install from GitHub
devtools::install_github("hugorteixeira/backtestforge")
```

## 🚀 Quick Start

Here's how to run a basic ElDoc channel backtest:

```r
library(backtestforge)

# Simple backtest with 40/40 ElDoc channels
results <- bt_eldoc(
  ticker = "AAPL",           # Symbol or data object
  up = 40,                   # Upper channel period
  down = 40,                 # Lower channel period
  ps_value = 2,        # 2% risk per trade
  ps_type = "eldoc",   # Risk to the opposite ElDoc channel
  execution = "breakout",    # Exact touched channel price
  initial_equity = 100000,   # Starting capital
  fee_value = 4,
  fee_type = "order",
  slip_value = 3,
  slip_type = "bps",
  start_date = "2020-01-01", # Backtest start
  end_date = "2023-01-01"    # Backtest end
)

# Access results
returns <- results$rets     # Portfolio returns
stats <- results$stats      # Trade statistics
trades <- results$trades    # Transaction details
```

In `results$trades`, `fees` is commission only, `slippage` is the estimated
slippage cost, and `total_cost` is the amount subtracted from equity.
Console reports include a `Costs & Slippage Summary` section before the returns
summary, with cost values and cost impact percentages.

Parameter search for agents:

```r
search <- bt_search_native(
  ticker = "AAPL",
  space = list(type = "donchian", up = c(20, 40, 80), down = c(10, 20, 40)),
  budget = 6,
  objective = "total_return"
)
```

## 🔧 Key Functions

### `bt_eldoc()` - Basic ElDoc Backtesting Function

The default ElDoc wrapper is intentionally conservative: Donchian entries/exits,
explicit execution timing, and normal position sizing. It does not expose
pyramiding in the public baseline call.

```r
bt_eldoc(
  ticker,              # Symbol or data object
  up = 40,             # Upper channel period
  down = 40,           # Lower channel period
  ps_value = NULL, # Position-sizing value; NULL reads ticker metadata
  initial_equity = 100000, # Starting capital
  ps_type = NULL,# "eldoc", "atr", "notional", or "contract"
  execution = "breakout", # "breakout", "same_close", "next_open",
                          # "next_close", or "next_avg"
  atr_n = 20,          # ATR lookback for ATR sizing
  atr_mult = 2,        # ATR risk multiple when ps_type = "atr"
  fee = "normal",      # Fee handling
  fee_value = NULL,    # Account-currency commission; NULL reads metadata
  fee_type = NULL,     # "contract" or "order"; NULL reads metadata
  slip_value = NULL, # Optional slippage override; NULL reads metadata
  slip_type = NULL,# "bps", "ticks", "points", or "cash"; NULL reads metadata
  start_date = "1900-01-01",
  end_date = Sys.Date(),
  long = TRUE,         # Enable long trades
  short = TRUE,        # Enable short trades
  plot = FALSE         # Plot results
)
```

When `ps_value`/`ps_type`, `fee_value`/`fee_type`, or
`slip_value`/`slip_type` are `NULL`, the wrapper reads the ticker
metadata. If the required metadata is missing, the backtest errors and asks for
the missing argument. `fee = "nofee"` disables fee and slippage requirements.

`fee_value` is money. With `fee_type = "contract"`, `fee_value = 4` charges 4
per contract/share traded. With `fee_type = "order"`, the same 4 is charged once
for the whole executed order. Slippage can be overridden with the same native
units used by metadata: basis points, ticks, price points, or cash per contract.

ElDoc execution modes:

- `breakout`: fill at the exact touched channel price.
- `exact_price`: alias for `breakout`.
- `same_close`: fill at the signal candle close.
- `next_open`: fill at the next candle open.
- `next_close`: fill at the next candle close.
- `next_avg`: fill at the next candle OHLC average.

ElDoc position sizing modes:

- `eldoc`: risk `ps_value` percent of equity to the opposite channel.
- `atr`: risk `ps_value` percent of equity to `ATR * atr_mult`.
- `notional`: allocate `ps_value` percent of equity to notional exposure.
- `contract`: trade `ps_value` contracts/shares per entry or add.

Experimental trade diagnostics are still computed and stored, but they are not
printed by default by `bt_eldoc()` or `bt_stats()`. Request a hidden block
explicitly when needed:

```r
bt_stats(results, blocks = "excursion_thresholds")
```

### `bt_eldoc_exp()` - Experimental ElDoc Surface

Use `bt_eldoc_exp()` for the research-heavy ElDoc variant. This is where
pyramiding and detailed exploratory diagnostics live.

```r
exp <- bt_eldoc_exp(
  ticker = "AAPL",
  up = 40,
  down = 40,
  ps_value = 2,
  ps_type = "eldoc",
  pyramid = TRUE,
  pyramid_start = 5,
  pyramid_step = 2,
  pyramid_sizing = "entry_qty",
  pyramid_qty_pct = 0.5,
  max_units = 4
)
```

Trade diagnostics report MFE/MAE in percent, ATR, and R units. The `ATR
Statistics` block summarizes the entry ATR scale, initial stop distance in ATR
units, and ATR/initial-stop value per contract or unit. The `Trade Excursions`
block summarizes the distribution; the `Excursion Thresholds` table focuses on
large continuation levels (`5, 10, 15, 20, 25, 30, 35, 40` ATR), with hit counts,
hit rates, median/mean final R, and median/mean post-threshold R diagnostics.
These thresholds are descriptive diagnostics, not automatic parameter
recommendations.

When pyramiding is active, the `Pyramiding` block compares entry and add size,
stop distance at add, and entry/add costs per contract and percent of notional.

### `bt_tsmom()` - Time-Series Momentum

`bt_tsmom()` implements a compact time-series momentum rule: trailing return
over `lookback` bars above `threshold` goes long, below `-threshold` goes short.
The default uses notional sizing and `next_open` execution.

```r
mom <- bt_tsmom(
  ticker = "AAPL",
  lookback = 252,
  threshold = 0,
  ps_value = 100,
  ps_type = "notional",
  execution = "next_open"
)
```

`lookback` is in bars, not calendar days. On intraday data, convert the intended
horizon to the data frequency before comparing results.

### Native spec API

```r
strategy <- bt_strategy_spec("donchian", up = 40, down = 20)
risk <- bt_risk_spec(mode = "risk", ps_type = "eldoc", initial_equity = 100000, risk_pct = 2, reinvest = TRUE)
execution <- bt_execution_spec(
  execution = "breakout",
  fee_value = 4,
  fee_type = "contract",
  slip_value = 7,
  slip_type = "bps"
)

result <- bt_run_native(
  ticker = "AAPL",
  strategy = strategy,
  risk = risk,
  execution = execution,
  start_date = "2020-01-01"
)
```

Risk sizing reinvests by default: new entries use current account equity, while
an open position keeps its original quantity until the next order. Use
`reinvest = FALSE` to size every entry from the initial capital instead.

`normalize_risk` normalizes the returned `rets` stream to a target annualized
volatility for system comparison, measured on daily compounded returns. For
intraday data, bars are compounded by calendar day and annualized with
`sqrt(252)`, so 1H, 4H, and 1D systems use the same risk basis. When it is
supplied, report performance blocks, monthly/quarterly returns, and returns
summaries use the normalized return stream. Execution facts remain raw: trades,
orders, contracts, fees, slippage, and cost impact continue to describe the
actual simulated orders. Full results therefore carry both views:
`stats`/`performance_stats` match `rets`, while `raw_stats` and `raw_rets` keep
the unnormalised execution path.

For futures, attach instrument metadata to the `xts` object when the data source
does not already provide it:

```r
attr(wdo, "fut_multiplier") <- 10
attr(wdo, "fut_tick_size") <- 0.5
attr(wdo, "fee_value") <- 1
attr(wdo, "fee_type") <- "contract"
attr(wdo, "slip_value") <- 1
attr(wdo, "slip_type") <- "ticks"
attr(wdo, "ps_value") <- 2
attr(wdo, "ps_type") <- "eldoc"
```

When data comes from `finharvest` aggregate futures, the native engine reads the
same direct attrs emitted on the `xts` object:

```r
attr(ccm, "ticksize") <- 0.01
attr(ccm, "tickvalue") <- 4.5
attr(ccm, "fee_value") <- 2.5
attr(ccm, "fee_type") <- "contract"
attr(ccm, "slip_value") <- 7
attr(ccm, "slip_type") <- "bps"
attr(ccm, "ps_value") <- 2
attr(ccm, "ps_type") <- "eldoc"
```

If `fut_multiplier`/`multiplier` is absent but `tickvalue` and `ticksize` are
present, the native engine derives the multiplier as `tickvalue / ticksize`.
`slip_value` is interpreted as basis points by default. For non-DI
instruments the cost is `price * slip_value / 10000 * multiplier`; for DI
contracts the basis points apply to the quoted rate and are converted with the
row `TickValue`. To provide slippage as ticks or price points, set `slip_type` to
`"ticks"`/`"price_points"` or use explicit `slippage_ticks`/`slippage_points`.

`ticksize` and `multiplier` are separate contract fields: `ticksize` is the
minimum price increment, while `multiplier` is the cash value of one full price
point.

## 📊 Position Sizing Methods

1. **ElDoc risk** - Risk a percent of equity to the opposite channel.
2. **ATR risk** - Risk a percent of equity to `ATR * atr_mult`.
3. **Notional allocation** - Allocate a percent of equity to exposure.
4. **Contract count** - Trade a fixed number of contracts/shares.
5. **DI futures handling** - Use rate OHLC for signals and PU for execution
   when PU data is available.

## 📈 Example Output

The library provides comprehensive performance metrics:

```
Results for AAPL_eD_nodets - elDoc 40/40

| Trades | Net Profit | Avg Trade | Profit Factor | Max Drawdown |
|-------:|-----------:|----------:|--------------:|-------------:|
|     42 |  $1,245.32 |   $29.65  |          1.78 |      -8.42%  |

Annualized Returns: 12.78%
Cumulative Returns: 156.34%
```

## 🛠️ Dependencies

This library uses:

- `TTR` - Technical analysis functions
- `xts`/`zoo` - Time series handling
- `finharvest` - Data fetching
- `brfutures` - Optional Brazilian futures and DI helpers
- `tradeplotr` - Visualization

## 🤝 Contributing

Contributions are welcome! Since this is a work in progress:

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Open a pull request

## 📝 Current Roadmap

- [x] Native-only engine without `quantstrat`, `blotter`, or `FinancialInstrument`
- [x] Explicit fees/slippage with separate trade-ledger columns
- [x] Basic Donchian/ElDoc execution modes, ATR sizing, and contract sizing
- [x] Experimental ElDoc pyramiding and trade-excursion diagnostics isolated in `bt_eldoc_exp()`
- [x] Time-series momentum wrapper with native `tsmom` strategy support
- [x] Unit tests for native sizing, DI PU handling, costs, and batch failure strictness
- [ ] Split the native simulator internals into smaller state-transition helpers
- [ ] Add more real-data regression fixtures for DI and stitched futures series
- [ ] Improve documentation around metadata attrs expected from `finharvest`
- [x] Parameter search with `bt_search_native()`

## 📄 License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## 👨‍💻 About the Author

Hi, I’m Hugo. I build tools around trading and backtesting in R to streamline workflow and help iterate on strategies faster. If you find backtestforge useful (or frustrating!), feedback is welcome.


---

Project Link: [https://github.com/hugorteixeira/backtestforge](https://github.com/hugorteixeira/backtestforge)

<p align="center">Made with ❤️ and ☕ in R</p>
