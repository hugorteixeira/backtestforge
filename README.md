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
- **Smart Position Sizing** - Native specs delegate quantity math to `positionsizer`
- **Shared-Capital Portfolio Backtests** - Runs multiple instruments against one growing account equity
- **Multi-Asset Support** - Works with stocks, futures, and crypto
- **Brazilian Futures Support** - DI PU conversion and position sizing use `positionsizer`; contract metadata is read from `xts` attrs
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

### Position Sizing Modes
- ✅ Fixed Contracts Quantity
- ✅ Percentage of Equity (Simple)
- ✅ Donchian-based Risk Position Sizing
- ✅ ATR-risk Position Sizing
- ✅ DI Futures Specialized Position Sizing

These are `backtestforge` strategy/risk modes, not standalone sizing engines.
The actual quantity math is delegated to `positionsizer`, including DI rate/PU
conversion and canonical futures sizing. Legacy package-local DI sizing helpers
and their generated `man/dot-calculate_futures_di_*` pages were retired for that
reason.

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
audit <- results$trade_audit # Completed-trade audit ledger
```

In `results$trades`, `fees` is commission only, `slippage` is the estimated
slippage cost, and `total_cost` is the execution-cost amount subtracted from
equity. Perpetual-futures funding cashflows are kept separately in
`results$funding_events` and summarized as `stats$funding`. Console reports
include a `Costs & Slippage Summary` section before the returns summary, with
fees, slippage, funding, and impact percentages.

`results$trade_audit` expands completed trades into order-level rows. Entry,
pyramid, and exit rows show `signal_price`, slippage-adjusted `fill_price`,
fees, slippage, total cost, and bars held. Perpetual runs with funding add a
separate `event_type = "funding"` row per completed trade with funding paid or
received.

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
  fee_value = NULL,    # Commission value; NULL reads metadata
  fee_type = NULL,     # "contract", "order", "percent", or "bps"
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

`fee_type = "contract"` treats `fee_value` as money per contract/share traded.
`fee_type = "order"` charges it once for the whole executed order.
`fee_type = "percent"` and `"bps"` charge against traded notional
(`abs(qty_delta) * price * multiplier`), which is the intended path for Binance
USDT-M futures fees. When `finharvest` codigos attrs are present,
`costs$style_fee = "percent"` is accepted as the fee type and `broker_fee` or
`taker_fee` can provide the fee value. Slippage can be overridden with the same
native units used by metadata: basis points, ticks, price points, or cash per
contract.

For Binance-style USDT-M crypto futures, wrapper-created risk specs default to
decimal quantities and forward exchange filters such as `quantity_step`,
`min_qty`, `max_qty`, and `min_notional` when they are present in metadata. Pass
`max_leverage = 50` to cap notional at 50x equity when you want the sizing layer
to model the exchange leverage setting as a notional limit/margin estimate;
leverage does not multiply returns. Pass `integer_qty = TRUE` only when an
instrument should trade whole units. This same linear USDT-M path applies to
Binance perpetual, `QTR`/`NQTR`, and dated delivery futures such as
`BTCUSDT_260627`; the quarterly/delivery variants do not get funding unless
explicit funding events are supplied.

Perpetual funding is a cashflow, not slippage and not a price adjustment. Pass
`funding = TRUE` to read funding columns from the supplied `xts` or fetch
`finharvest::finget_binance_fut_funding()` only for explicit `_PERPETUAL`
tickers. `QTR`/`NQTR`/dated Binance delivery futures keep zero funding under
`funding = TRUE`; use explicit funding data only for a deliberate scenario test.
You can pass an `xts`/`data.frame` with `date`/`timestamp`,
`FundingRate`/`funding_rate`, and optional `FundingMarkPrice`/`mark_price`.
Positive funding rates debit longs and credit shorts while the position is open.

`bt_hold()` opens on the first available close and exits on the last close. It
defaults to 100% notional sizing with fractional quantity, so it is useful for
sanity-checking spot, perpetual, and quarterly futures returns against simple
long or short hold behavior.

ElDoc execution modes:

- `breakout`: fill at the exact touched channel price.
- `exact_price`: alias for `breakout`.
- `same_close`: fill at the signal candle close.
- `next_open`: fill at the next candle open.
- `next_close`: fill at the next candle close.
- `next_avg`: fill at the next candle OHLC average.

`execution_timeframe` is an advanced single-instrument option for Donchian
breakout intrabar execution. The default `"same"` uses the strategy bars exactly
as before. `"5m"`, `"1h"`, and `"4h"` derive and fetch a matching execution
ticker such as `BTCUSDT_PERPETUAL_5m`, require the execution timeframe to be
lower/equal than the strategy timeframe, and fail if the lower series is missing
or does not cover the signal window. The engine iterates every execution candle
inside each signal candle, so repeated upper/lower touches can trigger multiple
reversals within the same strategy bar. Indicators, Donchian levels, sizing
stops, funding lookup, and the returned equity frequency remain on the strategy
timeframe. Audit rows expose both `event_time` and `signal_time`.

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
For all native runners, `backtestforge` chooses the strategy stop/source price
and builds the instrument spec, then calls `positionsizer` for the canonical
quantity calculation.

### Shared-capital portfolio backtests

Use `bt_run_portfolio()` when several instruments must share the same account
equity. This is different from `bt_batch(gen_portfolio = ...)`: batch-generated
portfolios aggregate finished return streams after the individual backtests have
already been run; `bt_run_portfolio()` simulates all instruments on one timeline
and sizes new positions from the same equity curve.

```r
portfolio <- bt_run_portfolio(
  tickers = list(
    WDO = wdo_xts,
    WIN = win_xts,
    DI1 = di1_xts,
    BGI = bgi_xts
  ),
  strategy = list(
    WDO = bt_strategy_spec("donchian", up = 40, down = 20),
    WIN = bt_strategy_spec("donchian", up = 20, down = 10),
    DI1 = bt_strategy_spec("tsmom", lookback = 120),
    BGI = bt_strategy_spec("donchian", up = 55, down = 25)
  ),
  risk = list(
    WDO = bt_risk_spec(mode = "risk", risk_pct = 1.0, ps_type = "eldoc"),
    WIN = bt_risk_spec(mode = "risk", risk_pct = 0.7, ps_type = "eldoc"),
    DI1 = bt_risk_spec(mode = "risk", risk_pct = 1.5, ps_type = "atr", atr_mult = 2),
    BGI = bt_risk_spec(mode = "fixed", fixed_qty = 1)
  ),
  execution = list(
    WDO = bt_execution_spec(
      execution = "breakout",
      fee_value = 1,
      fee_type = "contract",
      slip_value = 1,
      slip_type = "ticks"
    ),
    WIN = bt_execution_spec(
      execution = "breakout",
      fee_value = 1,
      fee_type = "contract",
      slip_value = 1,
      slip_type = "ticks"
    ),
    DI1 = bt_execution_spec(
      execution = "same_close",
      fee_value = 2,
      fee_type = "contract",
      slip_value = 0,
      slip_type = "cash"
    ),
    BGI = bt_execution_spec(
      execution = "breakout",
      fee_value = 2.5,
      fee_type = "contract",
      slip_value = 7,
      slip_type = "bps"
    )
  ),
  initial_equity = 100000,
  max_positions = Inf
)
```

All of `strategy`, `risk`, `execution`, `ps_value`, `ps_type`, and `reinvest`
can be scalar values applied to every instrument or named per-instrument
overrides. Names are resolved against the list label, the source symbol, and
metadata such as `attr(x, "information")$ticker`.

If you only need different fixed contract sizes while sharing one setup:

```r
portfolio <- bt_run_portfolio(
  tickers = list(WDO = wdo_xts, WIN = win_xts),
  strategy = bt_strategy_spec("donchian", up = 40, down = 20),
  ps_value = c(WDO = 1, WIN = 2),
  ps_type = c(WDO = "contract", WIN = "contract"),
  execution = bt_execution_spec(
    execution = "breakout",
    fee_value = 0,
    fee_type = "contract",
    slip_value = 0,
    slip_type = "cash"
  ),
  initial_equity = 100000
)
```

The result includes the portfolio-level equity curve and return stream plus
per-instrument specs and trade summaries:

```r
portfolio$equity
portfolio$rets
portfolio$trades
portfolio$positions
portfolio$instrument_stats
portfolio$spec$strategies
portfolio$spec$risks
portfolio$spec$executions
```

Current portfolio limitations: only `breakout` and `same_close` execution are
implemented; `next_open`, `next_close`, `next_avg`, pyramiding, margin, and
advanced signal-priority rules are not implemented yet.

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

For futures, the current `finharvest` contract attaches instrument metadata in
nested `xts` plugin attrs. `backtestforge` reads `contract` for contract specs
and `costs` for sizing, fees, and slippage:

```r
attr(wdo, "contract") <- list(
  multiplier = 10,
  ticksize = 0.5,
  tickvalue = 5,
  root = "WDO",
  timeframe = "1H"
)
attr(wdo, "costs") <- list(
  fee_value = 1,
  fee_type = "contract",
  slip_value = 1,
  slip_type = "ticks",
  ps_value = 2,
  ps_type = "eldoc"
)
```

When data is fetched internally, `backtestforge` calls `finharvest::finget()`
with `attrs_source = "codigos"` so the runtime codigos attrs are used. The
curated `attrs_source = "fintickers"` registry remains an opt-in path for
callers that fetch and pass an `xts` object directly. Direct legacy attrs are
still accepted for older objects, but new objects should prefer the plugin
layout:

```r
attr(ccm, "information")$ticker
#> "CCMFUT_1H_AGG"
attr(ccm, "contract")
#> list(multiplier = 450, root = "CCM", ticksize = 0.01,
#>      tickvalue = 4.5, timeframe = "1H")
attr(ccm, "costs")
#> list(fee_value = 2.5, fee_type = "contract",
#>      slip_value = 7, slip_type = "bps",
#>      ps_value = 2, ps_type = "eldoc")
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

When an `xts` object is passed directly, symbols are resolved in this order:
`attr(x, "symbol")`, `attr(x, "ticker")`, `attr(x, "information")$ticker`, the
name used in the `tickers` list, then a generated `AssetN` fallback.

## 📊 Position Sizing Methods

1. **ElDoc risk** - Risk a percent of equity to the opposite channel.
2. **ATR risk** - Risk a percent of equity to `ATR * atr_mult`.
3. **Notional allocation** - Allocate a percent of equity to exposure.
4. **Contract count** - Trade a fixed number of contracts/shares.
5. **DI futures handling** - Use rate OHLC for signals and PU for execution
   when PU data is available.

The actual quantity calculation is delegated to `positionsizer`. `backtestforge`
still owns strategy signals, stop selection, execution timing, costs, shared
equity simulation, and result reporting, but the canonical sizing math lives in
that package. This is intentional: `backtestforge` should not grow parallel
helpers for DI notional/rate sizing or futures quantity formulas.

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
- `positionsizer` - Canonical position sizing and DI rate/PU calculations
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
- [x] Shared-capital portfolio runner with per-instrument strategy, risk, and execution specs
- [x] Unit tests for native sizing, DI PU handling, costs, and batch failure strictness
- [ ] Split the native simulator internals into smaller state-transition helpers
- [ ] Add more real-data regression fixtures for DI and stitched futures series
- [x] Improve documentation around metadata attrs expected from `finharvest`
- [x] Parameter search with `bt_search_native()`

## 📄 License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## 👨‍💻 About the Author

Hi, I’m Hugo. I build tools around trading and backtesting in R to streamline workflow and help iterate on strategies faster. If you find backtestforge useful (or frustrating!), feedback is welcome.


---

Project Link: [https://github.com/hugorteixeira/backtestforge](https://github.com/hugorteixeira/backtestforge)

<p align="center">Made with ❤️ and ☕ in R</p>
