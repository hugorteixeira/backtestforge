# 📈 backtestforge

> **⚠️ WORK IN PROGRESS - EXPECT BUGS!** ⚠️

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=flat&logo=r&logoColor=white)
![Status: Beta](https://img.shields.io/badge/status-beta-orange)

A fast R library for trend-following backtests on stocks, futures, and crypto.
The default path is now a native in-memory engine that avoids `.GlobalEnv`,
`.blotter`, and `.strategy` mutation. The older **blotter/quantstrat** path is
still available with `engine = "quantstrat"` for parity checks and migration.

## 🚀 Features

- **Native Stateless Engine** - Runs without quantstrat/blotter global state
- **Donchian Channel Strategies** - Built-in support for classic breakout strategies
- **EMA/SMA Crossover Strategies** - Lightweight moving-average strategies
- **Smart Position Sizing** - Risk-based position sizing with multiple methods
- **Multi-Asset Support** - Works with stocks, futures, and crypto
- **Brazilian Futures Support** - Optional `brfutures` integration for DI PU conversion; contract metadata is read from `xts` attrs
- **Agent-Friendly Search** - `bt_search_native()` evaluates serializable parameter grids
- **Visual Backtesting** - Optional plotting with `tradeplotr`
- **Performance Analytics** - Comprehensive performance reporting

## Current Native Engine Notes

The current implementation plan, benchmark snapshot, parity findings, and next
session handoff are documented in
[`docs/native-engine-status-and-handoff.md`](docs/native-engine-status-and-handoff.md).
Read that file before continuing engine, DI futures, or server-job work.

## ⚠️ Important Disclaimer

> This library is **HEAVILY WORK IN PROGRESS** and contains **LOTS OF BUGS**! 
> 
> 🔧 It's currently functional for Donchian Channel strategies but is being actively developed.
> 
> 🐛 You WILL encounter bugs, strange behaviors, and missing features.
> 
> 💣 Use at your own risk - don't trust results without verification!

## 🎯 Current Capabilities

### Strategies
- ✅ Donchian Channel Breakout (Eldoc)
- ✅ Long/Short entry and exit signals
- ✅ Custom indicator support

### Position Sizing Functions
- ✅ Fixed Contracts Quantity
- ✅ Percentage of Equity (Simple)
- ✅ Donchian-based Risk Position Sizing
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

Here's how to run a basic Donchian Channel backtest:

```r
library(backtestforge)

# Simple backtest with 40/40 Donchian channels using the native engine
results <- bt_eldoc(
  ticker = "AAPL",           # Symbol or data object
  up = 40,                   # Upper channel period
  down = 40,                 # Lower channel period
  ps_risk_value = 2,         # 2% risk per trade
  start_date = "2020-01-01", # Backtest start
  end_date = "2023-01-01",   # Backtest end
  engine = "native"
)

# Access results
returns <- results$rets     # Portfolio returns
stats <- results$stats      # Trade statistics
trades <- results$trades    # Transaction details
```

For the legacy path:

```r
legacy <- bt_eldoc("AAPL", up = 40, down = 40, engine = "quantstrat")
```

Native parameter search for agents:

```r
search <- bt_search_native(
  ticker = "AAPL",
  space = list(type = "donchian", up = c(20, 40, 80), down = c(10, 20, 40)),
  budget = 6,
  objective = "total_return"
)
```

## 🔧 Key Functions

### `bt_eldoc()` - Donchian Backtesting Function

The workhorse of the library - runs a complete Donchian Channel backtest:

```r
bt_eldoc(
  ticker,              # Symbol or data object
  up = 40,             # Upper channel period
  down = 40,           # Lower channel period
  ps_risk_value = 2,   # Risk percentage
  ps = "pct",          # Position sizing method
  fee = "normal",      # Fee handling
  start_date = "1900-01-01",
  end_date = Sys.Date(),
  long = TRUE,         # Enable long trades
  short = TRUE,        # Enable short trades
  engine = "native",   # Or "quantstrat"
  plot = FALSE         # Plot results
)
```

### Native spec API

```r
strategy <- bt_strategy_spec("donchian", up = 40, down = 20)
risk <- bt_risk_spec(mode = "risk", risk_pct = 2)
execution <- bt_execution_spec(timing = "next_open", fee = "nofee")

result <- bt_run_native(
  ticker = "AAPL",
  strategy = strategy,
  risk = risk,
  execution = execution,
  start_date = "2020-01-01"
)
```

For futures, attach instrument metadata to the `xts` object when the data source
does not already provide it:

```r
attr(wdo, "fut_multiplier") <- 10
attr(wdo, "fut_tick_size") <- 0.5
attr(wdo, "identifiers") <- list(fees = 1, slippage = 1)
```

## 📊 Position Sizing Methods

1. **Fixed Contracts** - Trade fixed number of contracts
2. **Percentage Equity** - Simple percentage of equity allocation
3. **Donchian Risk-Based** - Risk-based sizing using Donchian stops
4. **DI Futures Specialized** - Custom logic for Brazilian DI futures

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

- Native engine dependencies:
  - `TTR` - Technical analysis functions
  - `xts`/`zoo` - Time series handling
  - `PerformanceAnalytics` - Performance metrics
  - `finharvest` - Data fetching
  - `brfutures` - Optional Brazilian futures and DI helpers
- Legacy engine dependencies:
  - `quantstrat` - Event-based backtesting framework
  - `blotter` - Portfolio and account management
  - `FinancialInstrument` - Legacy instrument modeling
- `tradeplotr` - Visualization (custom)

## 🤝 Contributing

Contributions are welcome! Since this is a work in progress:

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Open a pull request

## 📝 TODO (Lots of Work Needed!)

- [ ] Better error handling and validation
- [ ] More strategy types beyond Donchian
- [ ] Additional position sizing methods
- [ ] Improved documentation and examples
- [x] Unit tests for the native engine
- [x] Parameter search with `bt_search_native()`
- [ ] Quantstrat parity snapshots for selected strategies
- [ ] Server job/result persistence
- [ ] Additional asset class specializations

## 📄 License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## 👨‍💻 About the Author

Hi, I’m Hugo. I build tools around trading and backtesting in R to streamline workflow and help iterate on strategies faster. If you find backtestforge useful (or frustrating!), feedback is welcome.


---

Project Link: [https://github.com/hugorteixeira/backtestforge](https://github.com/hugorteixeira/backtestforge)

<p align="center">Made with ❤️ and ☕ in R</p>
