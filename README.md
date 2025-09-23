# 📈 rBacktestTools

> **⚠️ WORK IN PROGRESS - EXPECT BUGS!** ⚠️

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=flat&logo=r&logoColor=white)
![Status: Beta](https://img.shields.io/badge/status-beta-orange)

A powerful R library for simplifying backtesting with **blotter/quantstrat** using trend-following strategies on stocks, futures, and crypto. Currently focused on Donchian Channel strategies with position sizing functions.

## 🚀 Features

- **Donchian Channel Strategies** - Built-in support for classic breakout strategies
- **Smart Position Sizing** - Risk-based position sizing with multiple methods
- **Multi-Asset Support** - Works with stocks, futures, and crypto
- **Futures Specialization** - Custom logic for DI futures and other instruments
- **Visual Backtesting** - Integrated plotting with `rTradingPlots`
- **Performance Analytics** - Comprehensive performance reporting

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
devtools::install_github("hugorteixeira/rBacktestTools")
```

## 🚀 Quick Start

Here's how to run a basic Donchian Channel backtest:

```r
library(rBacktestTools)

# Simple stock backtest with 40/40 Donchian channels
results <- bt_eldoc(
  ticker = "AAPL",           # Symbol or data object
  up = 40,                   # Upper channel period
  down = 40,                 # Lower channel period
  ps_risk_value = 2,         # 2% risk per trade
  start_date = "2020-01-01", # Backtest start
  end_date = "2023-01-01",   # Backtest end
  plot = TRUE                # Show performance chart
)

# Access results
returns <- results$rets     # Portfolio returns
stats <- results$stats      # Trade statistics
trades <- results$trades    # Transaction details
```

## 🔧 Key Functions

### `bt_eldoc()` - Main Backtesting Function

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
  plot = FALSE         # Plot results
)
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

This library builds on top of these excellent R packages:

- `quantstrat` - Event-based backtesting framework
- `blotter` - Portfolio and account management
- `TTR` - Technical analysis functions
- `xts`/`zoo` - Time series handling
- `FinancialInstrument` - Instrument modeling
- `PerformanceAnalytics` - Performance metrics
- `rSenhorMercadoAPI` - Data fetching (custom)
- `rTradingPlots` - Visualization (custom)

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
- [ ] Unit tests (none currently!)
- [ ] Parameter optimization functions
- [ ] Additional asset class specializations

## 📄 License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## 👨‍💻 About the Author

Hi, I’m Hugo. I build tools around trading and backtesting in R to streamline workflow and help iterate on strategies faster. If you find rBacktestTools useful (or frustrating!), feedback is welcome.


---

Project Link: [https://github.com/hugorteixeira/rBacktestTools](https://github.com/hugorteixeira/rBacktestTools)

<p align="center">Made with ❤️ and ☕ in R</p>
