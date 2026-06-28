# Backtestforge Native Engine Status And Handoff

Date: 2026-06-01

`backtestforge` is now native-only. The public wrappers call the in-memory
simulator directly, and the package no longer imports or ships the old external
portfolio/instrument stack.

## Public Surface

- `bt_eldoc()`
- `bt_eldoc_exp()`
- `bt_ema()`
- `bt_sma()`
- `bt_tsmom()`
- `bt_batch()`
- `bt_strategy_spec()`
- `bt_risk_spec()`
- `bt_execution_spec()`
- `bt_run_native()`
- `bt_run_portfolio()`
- `bt_search_native()`

There is no `engine` argument. Passing `engine` through `bt_batch()` specs or
`...` fails explicitly so stale callers are easy to find.

## Simulation Contract

- Input data is an `xts` object or a symbol fetched with `finharvest::finget()`.
- The simulator keeps the input data frequency.
- Strategy, risk, and execution settings are plain list specs.
- ElDoc execution defaults to `breakout`, using the exact touched channel price.
  `exact_price` is an alias. Other supported modes are `same_close`,
  `next_open`, `next_close`, and `next_avg`.
- Risk sizing uses `reinvest = TRUE` by default: new entries use current equity,
  while open positions keep their original quantity until another order.
- `bt_run_portfolio()` is the shared-capital runner. It simulates multiple
  instruments on one timeline and sizes later entries from the same evolving
  account equity. This is different from `bt_batch(gen_portfolio = ...)`, which
  combines already-finished return streams after individual backtests.
- `normalize_risk` is a comparison layer on returns, not a rewrite of the
  executed order path. The target is annualized volatility of daily compounded
  returns: intraday bars are compounded by calendar day and annualized with
  `sqrt(252)` so 1H, 4H, and 1D systems share the same risk basis. When set,
  `rets`, `stats`, monthly/quarterly returns, returns summaries, and performance
  blocks use the risk-normalized stream; `raw_rets`, `raw_stats`, trades,
  positions, and cost/activity blocks preserve the unnormalised simulated
  execution.
- `bt_eldoc()` is the baseline Donchian wrapper: no pyramiding arguments and no
  research-heavy diagnostics in the default console/stat printout.
- `bt_eldoc_exp()` is the experimental Donchian surface. It keeps pyramiding,
  ATR statistics, trade excursions, excursion-threshold tables, and pyramiding
  diagnostics visible by default.
- ElDoc position sizing supports `ps_type = "eldoc"` for channel-risk sizing,
  `"atr"` for ATR-risk sizing, `"notional"` for percent-of-equity notional
  allocation, and `"contract"` for fixed contracts/shares.
- Pyramiding is part of the experimental ElDoc path, not a separate Turtle
  engine. Turtle-like behavior should be represented as a preset around
  `bt_eldoc_exp()`.
- With `pyramid = TRUE`, the first add triggers after `pyramid_start` ATR from
  the entry price; `NULL` uses `pyramid_step` to keep the old behavior. Later
  adds trigger every `pyramid_step` ATR from the previous add until `max_units`.
- Pyramiding defaults to `pyramid_sizing = "risk"`, which sizes each add from
  the active stop/risk distance. `pyramid_sizing = "entry_qty"` uses
  `pyramid_qty_pct` times the initial entry quantity for each add.
- `bt_tsmom()` implements a simple native time-series momentum strategy:
  trailing return over `lookback` bars above `threshold` is long, below
  `-threshold` is short. Defaults are `ps_type = "notional"`, `ps_value = 100`,
  and `execution = "next_open"`.
- Trade excursion diagnostics are descriptive. In `bt_eldoc()` they are stored
  but hidden from default `bt_stats()`/report output; request a block explicitly
  with `bt_stats(x, blocks = "excursion_thresholds")`. In `bt_eldoc_exp()`, they
  print by default. `ATR Statistics` summarizes the
  entry ATR scale, initial stop distance in ATR units, and ATR/initial-stop value
  per contract or unit. `Trade Excursions` summarizes MFE/MAE and MFE ATR
  percentiles; `Excursion Thresholds` focuses on large continuation levels
  (`5, 10, 15, 20, 25, 30, 35, 40` ATR) and carries hit rates with counts plus
  median/mean final and post-threshold R diagnostics. Do not treat top-tail
  percentiles as automatic pyramiding rules without cross-market validation. The
  console table uses short headers such as `ATRT`, `Med FR`, and `Med PR` to stay
  compact.
- The `Pyramiding` block reports add size versus entry size, stop distance at
  add, and entry/add cost diagnostics per contract and percent of notional.
- Results include returns, stats, trades, positions, equity, annotated market
  data, and the spec used to run the backtest.

## Shared-Capital Portfolio Contract

`bt_run_portfolio()` is the native multi-instrument runner. It is intended for
portfolio-style futures tests where all instruments share one account equity and
new entries size from that shared equity. Single-instrument wrappers remain the
normal entry point for one-symbol backtests.

Current behavior:

- Input is a character vector, an `xts`, or a list of character/`xts`
  instruments.
- The engine builds one unified timeline across all instruments.
- On each timestamp, open positions are marked to market, exits are processed,
  shared equity is updated, entries are collected, and new positions are sized
  from the current shared equity.
- Exits are processed before entries on the same timestamp.
- Each instrument keeps its own strategy, risk, execution, metadata, and source
  symbol.
- `strategy`, `risk`, `execution`, `ps_value`, `ps_type`, and `reinvest` can be
  scalar values for all instruments or named per-instrument overrides.
- Per-instrument names are matched against the list label, source symbol, ticker
  argument, and metadata such as `attr(x, "information")$ticker`.
- Results include `equity`, `rets`, `trades`, `positions`,
  `instrument_stats`, and per-instrument specs in `spec$strategies`,
  `spec$risks`, and `spec$executions`.

Example:

```r
res <- bt_run_portfolio(
  tickers = list(WDO = wdo_xts, WIN = win_xts, DI1 = di1_xts),
  strategy = list(
    WDO = bt_strategy_spec("donchian", up = 40, down = 20),
    WIN = bt_strategy_spec("donchian", up = 20, down = 10),
    DI1 = bt_strategy_spec("tsmom", lookback = 120)
  ),
  risk = list(
    WDO = bt_risk_spec(mode = "risk", risk_pct = 1.0, ps_type = "eldoc"),
    WIN = bt_risk_spec(mode = "risk", risk_pct = 0.7, ps_type = "eldoc"),
    DI1 = bt_risk_spec(mode = "risk", risk_pct = 1.5, ps_type = "atr")
  ),
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

Use `fee = "nofee"` to bypass costs during exploratory tests. With `fee =
"normal"`, pass `fee_value` and `fee_type`, or provide matching cost metadata in
the `xts` attrs. Missing fee/slippage metadata is strict by design.

`bt_batch(gen_portfolio = ...)` is not a shared-capital simulator. It aggregates
finished return streams after each individual backtest has already chosen its
own orders and sizing. Use `bt_run_portfolio()` when P/L from one instrument
must affect later sizing in another instrument.

Current limitations:

- Portfolio execution supports `breakout` and `same_close` only.
- `next_open`, `next_close`, and `next_avg` are not implemented in the
  portfolio runner yet.
- Portfolio pyramiding is not implemented yet.
- No margin/guarantee model is enforced.
- Entry priority is deterministic list order only (`priority = "order"`).
- Futures/DI behavior reuses the native single-instrument helpers, but more
  real-data regression coverage is still needed.

## Position Sizing Ownership

`backtestforge` owns strategy signals, sizing stop selection, execution timing,
ledger costs, shared equity simulation, and reporting. The canonical quantity
math is delegated to `positionsizer` through `positionsizer::ps_instrument_spec()`
and `positionsizer::ps_size_position()`.

DI rate-to-PU conversion also belongs to `positionsizer` via
`positionsizer::ps_di_rate_to_pu()`. The old package-local DI sizing helpers and
their generated `man/dot-calculate_futures_di_*` pages are intentionally removed;
do not reintroduce a second sizing engine in this package.

## Futures Metadata Policy

Contract metadata comes from the `xts` object only. Do not infer multiplier,
tick size, fees, or slippage from ticker roots. The current `finharvest` runtime
shape stores stable attrs as nested plugin lists:
`contract` carries contract specs such as `ticksize`, `tickvalue`, `multiplier`,
`root`, `timeframe`, `quantity_step`, `min_qty`, `max_qty`, and
`min_notional`; `costs` carries `fee_value`, `broker_fee`, `taker_fee`,
`fee_type`, `style_fee`, `slip_value`, `slip_type`, `ps_value`, and `ps_type`.
Direct legacy attrs are still accepted for older objects.

Recognized metadata fields include:

- `fut_multiplier`
- `multiplier`
- `fut_tick_size`
- `tick_size`
- `ticksize`
- `fut_tick_value`
- `tick_value`
- `tickvalue`
- `fee_value`
- `broker_fee`
- `taker_fee`
- `fee_type`
- `style_fee`
- `slip_value`
- `slip_type`
- `slippage_bps`
- `slippage_ticks`
- `slippage_points`
- `quantity_step`
- `min_qty`
- `max_qty`
- `min_notional`
- `ps_value`
- `ps_type`

When an `xts` object is passed directly, the symbol is resolved from
`attr(x, "symbol")`, then `attr(x, "ticker")`, then nested metadata such as
`attr(x, "information")$ticker`. In `bt_run_portfolio()`, the name used in the
`tickers` list is also used as a fallback label/source key.

If `multiplier` is absent but `tickvalue` and `ticksize` are present, the
simulator derives `multiplier = tickvalue / ticksize`. `slip_value` is
treated as basis points by default. For non-DI instruments the cash cost is
`price * slip_value / 10000 * multiplier`; for DI contracts the basis
points apply to the quoted annualized rate and are converted with the row
`TickValue`.
Use `slippage_points` for price-point slippage, `slippage_ticks` for tick
slippage, or set `slip_type` to `"ticks"`, `"price_points"`, or
`"cash"` when overriding the default unit.

Transaction rows keep cost attribution separate: `fees` is commission only,
`slippage` is estimated slippage cost, and `total_cost` is the amount subtracted
from equity.
The console report prints a `Costs & Slippage Summary` block before
`Returns Summary`; it includes cost values and cost impact percentages versus
gross P/L.

When `backtestforge` fetches symbols through `finharvest::finget()`, it requests
`attrs_source = "codigos"` so the simulator receives the runtime codigos attrs.
`attrs_source = "fintickers"` remains an explicit opt-in path for callers that
preload an `xts` object and pass it directly.

Backtest wrappers expose cost overrides directly. `fee_type = "contract"`
charges `fee_value` in account currency per unit traded, `fee_type = "order"`
charges it once for the whole executed order, and `fee_type = "percent"` /
`"bps"` charges against traded notional.
`slip_value` uses `slip_type` (`"bps"`, `"ticks"`, `"points"`, or
`"cash"`) and overrides the instrument slippage metadata for scenario tests.
The public position-sizing arguments are `ps_value` and `ps_type`;
no position-sizing aliases are accepted.

For `ps_value`/`ps_type`, `fee_value`/`fee_type`, and
`slip_value`/`slip_type`, `NULL` means "read from ticker metadata". If the
metadata is missing, the native engine errors instead of guessing. `fee =
"nofee"` disables fee and slippage resolution, but still requires position
sizing from arguments or metadata.

Backtest wrappers also expose `initial_equity`, defaulting to `100000`, and pass
it through to `bt_risk_spec()` for Donchian, EMA, SMA, TSMOM, and `bt_batch()`
runs.
They also forward `max_leverage` and `integer_qty` through `...` when `risk` is
not supplied. Binance-style USDT-M crypto futures default to decimal quantities;
`max_leverage` is a notional cap/margin estimate, not a return multiplier. The
same decimal linear-futures path applies to Binance `_PERPETUAL`, `QTR`/`NQTR`,
and dated delivery tickers such as `BTCUSDT_260627`.

Single-instrument Donchian breakout runs can set `execution_timeframe` in
`...` to `"5m"`, `"1h"`, or `"4h"`; `"same"` is the default. Non-`same` values
derive a matching execution ticker, for example `BTCUSDT_PERPETUAL_1h` ->
`BTCUSDT_PERPETUAL_5m`, fetch that series through the same `finharvest`
contract, require the execution timeframe to be lower/equal than the strategy
timeframe, and error if the series is unavailable or does not cover the signal
window. The lower timeframe is iterated candle by candle inside each signal bar:
upper/lower touches can repeat and can generate multiple reversals before the
strategy bar closes. Indicators, Donchian levels, sizing stops, funding lookup,
and the returned equity frequency stay on the strategy timeframe. Trade ledgers
and `trade_audit` include `signal_time`, `execution_timeframe`, and
`execution_source_ticker` so consumers can distinguish the signal candle from
the resolved execution candle.

Single-instrument native runs can also apply perpetual funding cashflows.
`funding = TRUE` reads funding columns from the input `xts` or fetches
`finharvest::finget_binance_fut_funding()` only for explicit `_PERPETUAL`
symbols. Binance `QTR`/`NQTR`/dated delivery futures are not auto-mapped to the
matching perpetual; they keep zero funding unless explicit funding events are
supplied as an `xts`/`data.frame` with `date`/`timestamp`, `funding_rate`, and
optional `mark_price`. For Binance perpetuals, current `finharvest` fills
missing historical funding marks from Binance mark-price klines before the
events reach the simulator. The engine uses that event mark for
`-quantity * mark_price * multiplier * funding_rate`; the candle-price fallback
exists only for manually supplied incomplete funding data. Funding is kept out
of `trades$total_cost`; it is returned in `funding_events`, summarized in
`stats$funding`, and included in the equity curve while the position is open.

Completed trades also produce `trade_audit`. This audit ledger contains
order-level `entry`, `pyramid`, and `exit` rows with signal price,
slippage-adjusted fill price, fees, slippage, total cost, and bars held.
Perpetual-funding runs add a separate `event_type = "funding"` row per
completed trade with funding paid/received and event count.

`bt_hold()` is a native wrapper for first-close to last-close hold tests. It
uses the same execution, fee, slippage, funding, and audit ledgers as the other
wrappers, and defaults to 100% notional sizing with fractional quantity.

`ticksize` and `multiplier` are intentionally separate: `ticksize` is the minimum
price increment, while `multiplier` is the cash value of one full price point.

If multiplier or tick-size metadata is missing, the simulator still warns and
uses `1` as the fallback. Position sizing, fee, and slippage metadata are strict
and error when required values are missing.

## DI Futures Behavior

For DI-like symbols, the simulator:

- tries to fill missing maturity using `finharvest::finget_maturities()` when
  available;
- uses `positionsizer` to convert annualized-rate OHLC data into PU columns when
  a conversion is needed;
- keeps annualized-rate OHLC columns for indicators and signals;
- prefers PU columns (`PU_o`, `PU_h`, `PU_l`, `PU_c`, or equivalent names) for
  execution and mark-to-market.

Experimental DI pyramiding follows the same split: ATR and favorable-move
triggers are calculated in rate space, then each add is filled and marked in PU
space. Long DI positions gain when PU falls, so the simulator keeps the negative
PU P/L multiplier for positive long quantity.

The preferred data-layer improvement is to have `finharvest` or `brfutures`
return DI PU columns and contract metadata as attrs before the backtest starts;
when the simulator needs an on-the-fly conversion, the calculation belongs to
`positionsizer`.

## Verification

Run from the repository root:

```r
devtools::document()
devtools::test(reporter = "summary")
```

Then run:

```sh
git diff --check
R CMD INSTALL .
```

## Next Work

1. Stabilize futures metadata at the data layer so all B3 futures arrive with
   `multiplier`, `ticksize`, fees, slippage, and DI maturity where applicable.
2. Pre-enrich DI series with PU columns before repeated backtests.
3. Keep deterministic DI fixtures for execution, costs, sizing, and pyramiding
   regressions.
4. Add an optional real-data smoke script for local `finharvest` caches.
5. Add a lightweight benchmark helper for fixed symbols/specs.
6. Extend `bt_run_portfolio()` to support `next_*` execution, portfolio
   pyramiding, richer entry-priority rules, and a futures margin/guarantee
   model.
7. Add job/result persistence only after the single-symbol and shared-capital
   portfolio paths are stable.
