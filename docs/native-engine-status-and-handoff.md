# Backtestforge Native Engine Status And Handoff

Date: 2026-05-08

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
- `normalize_risk` is a comparison layer on returns, not a rewrite of the
  executed order path. When set, `rets`, `stats`, monthly/quarterly returns,
  returns summaries, and performance blocks use the risk-normalized stream;
  `raw_rets`, `raw_stats`, trades, positions, and cost/activity blocks preserve
  the unnormalised simulated execution.
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

## Futures Metadata Policy

Contract metadata comes from the `xts` object only. Do not infer multiplier,
tick size, fees, or slippage from ticker roots.

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
- `fee_type`
- `slip_value`
- `slip_type`
- `slippage_bps`
- `slippage_ticks`
- `slippage_points`
- `ps_value`
- `ps_type`

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

Backtest wrappers expose cost overrides directly. `fee_value` is account
currency, with `fee_type = "contract"` charging per unit traded and
`fee_type = "order"` charging once for the whole executed order.
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
it through to `bt_risk_spec()` for Donchian, EMA, SMA, and `bt_batch()` runs.

`ticksize` and `multiplier` are intentionally separate: `ticksize` is the minimum
price increment, while `multiplier` is the cash value of one full price point.

If multiplier or tick-size metadata is missing, the simulator still warns and
uses `1` as the fallback. Position sizing, fee, and slippage metadata are strict
and error when required values are missing.

## DI Futures Behavior

For DI-like symbols, the simulator:

- tries to fill missing maturity using `finharvest::finget_maturities()` when
  available;
- uses `brfutures` when installed to convert annualized-rate OHLC data into PU
  columns;
- keeps annualized-rate OHLC columns for indicators and signals;
- prefers PU columns (`PU_o`, `PU_h`, `PU_l`, `PU_c`, or equivalent names) for
  execution and mark-to-market.

Experimental DI pyramiding follows the same split: ATR and favorable-move
triggers are calculated in rate space, then each add is filled and marked in PU
space. Long DI positions gain when PU falls, so the simulator keeps the negative
PU P/L multiplier for positive long quantity.

The preferred data-layer improvement is to have `finharvest` or `brfutures`
return DI PU columns and contract metadata as attrs before the backtest starts.

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
6. Add job/result persistence only after the single-symbol path is stable.
