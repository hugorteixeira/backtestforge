# Backtestforge Native-Only Implementation Plan

This document is the maintenance plan after removing the old external
backtesting stack. For the current state and verification commands, read
`docs/native-engine-status-and-handoff.md`.

## Target Architecture

`backtestforge` has three explicit layers:

1. Data layer
   - Source: `finharvest::finget()` or already loaded `xts` objects.
   - Futures helpers: use `brfutures` when available for DI rate-to-PU
     conversion.
   - Contract metadata must come from `xts` attributes.
   - Shape: one `xts` per symbol with at least `Open`, `High`, `Low`, `Close`.
   - Server rule: fetch once per universe/timeframe, then reuse immutable data.

2. Native simulation layer
   - Pure R functions.
   - Input: data plus strategy, risk, and execution specs.
   - Output: trades, positions, returns, stats, and annotated market data.
   - Inner loop: local vectors only; no database and no mutable package globals.

3. Service layer
   - Wrappers (`bt_eldoc()`, `bt_sma()`, `bt_ema()`, `bt_batch()`) call the
     native simulator directly.
   - Server jobs should store immutable specs, data hashes, seeds, status,
     logs, and result artifacts outside the inner simulation loop.

## Non-Negotiable Constraints

- Do not infer multiplier, tick size, fees, or slippage from ticker roots.
- Read contract metadata from `xts` attrs and warn when required values are
  missing.
- The same spec and data must produce the same result.
- Backtest specs must be serializable as JSON-like lists.
- Batch/search APIs must be usable by agents without writing arbitrary R code.
- Tests must cover the package without downloading market data.

## ElDoc-First Scope

The immediate product target is an impeccable ElDoc backtester. Do not broaden
the package into a general indicator framework before this path is stable.
Moving-average wrappers may remain for compatibility, but current development
priority is:

1. ElDoc entries and exits.
2. Explicit execution timing for those entries and exits.
3. Position sizing that can use ElDoc channel risk, ATR risk, notional
   allocation, or fixed contract/share counts.
4. Generic pyramiding on top of the ElDoc event loop.
5. Clear trade/cost/result audit output.

Turtle-style behavior should be expressible as a preset/wrapper around
`bt_eldoc()`, not as a separate primary engine.

Execution modes for ElDoc:

- `breakout`: execute at the exact touched ElDoc channel price.
- `exact_price`: accepted alias for `breakout`.
- `same_close`: execute at the close of the signal candle.
- `next_open`: execute at the next candle open.
- `next_close`: execute at the next candle close.
- `next_avg`: execute at the next candle OHLC average.

Position-sizing modes for ElDoc:

- `eldoc`: risk a percent of equity using the opposite ElDoc channel as the
  sizing stop.
- `atr`: risk a percent of equity using `ATR * atr_mult` as the sizing
  distance.
- `notional`: allocate a percent of equity to notional exposure.
- `contract`: trade a fixed number of contracts/shares per entry/add.

Pyramiding belongs in the native ElDoc path. The initial implementation should
add units at ATR-based intervals (`pyramid_step` in ATR units), cap the number
of units with `max_units`, and size each add independently at the time of the
add.

DI pyramiding must preserve the DI contract model:

- indicators, signals, ATR, and pyramid triggers are computed in annualized-rate
  space;
- execution, mark-to-market, and transaction costs are computed in PU space;
- long DI positions are positive quantity and gain when PU falls; short DI
  positions are negative quantity and gain when PU rises;
- each pyramid add stores its trigger in rate space and its fill in PU space.

## Core Public Specs

- `bt_strategy_spec()` accepts `type = "donchian"`, `"eldoc"`, `"ema"`, or
  `"sma"`, indicator parameters, side flags, and inversion flags.
- `bt_risk_spec()` stores initial equity, position-sizing value/type, sizing
  mode, max quantity, max leverage, integer quantity behavior, minimum risk,
  ATR risk settings, and `reinvest`.
- `bt_execution_spec()` stores execution mode and cost assumptions.
- `bt_run_native()` accepts a symbol or `xts`, computes indicators/signals, runs
  the event loop, and returns a `bt_native_result`.
- `bt_search_native()` evaluates serializable parameter grids and returns a
  sorted data frame with full results attached as an attribute.

## Futures Requirements

- Recognized metadata: `fut_multiplier`, `multiplier`, `fut_tick_size`,
  `tick_size`, `ticksize`, `fut_tick_value`, `tick_value`, `tickvalue`,
  `fee_value`, `fee_type`, `slip_value`, `slip_type`,
  `slippage_bps`, `slippage_ticks`, `slippage_points`,
  `ps_value`, and `ps_type`.
- If `multiplier` is absent but both `tickvalue` and `ticksize` are present,
  derive `multiplier = tickvalue / ticksize`.
- Keep `ticksize` and `multiplier` separate.
- Treat `slip_value` as basis points by default. For non-DI instruments,
  convert it as `price * slip_value / 10000 * multiplier`; for DI, convert
  rate basis points using the row `TickValue`. Use `slippage_ticks` or
  `slippage_points` for explicit tick or price-point slippage, or
  `slip_type` for explicit `"ticks"`, `"price_points"`, or `"cash"`.
- Store transaction costs separately: `fees` for commission, `slippage` for
  slippage cost, and `total_cost = fees + slippage` for the equity debit.
- Print a `Costs & Slippage Summary` before `Returns Summary`, including
  cost values and cost impact percentages versus gross P/L.
- Expose scenario-test cost overrides in wrappers and specs: `fee_value` in
  account currency, `fee_type = "contract"` or `"order"`, plus
  `slip_value` with `slip_type = "bps"`, `"ticks"`, `"points"`, or
  `"cash"`.
- Expose `ps_value` and `ps_type` for position sizing. `NULL` values are
  resolved from ticker metadata; missing position-sizing, fee, or slippage
  metadata errors instead of guessing.
- Expose `initial_equity` in Donchian, EMA, SMA, and batch wrappers. The native
  default starting capital is `100000`.
- For DI contracts, keep annualized-rate OHLC columns for indicators/signals and
  prefer PU columns for execution and mark-to-market.

## Tests

Minimum test coverage:

- Donchian returns are finite and shaped correctly.
- Metadata warning/fallback behavior is explicit.
- Metadata attrs from `finharvest` are consumed without external registration.
- DI uses rate OHLC for signals and PU for execution when PU columns exist.
- DI ElDoc execution modes convert rate triggers to PU fills where appropriate.
- DI position sizing converts the sizing stop from rate to PU before computing
  per-contract risk.
- DI cost tests cover fee type and slippage units (`bps`, `ticks`, `points`,
  and `cash`).
- DI pyramiding adds ATR-stepped units in rate space while filling in PU space.
- EMA/SMA wrappers run on synthetic data.
- `bt_search_native()` evaluates and sorts a small search space.
- `bt_batch()` works with exact-match preloaded data.
- Passing removed `engine` inputs fails explicitly.

Optional real-data smoke coverage should live outside the deterministic unit
tests and use local `finharvest` data when available:

- `CCMFUT_1H_AGG`, `BGIFUT_1H_AGG`, `WDOFUT_1H_AGG`, and `DI1F28`.
- `bt_eldoc()` with `breakout`, `same_close`, `next_open`, `next_close`, and
  `next_avg`.
- Assertions: no metadata fallback warning, at least one trade when signals are
  present, finite returns, finite equity, and nonnegative costs.
- Command: `Rscript scripts/smoke-native-real-data.R`.
- By default the smoke uses explicit scenario costs (`fee_value = 4`,
  `fee_type = "contract"`, `slip_value = 7`, `slip_type = "bps"`). Set
  `BT_SMOKE_STRICT_METADATA=true` to require fee/slippage attrs from the ticker
  itself.

## Verification Checklist

Run from the repository root:

```r
devtools::document()
devtools::load_all(".")
devtools::test(reporter = "summary")
```

Then run:

```sh
git diff --check
R CMD INSTALL .
```
