# Backtestforge Native-Only Implementation Plan

This document is the maintenance plan after removing the old external
backtesting stack. For the current state and verification commands, read
`docs/native-engine-status-and-handoff.md`.

## Target Architecture

`backtestforge` has three explicit layers:

1. Data layer
   - Source: `finharvest::finget()` or already loaded `xts` objects.
   - Internal `finget()` calls must request `attrs_source = "fintickers"` so
     curated XTS attrs are available to the simulator.
   - Futures helpers: use `positionsizer` for DI rate-to-PU conversion and
     canonical position-sizing math.
   - Contract metadata must come from `xts` attributes, preferably the nested
     `contract` plugin attr. Legacy flat attrs are compatibility inputs only.
   - Shape: one `xts` per symbol with at least `Open`, `High`, `Low`, `Close`.
   - Server rule: fetch once per universe/timeframe, then reuse immutable data.

2. Native simulation layer
   - Pure R functions.
   - Input: data plus strategy, risk, and execution specs.
   - Output: trades, positions, returns, stats, and annotated market data.
   - Inner loop: local vectors only; no database and no mutable package globals.
   - Quantity sizing delegates to `positionsizer`; this layer should not contain
     a parallel DI notional/rate sizing engine.
   - `bt_run_native()` is the single-instrument runner.
   - `bt_run_portfolio()` is the shared-capital multi-instrument runner. It
     builds one timeline across instruments and sizes entries from one evolving
     equity curve.

3. Service layer
   - Wrappers (`bt_eldoc()`, `bt_eldoc_exp()`, `bt_sma()`, `bt_ema()`,
     `bt_tsmom()`, `bt_batch()`, `bt_run_portfolio()`) call the native simulator
     directly.
   - `bt_batch(gen_portfolio = ...)` remains post-backtest return aggregation;
     it must not be treated as a portfolio simulator because individual order
     paths have already been fixed before aggregation.
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
- Do not reintroduce package-local DI sizing helpers such as the retired
  `.calculate_futures_di_*` path; `positionsizer` owns that math.

## ElDoc-First Scope

The immediate product target is still an impeccable baseline ElDoc backtester.
`bt_eldoc()` should stay simple. Moving-average and TSMOM wrappers may remain as
compact native systems, while exploratory ElDoc research lives in
`bt_eldoc_exp()`.

1. ElDoc entries and exits.
2. Explicit execution timing for those entries and exits.
3. Position sizing that can use ElDoc channel risk, ATR risk, notional
   allocation, or fixed contract/share counts.
4. Experimental pyramiding on top of the ElDoc event loop, exposed through
   `bt_eldoc_exp()`.
5. Clear trade/cost/result audit output.

Turtle-style behavior should be expressible as a preset/wrapper around
`bt_eldoc_exp()`, not as a separate primary engine.

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

The modes above are `backtestforge` risk-spec choices. Final quantity
calculation is performed by `positionsizer::ps_size_position()` using an
instrument spec built from the resolved XTS metadata.

Pyramiding belongs in the experimental native ElDoc path. The first add can use
its own ATR-based threshold (`pyramid_start` in ATR units); when omitted, it
falls back to `pyramid_step` for backward-compatible behavior. Later adds use
`pyramid_step`, and the number of units is capped by `max_units`. The default
`pyramid_sizing = "risk"` sizes each add independently at the time of the add;
`pyramid_sizing = "entry_qty"` instead adds `pyramid_qty_pct` times the initial
entry quantity.

DI pyramiding must preserve the DI contract model:

- indicators, signals, ATR, and pyramid triggers are computed in annualized-rate
  space;
- execution, mark-to-market, and transaction costs are computed in PU space;
- long DI positions are positive quantity and gain when PU falls; short DI
  positions are negative quantity and gain when PU rises;
- each pyramid add stores its trigger in rate space and its fill in PU space.

## Core Public Specs

- `bt_strategy_spec()` accepts `type = "donchian"`, `"eldoc"`, `"ema"`,
  `"sma"`, or `"tsmom"`, indicator parameters, side flags, and inversion flags.
- `bt_risk_spec()` stores initial equity, position-sizing value/type, sizing
  mode, max quantity, max leverage, integer quantity behavior, minimum risk,
  ATR risk settings, and `reinvest`.
- `bt_execution_spec()` stores execution mode and cost assumptions.
- `bt_run_native()` accepts a symbol or `xts`, computes indicators/signals, runs
  the event loop, and returns a `bt_native_result`.
- `bt_run_portfolio()` accepts multiple symbols or `xts` objects, computes each
  instrument's indicators/signals, and runs one shared-capital event loop.
- In `bt_run_portfolio()`, `strategy`, `risk`, `execution`, `ps_value`,
  `ps_type`, and `reinvest` can be single values or named per-instrument
  overrides. Names are matched against list labels, source symbols, ticker
  arguments, and `information$ticker` metadata.
- `normalize_risk` belongs to the returned/performance return stream. Performance
  stats and return tables should use normalized returns when supplied; execution
  facts and cost blocks should keep the raw simulated order path.
- `bt_search_native()` evaluates serializable parameter grids and returns a
  sorted data frame with full results attached as an attribute.

## Futures Requirements

- Recognized metadata: nested plugin attrs `contract`, `costs`,
  `classification`, and `information`; plus legacy flat attrs
  `fut_multiplier`, `multiplier`, `fut_tick_size`,
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
- When an `xts` object has no flat `symbol` or `ticker` attr, resolve the source
  symbol from nested attrs such as `attr(x, "information")$ticker`.
- DI rate-to-PU conversion and per-contract sizing inputs should come from
  `positionsizer`, not from local formula copies.

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
- Experimental DI pyramiding adds ATR-stepped units in rate space while filling
  in PU space.
- EMA/SMA/TSMOM wrappers run on synthetic data.
- `bt_search_native()` evaluates and sorts a small search space.
- `bt_batch()` works with exact-match preloaded data.
- `bt_batch(gen_portfolio = ...)` is documented and tested as post-backtest
  return aggregation, not shared-capital simulation.
- `bt_run_portfolio()` sizes later entries from shared equity and keeps
  per-instrument strategy/risk/execution specs separate.
- `bt_run_portfolio()` accepts per-instrument `ps_value`/`ps_type` and resolves
  unnamed direct `xts` symbols from `information$ticker`.
- Passing removed `engine` inputs fails explicitly.

Optional real-data smoke coverage should live outside the deterministic unit
tests and use local `finharvest` data when available:

- `CCMFUT_1H_AGG`, `BGIFUT_1H_AGG`, `WDOFUT_1H_AGG`, and `DI1F28`.
- `bt_eldoc()` with `breakout`, `same_close`, `next_open`, `next_close`, and
  `next_avg`.
- `bt_eldoc_exp()` for pyramiding and research diagnostics.
- `bt_tsmom()` with the horizon converted to the selected data frequency.
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

## Shared-Capital Portfolio Next Steps

The first shared-capital implementation is intentionally narrow and should be
extended in this order:

1. Add real futures fixtures for WDO/WIN/DI/CCM/BGI-style combinations with
   different strategy/risk specs.
2. Add `next_open`, `next_close`, and `next_avg` execution to the portfolio
   runner with deterministic cross-instrument event ordering.
3. Add portfolio pyramiding using the same audit fields as `bt_eldoc_exp()`.
4. Add priority modes for simultaneous entries, such as explicit ranking,
   volatility/risk ranking, or first-touch timestamps when intrabar data is
   available.
5. Add an optional futures margin/guarantee model; keep it separate from equity
   P/L so users can run both unconstrained research and margin-constrained
   simulations.
6. Add plotting support through `tradeplotr` for `bt_portfolio_result` objects.
