# Backtestforge Native Engine Implementation Plan

This plan is written so an agent with no prior context can continue the work
without rediscovering the architecture. The goal is to turn `backtestforge` into
a fast, deterministic, server-safe trend-following backtest package.

For the current implementation snapshot, benchmark results, parity findings,
and next-session continuation notes, read
`docs/native-engine-status-and-handoff.md` first.

## Target Architecture

`backtestforge` should have three explicit layers:

1. Data layer
   - Source: `finharvest::finget()` or already loaded `xts` objects.
   - Futures helpers: use `brfutures` when available for DI rate-to-PU
     conversion. Contract metadata must come from `xts` attributes.
   - Contract: data is returned to the caller, never assigned into `.GlobalEnv`.
   - Shape: one `xts` per symbol with at least `Open`, `High`, `Low`, `Close`.
   - Server rule: fetch once per universe/timeframe, then reuse immutable data.

2. Native simulation layer
   - Pure R functions.
   - No `.GlobalEnv`, `.blotter`, `.strategy`, or `FinancialInstrument` writes.
   - Input: data plus strategy, risk, and execution specs.
   - Output: trades, positions, returns, stats, and annotated market data.
   - Inner loop: local vectors only; no database and no mutable package globals.

3. Compatibility/service layer
   - Existing wrappers (`bt_eldoc()`, `bt_sma()`, `bt_ema()`, `bt_batch()`) get
     an `engine` argument.
   - `engine = "native"` is the default path.
   - `engine = "quantstrat"` remains available for regression/parity checks.
   - Future server jobs should store immutable specs, data hashes, seeds, status,
     and result artifacts outside the inner simulation loop.

## Non-Negotiable Constraints

- Native backtests must not create or mutate `.GlobalEnv` objects.
- Native backtests must not require `quantstrat`, `blotter`, or
  `FinancialInstrument` at runtime.
- Native Brazilian futures handling must not call private `brfutures` helpers
  that register `FinancialInstrument` objects. Reuse exported helpers only.
- Do not infer multiplier, tick size, fees, or slippage from ticker roots. Read
  them from the `xts` attributes/metadata. If missing, warn and use `1`.
- The same spec and data must produce the same result.
- Backtest specs must be serializable as JSON-like lists.
- Batch/search APIs must be usable by agents without writing arbitrary R code.
- Tests must cover the native path without downloading market data.

## Phase 1: Native Core

Implement a new `R/native_engine.R` with:

- `bt_strategy_spec()`
  - Accepts `type = "donchian"`, `"eldoc"`, `"ema"`, or `"sma"`.
  - Stores indicator parameters and side flags.
  - Produces a simple list with class `bt_strategy_spec`.

- `bt_risk_spec()`
  - Stores initial equity, risk percentage, sizing mode, max quantity, max
    leverage, integer quantity behavior, and minimum risk.
  - Default behavior should match the current Donchian intent: risk-based
    sizing from channel stop distance when a stop is available.

- `bt_execution_spec()`
  - Stores execution timing and cost assumptions.
  - Defaults to next-bar open execution to avoid lookahead.
  - Supports `same_open` for legacy quantstrat parity checks.
  - Supports `fee = "normal"` and `fee = "nofee"`.

- `bt_run_native()`
  - Accepts a symbol or an `xts` object.
  - Fetches with `finharvest` only when data is not already supplied.
  - Computes indicators/signals.
  - Runs an in-memory event loop.
  - Returns a list with at least `rets`, `stats`, `trades`, `rets_acct`,
    `mktdata`, `positions`, and `spec`.
  - If `only_returns = TRUE`, returns only the `xts` returns object.

Expected output columns:

- Returns: `Discrete`, `Log`.
- Trades: `symbol`, `side`, `qty`, `qty_delta`, `price`, `fees`, `reason`,
  `equity`.
- Positions: `qty`, `close`, `equity`, `position_value`.
- Market data: original OHLC plus indicator and signal columns.

Futures requirements:

- Resolve contract metadata from attributes only. Recognized fields include
  `fut_multiplier`, `multiplier`, `fut_tick_size`, `tick_size`,
  `fut_tick_value`, `tick_value`, `identifiers$fees`, and
  `identifiers$slippage`.
- If any required metadata is absent, emit a warning and use `1` as the fallback
  value. This keeps tests runnable but makes bad data visible.
- For DI contracts, prefer PU columns for execution and mark-to-market when
  available (`PU_o`, `PU_h`, `PU_l`, `PU_c`, or `PU_open`, `PU_high`,
  `PU_low`, `PU_close`).
- If DI data is in annualized-rate OHLC and `brfutures` is installed, enrich the
  series through `brfutures::di_ohlc_to_pu_augmented_xts()` using the maturity
  attribute or `brfutures::di_maturity_from_ticker()`.
- DI tick-size and PU conversion rules should follow `brfutures`; do not keep a
  separate hand-written DI formula unless `brfutures` is unavailable.

## Phase 2: Public Wrapper Migration

Update:

- `bt_eldoc()`
- `bt_ema()`
- `bt_sma()`
- `bt_batch()`

Required behavior:

- Add `engine = c("native", "quantstrat")`.
- Default to `"native"`.
- For `"native"`, call `bt_run_native()` with equivalent specs.
- For `"quantstrat"`, keep the existing implementation untouched.
- Keep return shape compatible with existing batch/returns extraction.

## Phase 3: Agent Search API

Implement `bt_search_native()`:

- Accepts one dataset and a parameter space.
- Supports grid or random sampling.
- Returns a data frame sorted by objective.
- Keeps full result objects as an attribute, not as huge data-frame columns.
- Supports `workers > 1` through process-level parallelism where available.

Default objective order:

1. `total_return`
2. `sharpe`
3. `max_drawdown`
4. `num_trades`

## Phase 4: Tests

Create `tests/testthat/` if it does not exist.

Minimum tests:

- Native Donchian returns are finite and shaped correctly.
- Native wrapper does not create `.GlobalEnv$.blotter` or `.GlobalEnv$.strategy`.
- Native EMA/SMA wrappers run on synthetic data.
- `bt_search_native()` evaluates a small search space and returns sorted rows.
- `bt_batch(..., engine = "native")` works with exact-match preloaded data.

No test should require internet, finharvest DB access, or quantstrat.

## Phase 5: Documentation And Package Surface

Update:

- `DESCRIPTION`
  - Description should say native engine first, quantstrat legacy second.
  - Add `testthat` to `Suggests`.

- `README.md`
  - Present native engine as the default path.
  - Explain `engine = "quantstrat"` as legacy/parity mode.
  - Include examples for direct native run, wrapper run, batch run, and search.

- Roxygen docs
  - Regenerate `NAMESPACE` and `man/*.Rd` with `devtools::document()`.

## Phase 6: Verification Checklist

Run these commands from the repository root:

```r
devtools::document()
devtools::load_all(".")
testthat::test_dir("tests/testthat")
```

Then run a smoke benchmark:

```r
res <- bt_eldoc(synthetic_xts, up = 20, down = 10, engine = "native")
stopifnot(inherits(res, "bt_native_result"))
```

Success means:

- Native tests pass.
- Documentation regenerates.
- `bt_eldoc(..., engine = "native")` returns results without quantstrat state.
- `bt_eldoc(..., engine = "quantstrat")` still routes to the legacy path.
- `bt_batch(..., engine = "native")` can run multiple specs without global
  backtest state.

## Deferred Work

These are intentionally outside the first implementation pass:

- Exact transaction-by-transaction parity with quantstrat.
- Database-backed server queue tables.
- Cross-process result artifact persistence.
- Multi-asset portfolio margin model.
- Walk-forward train/test optimization.

Those should be added after the native single-symbol engine is stable and
covered by tests.
