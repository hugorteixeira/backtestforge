# Backtestforge Native Engine Status And Handoff

Date: 2026-05-01

This document records the current implementation state, the parity findings,
and the next engineering steps for continuing the `backtestforge` native engine
work in a later session.

## Current Direction

The chosen direction is to make `backtestforge` a fast, deterministic
trend-following backtest package with a native in-memory engine as the default.
The legacy `quantstrat` path stays available only for migration and parity
checks.

Do not try to "fix quantstrat" as the primary path. The native engine is already
fast enough to justify continuing with it, and it avoids the global-state
problems from `.GlobalEnv`, `.blotter`, `.strategy`, and
`FinancialInstrument`.

## Implemented Surface

Native public APIs:

- `bt_strategy_spec()`
- `bt_risk_spec()`
- `bt_execution_spec()`
- `bt_run_native()`
- `bt_search_native()`

Wrappers now default to the native engine:

- `bt_eldoc(..., engine = "native")`
- `bt_ema(..., engine = "native")`
- `bt_sma(..., engine = "native")`
- `bt_batch(..., engine = "native")`

Legacy mode remains:

```r
bt_eldoc("WDOFUT_4H", up = 40, down = 20, engine = "quantstrat")
```

## Native Engine Contracts

- The native path must not create or mutate `.GlobalEnv$.blotter` or
  `.GlobalEnv$.strategy`.
- The native path must not require `quantstrat`, `blotter`, or
  `FinancialInstrument`.
- The native path keeps the input data frequency. For a 1H series, returns stay
  on the 1H index. This is different from the legacy quantstrat path, which can
  return daily account returns even when trades are intraday.
- Donchian signals now match the legacy `eldoc()` plus
  `quantstrat::sigCrossover()` behavior:
  - Donchian bands use `xts::lag.xts()` just like `eldoc(type = "data")`.
  - `NA -> TRUE` does not count as a crossover.
  - Entries require a valid opposite Donchian stop.
- Default native execution is `next_open`, which avoids same-bar lookahead.
- For legacy parity checks, use:

```r
bt_run_native(
  x,
  strategy = bt_strategy_spec("donchian", up = 40, down = 20),
  risk = bt_risk_spec(mode = "risk", risk_pct = 1),
  execution = bt_execution_spec(
    timing = "next_open",
    fee = "nofee",
    close_on_end = FALSE
  )
)
```

## Futures Metadata Policy

Do not infer futures metadata from ticker roots such as `WDO`, `WIN`, or `DI1`.
The native engine must read contract metadata from `xts` attributes only.

Recognized metadata fields include:

- `fut_multiplier`
- `multiplier`
- `fut_tick_size`
- `tick_size`
- `fut_tick_value`
- `tick_value`
- `identifiers$fees`
- `identifiers$slippage`

If metadata is missing, the native engine warns and uses `1` as the fallback.
This is intentional: it keeps backtests runnable while making bad or incomplete
data visible.

Example:

```r
attr(wdo, "fut_multiplier") <- 10
attr(wdo, "fut_tick_size") <- 0.5
attr(wdo, "identifiers") <- list(fees = 1, slippage = 1)
```

## DI Futures Behavior

For DI-like symbols, the native engine:

- tries to fill missing maturity using `finharvest::finget_maturities()` when
  available;
- uses `brfutures` when installed to convert annualized-rate OHLC data into PU
  columns;
- prefers PU columns (`PU_o`, `PU_h`, `PU_l`, `PU_c`) for execution,
  indicators, and mark-to-market.

Important current data issue:

- `DI1F28_1H` is not available as a plain symbol in `finharvest`.
- `finharvest` suggests `DI1F28_1H_AGG` and `DI1F28_1H_OLD`.
- `DI1F28_1H_AGG` currently arrives without `maturity`, `fut_tick_size`,
  `fut_multiplier`, fees, or slippage attrs.
- The native engine can derive/enrich enough to run, but pays DI PU conversion
  cost inside each backtest.

The next data-layer improvement should be to make `finharvest` or `brfutures`
return DI PU columns and contract metadata as attrs before the backtest starts.
That is not a backtest-result cache; it is data normalization.

## Benchmark Snapshot

These are informal local timings from 2026-05-01 using real `finharvest` data,
Donchian `up = 40`, `down = 20`, `risk_pct = 1`, `fee = "nofee"`.

| Symbol | Rows | Native | Quantstrat | Speedup | Parity Notes |
| --- | ---: | ---: | ---: | ---: | --- |
| `WDOFUT_4H` | 2483 | ~0.74s | ~4.05s | ~5.4x | Trades matched exactly after alignment. |
| `WDOFUT_1H` | 8110 | 2.124s | 12.011s | 5.65x | Signals matched exactly. |
| `DI1F28_1H_AGG` | 7423 | 9.889s | 33.500s | 3.39x | Not parity; native converts DI to PU, legacy path differs. |

For `WDOFUT_4H`, exact trade parity was reached under the parity config above:

- signal timestamps equal;
- 105/105 nonzero transactions aligned;
- `qty_max_abs = 0`;
- `price_max_abs = 0`.

The remaining P&L difference in legacy comparisons came from mark-to-market
frequency, not from transaction price or quantity. Native kept the 4H index;
quantstrat/blotter marked account returns on daily rows.

## Validation Snapshot

Commands run successfully after the native engine and parity fixes:

```r
devtools::document()
devtools::test(reporter = "summary")
```

Package checks:

```sh
git diff --check
R CMD build /home/hugorteixeira/Documents/Libs/backtestforge --no-manual --no-build-vignettes
R CMD check --no-manual --no-build-vignettes backtestforge_0.0.1.tar.gz
```

`R CMD check` finished with no ERROR/WARNING and one environment NOTE:

```text
Failed to connect to system scope bus via local transport: Operation not permitted
```

That NOTE is environmental and was already seen in this workspace.

## Next Session Plan

Continue in this order:

1. Data normalization for futures metadata.
   - Ensure `finharvest::finget()` or a helper around it returns contract attrs
     consistently for B3 futures.
   - Required attrs: `fut_multiplier`, `fut_tick_size`, optional
     `fut_tick_value`, `identifiers$fees`, `identifiers$slippage`, and for DI
     `maturity`.

2. DI pre-enrichment.
   - For DI intraday series, avoid converting rate OHLC to PU inside every
     backtest run.
   - Prefer storing or returning PU columns (`PU_o`, `PU_h`, `PU_l`, `PU_c`)
     from the data layer.

3. Parity fixtures.
   - Add small local fixtures for WDO Donchian parity that do not hit the
     `finharvest` database.
   - Test signal equality and transaction equality under the legacy-parity
     execution config.

4. Performance measurement harness.
   - Add a script or test helper that runs a fixed benchmark suite and reports
     rows, trades, elapsed time, and speedup.
   - Keep it outside normal CRAN-style tests if it requires the DB.

5. Server job architecture.
   - Add a job/result persistence layer only after single-symbol native
     behavior and data normalization are stable.
   - Store immutable specs, data identifiers/hashes, seeds, status, logs, and
     result artifacts.

## Useful Commands

WDO 1H benchmark:

```r
devtools::load_all(".")
x <- finharvest::finget(
  "WDOFUT_1H",
  start_date = "2023-01-01",
  end_date = Sys.Date(),
  assign = FALSE,
  single_xts = TRUE,
  consolidate = FALSE,
  mode = "raw",
  verbose = FALSE
)

native <- bt_run_native(
  x,
  strategy = bt_strategy_spec("donchian", up = 40, down = 20),
  risk = bt_risk_spec(mode = "risk", risk_pct = 1),
  execution = bt_execution_spec(timing = "next_open", fee = "nofee", close_on_end = FALSE)
)
```

DI 1H available symbol:

```r
x <- finharvest::finget(
  "DI1F28_1H_AGG",
  start_date = "2023-01-01",
  end_date = Sys.Date(),
  assign = FALSE,
  single_xts = TRUE,
  consolidate = FALSE,
  mode = "raw",
  verbose = FALSE
)
```

Current package verification:

```r
devtools::document()
devtools::test(reporter = "summary")
```

