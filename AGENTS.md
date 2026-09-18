# clockworks — Project Memory

## Package Overview

- **Package**: `clockworks` (v0.2.62)
- **Author**: Pascal Noser (noser.pascal95@gmail.com)
- **License**: MIT
- **Purpose**: A unified wrapper for multiple circadian-rhythm / rhythmicity-detection R packages. Provides consistent input/output formatting so that 13 different detection methods can be run with a single function call and return comparable, standardised results.

---

## Key Dependencies

**Wrapped rhythmicity methods** (each has its own `analyze_*.R` pipeline):
CircaN, GeneCycle, MetaCycle, prain (ARSER), TimeCycle, diffCircadian, dryR, RepeatedCircadian, HarmonicRegression

**Supporting packages**: edgeR, limma, DESeq2, nlme, dplyr, tidyr, readr, rlang, ggplot2, ComplexUpset, future, future.apply, methods

---

## Core Data Structure — `CircadianData` (S4 class)

Defined in `R/CircadianData.R` (~3,800 lines). All analyses operate on a `CircadianData` object.

### Slots

| Slot | Type | Contents |
|------|------|----------|
| `@dataset` | matrix | Features × samples (numeric) |
| `@metadata` | data.frame | Samples × attributes; **must** include a `time` column |
| `@experiment_info` | list | Auto-computed: `period`, `data_type`, `log_transformed`, `n_groups`, `repeated_measures`, `n_replicates`, `delta_t`, `n_cycles`, `missing_data_fraction` |
| `@wave_params` | list | Data frames `"original"` and/or `"batch_corrected"` with harmonic-regression cosine-wave fits for every feature |
| `@results` | list | Named by method; populated incrementally by `clockworks()` calls |

### Constructor

```r
CircadianData(
  dataset,
  metadata,
  colname_sample,   # column in metadata with sample IDs (must match dataset colnames)
  colname_time,     # column in metadata with numeric time values
  colname_group     = NULL,
  colname_subject   = NULL,
  data_type         = "normalized",   # or "count"
  log_transformed   = TRUE,
  period            = c(20, 28)       # expected circadian period range in hours
)
```

The constructor:
1. Maps user column names to internal standardised names (`time`, `group`, `subject`).
2. Validates that dataset columns match metadata rows.
3. Auto-calculates `experiment_info` (delta_t, n_cycles, n_replicates, etc.).
4. Runs `estimate_wave_params()` — harmonic regression on all features — and stores results in `@wave_params`.

---

## Primary Public API

| Function | Description |
|----------|-------------|
| `CircadianData(...)` | Constructor — creates and validates a `CircadianData` object |
| `clockworks(cd, method, method_args = list())` | Run a rhythmicity-detection method; returns updated `cd` |
| `get_results(cd, method = NULL)` | Extract results list (or a specific method's data frame) |
| `get_dataset(cd)` | Return `@dataset` matrix |
| `get_metadata(cd)` | Return `@metadata` data frame |
| `get_experiment_info(cd)` | Return `@experiment_info` list |
| `get_wave_params(cd)` | Return `@wave_params` list |
| `filter_samples(cd, ...)` | Subset samples by metadata conditions |
| `order_samples(cd, ...)` | Sort samples by metadata columns |
| `check_dataset(cd)` | Validate dataset integrity |
| `check_metadata(cd)` | Validate metadata integrity |
| `compute_relative_amplitude(cd)` | Amplitude on original scale for log-transformed data |
| `plot_feature(cd, feature)` | Plot time-series for a single feature |
| `plot_phase_estimates(cd)` | Phase-estimate visualisation across methods |
| `plot_upset(cd)` | UpSet plot of rhythmic feature overlap across methods |

S4 methods: subsetting (`[`), `colnames`, `rownames`, `dim`, `show`.

---

## Supported Methods (passed to `clockworks(cd, method = ...)`)

`"ARSER"`, `"diffCircadian"`, `"dryR"`, `"GeneCycle"`, `"JTK_CYCLE"`, `"RepeatedCircadian"`, `"LimoRhyde"`, `"LS"`, `"MetaCycle"`, `"meta2d"`, `"meta3d"`, `"RAIN"`, `"TimeCycle"`

---

## Internal Pipeline Pattern

Every method is implemented across four internal (non-exported) function files:

```
check_M(cd)          → Validate object; convert counts to log-CPM (edgeR) if needed;
                        remove batch effects (limma) for repeated measures; add
                        temporary "group" column if absent; order samples; clear
                        previous results for that method.

prepare_M(cd, grp)   → Filter by group; handle replicates (e.g. median-aggregate per
                        timepoint for ARSER/GeneCycle); extract data matrix and
                        metadata into the format expected by the external package.

execute_M(inputs)    → Merge method_args with defaults via modifyList(); identify
                        features with insufficient data and exclude them with a
                        warning; call the external detection function via do.call();
                        re-attach excluded features as NA rows.

format_M(results)    → Combine results across groups; standardise column names;
                        compute BH-adjusted p-values per group; merge harmonic-
                        regression wave parameters (prefixed "hr_"); return both
                        raw and formatted data frames.
```

Files follow the naming convention `R/{check,prepare,execute,format,analyze}_<method>.R`.  
The `analyze_<method>.R` file orchestrates all four steps for one method.

---

## Standardised Output Columns

Every `format_*` function produces a data frame with at least these columns:

| Column | Description |
|--------|-------------|
| `feature` | Feature/gene identifier |
| `group` | Experimental group |
| `period_estimate` | Estimated period (hours) |
| `phase_estimate` | Estimated phase (hours) |
| `pval` | Raw p-value |
| `pval_adj` | BH-adjusted p-value (per group) |
| `method` | Method name string |
| `hr_*` | Harmonic-regression wave parameters (amplitude, phase, mesor, etc.) |

---

## Key Internal Helper Functions

| Function | File | Purpose |
|----------|------|---------|
| `validate_exp_info()` | `validate_exp_info.R` | Check `@experiment_info` is complete before analysis |
| `count_valid()` | `count_valid.R` | Count non-NA values and unique timepoints per feature |
| `convert_to_cpm()` | `convert_to_cpm.R` | edgeR-based CPM/log-CPM normalisation |
| `remove_batch_effects()` | `remove_batch_effects.R` | limma `removeBatchEffect()` for repeated measures |
| `estimate_wave_params()` | `CircadianData.R` | Harmonic regression (cosine fit) across all features |
| `compute_relative_amplitude()` | `compute_relative_amplitude.R` | Back-transform amplitude when data is log-transformed |

---

## Bundled Example Data

Stored in `data/`, documented in `R/data.R`.

| Object | Dimensions | Description |
|--------|-----------|-------------|
| `cw_data` | 10 × 96 | Log-CPM normalised expression (2 rhythmic + 8 noise genes) |
| `cw_data_counts` | 10 × 96 | Raw RNA-seq counts for the same features/samples |
| `cw_metadata` | 96 × 4 | Columns: `Sample_ID`, `Time`, `Group`, `Subject_ID` |

Design: 2 groups (A, B), 4 subjects each, 24 timepoints (0–46 h at 2 h intervals), i.e. repeated-measures design over ~2 circadian cycles.

---

## R/ Folder Map

```
R/
├── CircadianData.R          # S4 class definition, constructor, all S4 methods,
│                            # filter_samples, order_samples, estimate_wave_params, etc.
├── clockworks.R             # Main dispatch: routes to analyze_<method>()
├── analyze_*.R              # 13 files — orchestrate check/prepare/execute/format
├── check_*.R                # 12 files — input validation + preprocessing
├── prepare_*.R              # 12 files — format inputs for external package
├── execute_*.R              # 13 files — call external function, handle missingness
├── format_*.R               # 13 files — standardise outputs
├── validate_exp_info.R      # Helper
├── count_valid.R            # Helper
├── convert_to_cpm.R         # Helper
├── remove_batch_effects.R   # Helper
├── compute_relative_amplitude.R  # Helper
└── data.R                   # Roxygen documentation for example datasets
```

---

## Typical User Workflow

```r
library(clockworks)

data("cw_data")
data("cw_metadata")

# 1. Construct CircadianData object
cd <- CircadianData(
  dataset        = cw_data,
  metadata       = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time   = "Time",
  colname_group  = "Group",
  colname_subject = "Subject_ID",
  data_type      = "normalized",
  log_transformed = TRUE,
  period         = c(20, 28)
)

# 2. Run one or more methods
cd <- clockworks(cd, method = "RAIN")
cd <- clockworks(cd, method = "JTK_CYCLE")

# 3. Extract results
res <- get_results(cd)          # list with $RAIN, $JTK_CYCLE, ...
rain_res <- get_results(cd, method = "RAIN")  # data frame for one method
```
