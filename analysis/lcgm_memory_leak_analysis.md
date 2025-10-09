# Memory Leak Risk Assessment for `lcgm.b.R`

## Overview
The `lcgmClass` R6 analysis class drives the jamovi latent class growth modeling (LCGM) module. It caches expensive tidySEM objects, populates jamovi tables, and stores plot states inside the results tree. The patterns below flag code paths that can accumulate large objects in memory or prevent garbage collection when the analysis is rerun with different options or datasets.

## Potential Memory Leak Patterns

### 1. Long-lived private caches that are never invalidated
`lcgmClass` memoises every expensive computation (`mx_growth_mixture`, descriptives, fit statistics, parameter tables, and class probabilities) inside private fields such as `private$.res_cache`, `private$.desc_cache`, `private$.fit_cache`, `private$.para_cache`, and `private$.cp_cache`.【F:R/lcgm.b.R†L12-L82】 Once these caches are populated, no code path resets them to `NULL`; the class invokes `private$.checkpoint()` throughout `.run()`, but the method is not defined in this class or its base type. Consequently, the first set of results stays pinned in memory for the lifetime of the analysis object, even after the user changes options or re-runs the model with a different dataset. Because `mx_growth_mixture()` returns an `MxModel` that retains data, gradients, and external pointers, the absence of cache invalidation is a strong leak candidate when analysts iterate on large models.

### 2. Retaining full individual-level posterior data irrespective of UI options
`classProbabilities` materialises the full per-case posterior table returned by `tidySEM::class_prob()` and stores it in `private$.cp_cache$individual`, together with a summary table.【F:R/lcgm.b.R†L52-L72】 The cache is populated as soon as any caller (for example the 3-step auxiliary routine) touches the active binding, even if the analyst has not requested the class-probability tables or member listings. Because the cached data frame mirrors the input sample size × number-of-classes matrix, it can be very large; without a corresponding invalidation strategy, the object will persist until the entire analysis instance is destroyed.

### 3. Results tables populated incrementally without clearing
Helper routines such as `.populateDescTable()`, `.populateFitTable()`, `.populateEST()`, and `.populateClassSizeTable()` call `table$addRow()` for every run but never clear or overwrite previous rows.【F:R/lcgm.b.R†L445-L488】 If jamovi does not deduplicate rows with the same key, each rerun appends another copy of the data to the results object. This steady growth can inflate the analysis payload and prevent old rows from being released, especially when analysts iterate over many class counts or auxiliary variables in a single session.

### 4. Storing heavyweight model objects in image state
`.setPlot()` injects `self$res`—the full `MxModel` produced by `mx_growth_mixture()`—directly into the image state, which jamovi keeps alive for plotting callbacks.【F:R/lcgm.b.R†L535-L580】 Because the plot callbacks simply forward the model to `tidySEM::plot_growth()`, the image result now owns another strong reference to the already cached `MxModel`. Unless the cache is cleared, both the private field and the image state retain the same heavyweight object, making it impossible for the garbage collector to reclaim memory between runs.

## Recommendations
- Implement a concrete `private$.checkpoint()` (or similar) that resets all caches whenever `.run()` is invoked or when key options/data change.
- Lazily compute class probabilities only when the related UI elements are enabled, and drop the cached posterior matrix after dependent outputs have been constructed.
- Replace `table$addRow()` with `table$setRow()` or clear table contents before repopulating to prevent unbounded growth of jamovi result tables.
- Serialize only the lightweight data required for plotting (e.g., predicted trajectories or summary statistics) instead of persisting the full `MxModel` inside result states.

These changes would prevent large tidySEM artifacts and duplicated result tables from accumulating in memory across iterative analysis workflows.
