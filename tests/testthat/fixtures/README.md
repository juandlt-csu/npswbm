# Golden test fixtures

`test-golden.R` checks that npswbm returns exactly what the old code in
`nps_water_vulnerability` returned before the split.

## The baseline: `nps_water_vulnerability` at `b62f366`

We chose one commit of the playground as the reference: `b62f366`, its
`upstream/main` when npswbm was split out (2026-10-20). The golden files always
come from that commit, even after the playground changes. `make_golden.R`
extracts `setup.R` and `src/` from that commit with `git archive`, whatever the
local clone is on. The commit is set in one place, `golden_commit` at the top of
`make_golden.R`.

This is a temporary arrangement for checking the move out of the playground.
Once development has moved off the playground, `b62f366` stops being the
reference, and these tests either retire or become npswbm's own regression
baseline.

The setup is copied from wavarapid's golden tests, with these differences:
- no saved inputs (`inputs.rds`). The inputs are made-up climate and routing
  series with no random numbers, built in `golden_cases.R`, so both runs get the
  same objects anyway;
- `args` can be a function of `fn`, which looks up a function by its npswbm name
  for the run in progress. The metrics are passed by name
  (`obj_fun = fn("nse")`), and the old code calls that one `NSE`;
- `old_fun`, as in npswava, for the functions renamed to snake case;
- no arrow setting, because npswbm doesn't read parquet files.

## A second baseline: playground PR #165

The rain-on-snow melt and the rounded coordinates from playground PR #165
(issue #2) aren't in `b62f366`, so their cases are marked `baseline = "pr165"` and
compared with the code at `7bc0211`, the head of that PR (2026-10-27).
`make_golden_pr165.R` writes them, and `make_golden.R` leaves them out. Repoint
`golden_commit` in `make_golden_pr165.R` to the merge commit once the PR is
merged, if the three files changed.

The cases against `b62f366` stay as they are. They show that a run with the
default `ROS = FALSE`, and with coordinates that match exactly, still returns what
the old code did.

```sh
Rscript tests/testthat/fixtures/make_golden_pr165.R ../nps_water_vulnerability
```

It adds `golden/<id>.rds`, `golden_info_pr165.txt` and `golden_env_pr165.csv`, and
changes nothing else in `golden/`. `make_golden.R` empties `golden/` when it
succeeds, so run this one after it. `setup.R` at that commit also needs
`ggspatial`, `sfnetworks` and `tidygraph`, which npswbm doesn't use.

The same rule applies: a failure is reported, not fixed by regenerating.

## Files

- `golden_cases.R`: the inputs, the calls to compare, and the helpers both
  scripts use.
  - **Cases** (`golden_cases`): each has a function, its arguments, and whether
    it needs the shared data folder or internet. Optional fields:
    - `old_fun`: the function's name in the old code. It's filled in from the
      table `golden_old_names`, so a case only names `fun`.
    - `reads`: for a data case, the files it reads under the data folder. The
      case skips without them, and `make_golden.R` records their checksums.
    - `keep`: a function applied to the result before it is saved or compared.
      A `SpatRaster` is a pointer and can't be saved, so `golden_keep_raster()`
      keeps its shape, CRS, layer names, a sum per layer and its values (all of
      them for a small raster, the middle two rows of a large one).
      `load_wbm_rasters()` returns nothing useful and assigns its rasters in the
      global environment, so its cases keep those and then remove them.
  - **`golden_run()`** runs a case for both scripts. It stops a case that would
    write files: `save = TRUE`, `get_30_year_wbm_gridmet()` without
    `save = FALSE`, `get_wbm_aoi(download = TRUE)`, or a function that always
    writes. The defaults of those write into the shared data folder. It also
    leaves out what the functions print.

  The file checks the fields of every case when it is sourced, so a typo stops
  with an error instead of silently dropping a skip.
- `make_golden.R`: runs those calls with the old code at `b62f366` and saves the
  results to `golden/<id>.rds`. It also writes:
  - `golden/golden_info.txt`: the date, the pinned commit, the commit the clone
    was on, the data folder, and row and warning counts;
  - `golden/golden_env.csv`: R and every package version, and a checksum of each
    data file.

  It writes to a temporary folder first and replaces `golden/` only if every
  case ran, so a failed run (e.g. a service is down) leaves the old golden files
  as they were.

  Both `make_golden.R` and `test-golden.R` use sort order `"C"` (`LC_COLLATE`),
  which testthat uses inside every test anyway, because `get_wbm_aoi()` reads
  files in the order `list.files()` returns them.
- `golden/`: the saved results, about 3.4 MB. Not in git for now; whether the
  outputs can go in this public repo is an open question for the team.

To create the golden files, from the npswbm repo root in a fresh R session (the
clone needs commit `b62f366` and its `data` link; the script doesn't change the
clone):

```sh
Rscript tests/testthat/fixtures/make_golden.R ../nps_water_vulnerability
```

To run the comparison, point npswbm at the same data folder the playground's
`data` link points to:

```sh
WAVA_DATA_DIR="/path/to/shared/data" Rscript -e 'devtools::test(filter = "golden")'
```

The comparison is `expect_identical()`. Every skip says which case it is and what
is missing:

- no golden files at all: one skip for the whole file, saying to run
  `make_golden.R` (this is what `R CMD check` shows, because `golden/` is in
  `.Rbuildignore`);
- one golden file missing: `<case>: golden file missing: <path>`;
- no data: `<case>: no <missing reads> in the data folder "<path>"`, plus whether
  the path came from the option, `WAVA_DATA_DIR`, or the `"data"` fallback;
- offline: `<case>: offline; this case downloads <what>` (the `downloads` field
  in `golden_cases.R`).

A service that is down while the machine is online is not skipped: the case
errors with the service's error, so an outage is never mistaken for a pass.

The model looks for `dem`, `soil`, `slope`, `aspect` and `jtemp` in the global
environment. `test-golden.R` sets aside any you already have there, and puts them
back at the end.

If a test fails, don't regenerate the golden file or loosen the comparison: find
out why the output changed first. A failure can come from npswbm, or from
something else that changed since the golden run: an R or package upgrade, a
change to the shared data, or the one live source. The failure message lists the
R, package and data changes it finds against `golden_env.csv`, and names the
download if the case has one. To find the cause:

1. Run `make_golden.R` from a scratch copy of npswbm (so the real `golden/` stays
   as it is) to rebuild the golden files with today's packages, data and
   downloads.
2. Compare the fresh files with the saved ones (`identical()`).
3. If they are identical, the old code still gives the saved output, so the
   change is in npswbm. If they differ, the change came from packages, data or a
   download, not from npswbm.

## Not covered

- `get_future_wbm()`, `get_wbm_grids()`, `get_historic_wbm_daymet()` and
  `get_wbm_aoi(download = TRUE)`, which calls `get_wbm_grids()`. They read from
  `www.yellowstone.solutions/thredds`, which returned HTTP 404 on 2026-11-04, so
  they can't run in either version. They also differ from the old code on
  purpose: the old versions put `getwd()` in front of `path`, and npswbm uses
  `path` as given.
- `et_penman_monteith_daily()` with relative humidity columns, which errors in
  both versions: it calls `actual_vp()` without the temperatures.
- The `save = TRUE` branch of `get_30_year_wbm_gridmet()`, which writes a file.
- `stop()` calls, such as `get_nps_wbm_params(area_weighted = TRUE)` without a
  `grid`, since the comparison is of returned values.
