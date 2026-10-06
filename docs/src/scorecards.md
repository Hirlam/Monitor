# Scorecards

`Run_verobs_all` can generate an optional WebgraF `Scorecards` entry from
Monitor's joint significance calculation. Enable it with `SCORECARDS=T` and
`SIGN_TEST_JOINT=T` in the experiment environment file. At least two
experiments must be listed in `EXP`; `DISPLAY_EXP` must contain the same
number of unique names in the same order. The first experiment in `EXP` is the
reference, and each remaining experiment is compared with it.

## Configure an experiment

Python 3 and Matplotlib are needed only when scorecards are enabled.
Start from the tracked environment template, then make a working copy:

```bash
cd scr
cp Env_exp Env_exp_scorecards
cd ..
```

Edit `Env_exp_scorecards` for the data and experiments being compared. Set
`EXP` and `DISPLAY_EXP` in matching order, set `SCORECARDS=T`, and ensure
`SIGN_TEST_JOINT=T`. Enable scorecards for either or both domains by including
`GEN` in `SURFPLOT` and/or `TEMPPLOT`; set `SURFPAR` and/or `TEMPPAR` to the
parameters to include. The corresponding `SURFSELECTION` or `TEMPSELECTION`
must include `ALL`, and `SURFINI_HOURS` or `TEMPINI_HOURS` must include `ALL`.
For example, the tracked `Env_exp` already enables `GEN`, joint significance,
and `ALL` for both domains; add the scorecard setting and supply at least two
experiments in `EXP` and matching unique display names in `DISPLAY_EXP`.

From the repository root, create and activate a Python environment for the
renderer:

```bash
python3 -m venv .venv-scorecards
source .venv-scorecards/bin/activate
python -m pip install -r src/python/requirements-scorecards.txt
export SCORECARD_PYTHON="$PWD/.venv-scorecards/bin/python"
```

Then run Monitor from `scr/` with the working environment file:

```bash
cd scr
./Run_verobs_all Env_exp_scorecards
```

The renderer uses the all-station (`station_scope=0`), `ALL` selection, and
`ALL` initial-time exports. It writes PNG and CSV files to
`WebgraF/<PROJECT>/Scorecards/` and keeps the selected versioned text exports
in `WebgraF/<PROJECT>/Scorecards/data/`. CSV files contain paired-case counts
for each lead. The default upper-air levels are 300, 500, 700, and 850 hPa;
override them with `SCORECARD_LEVELS`, for example
`SCORECARD_LEVELS="925,850,700,500"`. Period choices are shown in WebgraF
when multiple periods are generated; files retain their period in their names.

The plotted value is exactly
`100 * (mean reference-case RMSE - mean comparison-case RMSE) / mean of both
case RMSEs`, where each case RMSE is calculated over the paired observations.
Positive values mean the comparison has lower RMSE. A black outline marks
significance at the configured confidence level. The color scale uses
significant values when available, and values outside the scale saturate at
its endpoints. The footer counts positive and negative squares among
significant values and among all values; exact zeros are excluded.

The renderer accepts only versioned scorecard exports with
`schema_version=1`, as written by Monitor's joint-significance exporter.
