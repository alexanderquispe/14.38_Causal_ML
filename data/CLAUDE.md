# CLAUDE.md — `data/`

Shared datasets for all three notebook folders. Notebooks reach these files as
`../data/...`, so they resolve only when the kernel's working directory is
`R_Notebooks/`, `Python_Notebooks/` or `Julia_Notebooks/` — never the repo root.

## Object keys inside the `.RData` files

`pyreadr.read_r()` (Python) and `RData.load()` (Julia) return a dictionary keyed by
the **R object name**, which usually does not match the filename. Verified by
inspecting the serialized headers:

| File | Key to index with |
| --- | --- |
| `wage2015_subsample_inference.Rdata` | `data` |
| `gun_clean.RData` | `data` |
| `cps2012.RData` | `data` |
| `ajr.RData` | `AJR` (uppercase) |
| `pension.RData` | `pension` |
| `GrowthData.RData` | `GrowthData` |

The keys for `first_reg.RData`, `m_reg.RData`, `ols*.RData` and `rlasso_ira_reg.RData`
could not be read without R; check them in a session before relying on a guess.

## Filename casing

`wage2015_subsample_inference.Rdata` is the **only** file with a lowercase `d`;
every other R file uses `.RData`. Notebooks reference both spellings
interchangeably, which survives on macOS's case-insensitive volume and fails on
Linux and Colab. Copy the casing from `ls`, never from a neighbouring notebook.

## Duplicates, and the one exception

Several datasets exist twice under two naming conventions. These pairs are
byte-identical (same MD5), so either name works:

- `GrowthData` (no extension) ≡ `GrowthData.RData`
- `m_reg.RData` ≡ `first_reg.RData` — same bytes despite unrelated names
- `ols.cl_reg.RData` ≡ `ols_cl_reg.RData`
- `ols.cra_reg.RData` ≡ `ols_cra_reg.RData`

**The exception:** `ols.ira_reg.RData` and `ols_ira_reg.RData` have identical file
sizes but **different content**. Do not treat the dot and underscore spellings as
interchangeable for this one; use whichever the notebook already references.

## Dead Kaggle paths

Nine notebooks still load from the Kaggle mount, which does not exist locally. The
substitutions are:

| Path in the notebook | Local replacement |
| --- | --- |
| `../input/wage2015-inference/wage2015_subsample_inference.Rdata` | `../data/wage2015_subsample_inference.Rdata` |
| `../input/gun-example/gun_clean.csv` | `../data/gun_clean.csv` |
| `../input/reemployment-experiment/penn_jae.dat` | `../data/penn_jae.dat` |

## Orphans

`data.csv`, `data2.xlsx`, `first_reg.RData`, `ols.cl_reg.RData` and
`ols_cl_reg.RData` are referenced by zero notebooks. Leave them alone unless asked;
do not treat them as the source for anything.

## Formats

`.RData` is the source of truth. CSV mirrors exist only for `gun_clean`,
`wage2015_subsample_inference` and `darfur`; `gun_clean.csv` (14 MB) and
`gun_clean2.csv` (16 MB) are the two largest files in the repository. `penn_jae.dat`
is whitespace-delimited and needs `sep='\s'` with `engine='python'` in pandas.
