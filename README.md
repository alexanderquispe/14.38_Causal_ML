# 14.38 — Inference on Causal and Structural Parameters Using ML and AI

Teaching notebooks for MIT course **14.38**. Each topic is implemented three times —
in **R**, **Python** and **Julia** — so the same estimator can be read side by side in
whichever language the student is comfortable with.

This is course material, not a software package: there is no build system, no test
suite and no dependency manifest. The notebooks *are* the deliverable, and each one
declares its own dependencies in its first cells.

---

## Repository layout

| Path | Contents |
| --- | --- |
| `R_Notebooks/` | 29 numbered notebooks (IRkernel). The most complete set and the de-facto reference implementation. |
| `Python_Notebooks/` | 25 numbered notebooks (`python3`). |
| `Julia_Notebooks/` | 18 numbered notebooks (Julia 1.6 / 1.7), partial coverage. Also holds `hdmjl/`, a vendored Julia port of R's `hdm`. |
| `data/` | Shared datasets for all three languages. See [`data/CLAUDE.md`](data/CLAUDE.md) for per-file details. |
| `Lectures/Lecture_7/` | One-off GIS notebook used in lecture 7. |
| `DAG-book/` | Jupyter Book scaffold, still unconfigured (`title: My sample book`). Not part of the course material yet. |
| `H2O/`, `pyecon.pdf` | Reference PDFs. |

Notebooks **without** a numeric prefix (`pm1-notebook-inference.ipynb`,
`pm3-notebook-newdata-nn.ipynb`, `Untitled.ipynb`, …) are legacy Kaggle-era versions kept
for history. The numbered files supersede them and the two have diverged — edit the
numbered version.

---

## Topic index

**Notebook numbers do not line up across languages.** They agree for topics 1–12 and
then drift, because `R_Notebooks/` contains four duplicated files that consume numbers
(13 duplicates 08; 26 and 29 duplicate 21; 28 duplicates 25). Julia follows R's
numbering, not Python's. Match on the descriptive slug, never on the number — or use
this table.

| # | Topic | R | Python | Julia |
| --- | --- | --- | --- | --- |
| 1 | Linear model overfitting | `01_r-notebook-linear-model-overfiting` | `01_Python_Notebook_Linear_Model_Overfitting` | `01_Julia_Notebook_ Linear_Model_Overfitting` |
| 2 | OLS and lasso for wage prediction | `02_ols-and-lasso-for-wage-prediction` | `02_ols-and-lasso-for-wage-prediction` | `02_PM1_Notebook1_Prediction_newdata` |
| 3 | OLS and lasso for gender wage gap inference | `03_ols-and-lasso-for-gender-wage-gap-inference` | `03_ols-and-lasso-for-gender-wage-gap-inference` | `03_PM1_Notebook_Inference` |
| 4 | Some RCT examples (polio vaccine) | `04_r-notebook-some-rct-examples` | `04_py-notebook-some-rct-examples` | `04_Juli_Notebook_RCT-polio` |
| 5 | Analyzing RCT with precision | `05_r-notebook-analyzing-rct-with-precision` | `05_py-notebook-analyzing-rct-with-precision` | `05_Julia_Analyzing_RCT_with_Precision` |
| 6 | RCT reemployment experiment | `06_analyzing-rct-reemployment-experiment` | `06_analyzing-rct-reemployment-experiment` | `06_RCT_Precision_Adjustment` |
| 7 | Linear penalized regressions | `07_r-notebook-linear-penalized-regs` | `07_py-notebook-linear-penalized-regs` | `07_julia-notebook-linear-penalized-regs` |
| 8 | ML for wage prediction | `08_ml-for-wage-prediction` | `08_ML_for_wage_prediction` | — |
| 9 | Experiment on orthogonal learning | `09_r-notebook-experiment-on-orthogonal-learning (1)` | `09_py-notebook-experiment-on-orthogonal-learning` | `09_Julia_OrthogonalLearning_lab4` |
| 10 | Double lasso — convergence hypothesis | `10_double-lasso-for-the-convergence-hypothesis` | `10_double-lasso-for-the-convergence-hypothesis` | `10_Julia_pm2_notebook_jannis` |
| 11 | Heterogeneous wage effects | `11_heterogenous-wage-effects` | `11_py-heterogenous-wage-effects` | `11_py-heterogenous-wage-effects` |
| 12 | Collider bias (Hollywood) | `12_r-colliderbias-hollywood` | `12_py-colliderbias-hollywood` ⚠️ | — |
| 13 | Deep neural networks for wage prediction | `14_deep-neural-networks-for-wage-prediction` | `13_deep_neural_networks_for_wage_prediction` | `14_Julia_pm3_notebook_newdata_nn` |
| 14 | AutoML for wage prediction | `15_automl-for-wage-prediction` | `14_automl-for-wage-prediction` | — |
| 15 | Functional approximation by NN and RF | `16_functional-approximation-by-nn-and-rf` | `15_py-Functional-Approximation-By-NN-and-RF` | `16_Functional-aproximation-by-nn-and-rf` |
| 16 | Causal identification in DAGs (DAGitty) | `17_notebook-dagitty` | `16_notebook_dagitty` ⚠️ | `17_Notebook_DAGitty` |
| 17 | Causal identification with `dosearch` | `18_notebook-dosearch` | `17_notebook-dosearch` ⚠️ | — |
| 18 | DML inference for gun ownership | `19_dml-inference-for-gun-ownership` | `18_pm3_notebook_inference_clustering` | `19_Julia_pm3_notebook_inference_clustering` |
| 19 | DML with neural nets for gun ownership | `20_dml-inference-using-nn-for-gun-ownership` | `19_pm3_notebook_inference_nn` | `20_Julia-pm3-newdata-nn` |
| 20 | DML for ATE and LATE of 401(k) on wealth | `21_dml-for-ate-and-late-of-401-k-on-wealth` | `20_pm5-401k-kaggle-py` | `Julia-pm5-401k` (unnumbered) |
| 21 | Identification analysis of 401(k) with DAGs | `22_identification-analysis-of-401-k-example-w-dags` | `21_Identification Analysis of 401(k) Example w DAGs` | — |
| 22 | Debiased ML for the partially linear model | `23_debiased-ml-for-partially-linear-model-in-r` | `22_debiased-ml-for-partially-linear-model-in-python` | `23_Julia_debiased-ml-for-partially-linear-model` |
| 23 | Sensitivity analysis with Sensemakr and DML | `24_sensitivity-analysis-with-sensmakr-and-debiased-ml` | `23_sensitivity_analysis_with_sensmakr_and_debiased_ml` | — |
| 24 | Debiased ML for the partially linear IV model | `25_debiased-ml-for-partially-linear-iv-model-in-r` | `24_debiased-ml-for-partially-linear-iv-model-in-python` | `25_debiased-ml-for-partially-linear-iv-model-in-julia` |
| 25 | Weak IV experiments | `27_r-weak-iv-experiments` | `25_r_weak_iv_experiments` | `27_Julia_weak_iv_experiments` |

⚠️ = the file exists but contains only markdown; the code has not been written yet.
— = not implemented in that language.

---

## Running the notebooks

### The working directory matters

Every notebook resolves data through the relative path `../data/...`, and the Julia
notebooks resolve `include("hdmjl/hdmjl.jl")` relative to `Julia_Notebooks/`. **Launch
Jupyter from inside the language folder, never from the repository root.**

```bash
cd Python_Notebooks && jupyter lab
cd R_Notebooks     && jupyter lab
cd Julia_Notebooks && jupyter lab
```

Headless execution of a single notebook:

```bash
cd Python_Notebooks
jupyter nbconvert --to notebook --execute --inplace 10_double-lasso-for-the-convergence-hypothesis.ipynb
```

### On Google Colab

The notebooks assume a local checkout, so on Colab you need two extra steps before the
first cell: install the packages that notebook imports, and fetch the data files it
reads. This repository is public, so the datasets can be pulled directly:

```python
!wget -q -P ../data https://raw.githubusercontent.com/alexanderquispe/14.38_Causal_ML/main/data/wage2015_subsample_inference.Rdata
```

Note that `pyreadr.read_r()` does not accept URLs — the file must be on disk first.

### Kernel versions the saved outputs came from

| Folder | Kernels seen in the committed metadata |
| --- | --- |
| `R_Notebooks/` | R 3.6.1 – 4.2.1 (`ir`) |
| `Python_Notebooks/` | Python 3.8.6 – 3.9.12 |
| `Julia_Notebooks/` | Julia 1.6.5 – 1.7.3 |

---

## Dependencies

There is no central environment file. **Each notebook installs what it needs in its own
first cells** — `install.packages(...)` in R, `pip install ...` in Python, `Pkg.add(...)`
in Julia. When adding a dependency, follow that pattern rather than creating a manifest.

Two dependencies cannot be resolved from the standard registries:

- **`hdmpy`** (Python) — the Python port of R's `hdm` rigorous/plug-in lasso, used for
  `rlasso` and `rlassoEffects` in thirteen of the Python notebooks. It is not on PyPI:

  ```bash
  pip install git+https://github.com/maxhuppertz/hdmpy.git
  ```

- **`hdmjl`** (Julia) — a hand-written Julia port of the same `hdm` routines, vendored in
  this repository at `Julia_Notebooks/hdmjl/hdmjl.jl` and pulled in with
  `include("hdmjl/hdmjl.jl")`. `Julia_Notebooks/hdmjl/archive/` holds the notebooks used
  to develop the port; it is history, not a dependency.

Both exist because R's `hdm` has no direct equivalent in Python or Julia. When results
differ across languages in a penalized-regression notebook, these ports are the usual
cause.

---

## Data

`data/` holds the canonical copies. **`.RData` is the source-of-truth format even for the
Python and Julia notebooks**, which read it through `pyreadr.read_r()` and `RData.load()`
respectively. Both return a dictionary keyed by the *R object name*, which usually does
not match the filename — see [`data/CLAUDE.md`](data/CLAUDE.md) for the key of each file.
CSV mirrors exist only for `gun_clean`, `wage2015_subsample_inference` and `darfur`.

| Dataset | Used by (topic) | Description |
| --- | --- | --- |
| `wage2015_subsample_inference.Rdata` | 2, 3, 8, 13, 14 | US CPS 2015 wage subsample |
| `penn_jae.dat` | 6 | Pennsylvania reemployment bonus experiment |
| `GrowthData.RData` | 10, 22 | Barro–Lee cross-country growth data |
| `cps2012.RData` | 11 | US CPS 2012, gender wage gap |
| `gun_clean.csv` / `.RData` | 18, 19 | US county panel on gun ownership and homicide |
| `pension.RData` | 20 | SIPP 1991, 401(k) eligibility and net financial wealth |
| `darfur.csv` | 23 | Darfur survey, used for the sensitivity analysis |
| `ajr.RData` | 24 | Acemoglu–Johnson–Robinson settler mortality |

### Two path hazards to know about

1. **Filename casing.** Several notebooks ask for `.Rdata` where the file on disk is
   `.RData` (and vice versa). This works on macOS's case-insensitive volume and **fails on
   Linux and Colab**. Always copy the casing from `ls data/`. Note in particular that
   `wage2015_subsample_inference.Rdata` is the only file with a lowercase `d`.
2. **Kaggle paths.** Nine R notebooks still load from `../input/<dataset-slug>/...`, the
   mount path of the original Kaggle environment. That directory does not exist in a local
   checkout; the same files live in `data/`.

---

## Status

These notebooks were written for the library versions current in 2022 and have not been
re-executed since. Expect breakage against today's releases — in particular
`normalize=True` in scikit-learn (removed in 1.2), `DataFrame.append` in pandas (removed
in 2.0), and the Keras 3 / TensorFlow ≥ 2.16 API unification.

A review and modernization of `Python_Notebooks/` is under way; see the repository's
open issues. Contributions to the R and Julia sets are welcome too.

Committed notebooks keep their saved outputs on purpose — the outputs are part of the
teaching material. Please do not strip them to make diffs smaller.

## Contributing

Work happens on topic branches merged into `main` through pull requests. A change is
normally an edit to a single notebook, not a refactor across the tree. If you fix a path
or an API call, fix it in the notebook you are already touching rather than sweeping the
whole repository in one pull request.

## References

The material follows [*Applied Causal Inference Powered by ML and AI*](https://causalml-book.org/)
by Victor Chernozhukov, Christian Hansen, Nathan Kallus, Martin Spindler and Vasilis
Syrgkanis. Individual notebooks cite their sources inline;
the recurring ones are [arXiv:1608.00060](https://arxiv.org/abs/1608.00060) (double/debiased
machine learning) and [arXiv:1604.07125](https://arxiv.org/abs/1604.07125).

## License

MIT — see [`LICENSE.txt`](LICENSE.txt). Copyright (c) 2022 Alexander Quispe & Anzony Quispe.
