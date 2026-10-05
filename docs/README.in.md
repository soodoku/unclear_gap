# An Unclear Gap

## Partisan Cues and Evaluations of the Same Economic Information

**Carolyn E. Roush and Gaurav Sood**

[Paper](ms/main.pdf) · [Design and interpretation](docs/design.md) · [Data](docs/data.md) · [Validation](docs/validation.md)

People can receive the same numbers and give different assessments of them depending on which party the question brings to mind. These experiments test that possibility by presenting small declines in unemployment and inflation under either an Obama cue or a Republican Congress cue. Respondents then choose “got better,” “stayed about the same,” or “got worse.”

Across two online samples, evaluations were less favorable under the opposing-party cue. The evidence is clearest for both outcomes on MTurk and for inflation on Lucid; the Lucid unemployment estimate is less precise and includes zero. This is evidence about **evaluating supplied information**, not a direct test of factual knowledge. Both arms used the same response options, so the experiments do not identify the effect of making response options vague.

| Sample | Partisans, including leaners | Unemployment | Inflation |
|---|---:|---:|---:|
| MTurk, June 2020 | {{MTurk_N}} | {{MTurk_unemployment}} | {{MTurk_inflation}} |
| Lucid, January 2023 | {{Lucid_N}} | {{Lucid_unemployment}} | {{Lucid_inflation}} |

Entries are opposing-party minus own-party cue effects, with 95% confidence intervals. Scores are 0 for “got worse,” 50 for “stayed about the same,” and 100 for “got better.” A negative value means less favorable evaluations under the opposing-party cue. These are **score points**, not percentage-point changes in the chance of answering “got better”; those binary estimates are reported separately in the paper. Comparisons are made within respondent party and averaged using its share of the analytic sample.

The main analysis includes all consenting, assigned partisans with observed outcomes, excluding Lucid previews. Screened and strict-party samples are reported as sensitivity analyses. The MTurk quality screen uses post-experiment answers and is a heuristic, not a verified classification of fraudulent respondents. Neither sample supports nationally representative estimates. Obama versus Congress also changes the institution and named actor, so the contrast is a package of cues. The historical numbers are the figures supplied in the questionnaires, not verified descriptions of the 2016 economy.

## Reproduce

Install R 4.6 and a TeX distribution with XeLaTeX, BibTeX, and `latexmk`. From the repository root:

```sh
make restore
make check
```

`make check` runs the analysis, generates figures, tables, manuscript numbers and this README, compiles `ms/main.pdf`, lints the R sources, and runs the analytical tests. No Docker is needed. To edit this README, change `docs/README.in.md`; `make tables` supplies its estimates.

- `data/raw/`: numeric public extracts used by the analysis.
- `data/materials/`: plain-text fielded questionnaires and the Lucid demographic codebook.
- `R/`, `scripts/`: shared definitions, analysis, and document generation.
- `tabs/`, `figs/`: generated estimates, diagnostics, tables, and figures.
- `ms/main.tex`: manuscript source; `ms/references.bib`: references.
- `tests/testthat/`: coding, estimand, variance, and regression checks.

The public extracts omit direct identifiers and free text. Complete original exports are retained separately; they are not needed to reproduce the public results. Authorized holders can regenerate the extracts with `Rscript scripts/prepare_data.R /path/to/original/data`. Source hashes are recorded in `docs/source_manifest.csv`. See [data documentation](docs/data.md) for coding and provenance.

Citation metadata is in [CITATION.cff](CITATION.cff). This repository is a research note and replication package, not an installable R package.
