# Data and provenance

The public CSV files contain numeric extracts of the original surveys. Each row preserves a source record; no rows are silently deduplicated. Empty CSV fields represent missing responses or questions not shown. In particular, arm-specific missing outcomes are structural: only the assigned arm's questions were shown. `scripts/00_utils.R` checks routing and combines the assigned responses before assessing missingness.

`row_id` is the source row number after removal of Lucid's two Qualtrics metadata rows. `ip_group` is a within-study integer identifying a shared original address; it is not an address, a cross-study identifier, or proof that records represent the same person. Direct identifiers, IP addresses, location fields, and free text are omitted from the public extracts. Complete originals, including Word questionnaires, remain in the author's local `private-data` archive. Git history has not been rewritten as part of this release.

The extraction command is `Rscript R/01_prepare_public_data.R /path/to/original/data`. The folder must contain the three files named in `source_manifest.csv`. It verifies unique response IDs and the one-to-one MTurk join before dropping identifiers, checks agreement on party, treatment, and economic responses between the two MTurk sources, validates labels, and emits numeric-only extracts. It does not overwrite or edit the original files. The complete public analysis starts from these extracts and does not require private data.

## Sources

- MTurk: `turk/2020_06_29.csv` supplies survey responses; `turk/merged_survey_ip_06_29_2020_final.csv` supplies existing address-review flags. Both contain 1,505 records. The source experiment was fielded June 29, 2020. Two records have no assignment; the eligible analysis begins with 1,503 assigned records.
- Lucid: the January 28, 2023 ZIP contains one Qualtrics CSV. The first two rows after its header are descriptive/import metadata. Removing them leaves 821 records. Seven are previews, and 20 of the remaining records declined consent. The nonpreview field period is January 27, 2023.
- `materials/mturk_instrument.md` and `materials/lucid_instrument.md` transcribe the author-held Word questionnaire exports. Text, response codes, blocks, and page-break markers are retained. Decorative icons and the MTurk HIT-count illustration are omitted; the original files remain archived.
- `materials/lucid_demographic_codebook.csv` is the author-held provider demographic codebook from the author-held `extreme_recall_2023` project (`data/lucid/demographic-codebook.csv`). Its party labels confirm the mapping used here. Source-file hashes for analysis exports are in `source_manifest.csv`.

## Shared columns

| Column | Meaning |
|---|---|
| `row_id`, `ip_group` | Record and anonymous address-group identifiers described above |
| `consent`, `finished` | 1 = yes/true, 0 = no/false |
| `cue_d` | 1 = Obama cue; 0 = Republican Congress cue |
| `unemployment_d`, `inflation_d` | Assigned Obama-arm responses: 1 better, 2 same, 3 worse |
| `unemployment_r`, `inflation_r` | Assigned Congress-arm responses, same coding |
| `age`, `gender`, `education` | Study-specific codes below; do not pool codes across studies |

## MTurk columns

| Column | Coding |
|---|---|
| `age` | 1 under 18; 2 18–24; 3 25–34; 4 35–44; 5 45–54; 6 55–64; 7 65–74; 8 75–84; 9 85+ |
| `gender` | Original questionnaire codes; see Q5 in the text instrument |
| `pid3_raw` | 1 Democrat; 2 Republican; 3 independent; 4 other |
| `pid_dem`, `pid_rep` | Within identified party: 1 strong, 2 weak |
| `pid_ind` | 1 leans Republican; 2 leans Democrat; 3 neither |
| `pol_interest` | 0–10 political interest |
| `ft_dems`, `ft_reps` | 0–10 feeling thermometer |
| `sleep` | 1 ≤4 hours; 2 4–6; 3 6–8; 4 8–10; 5 >10 (overlapping endpoints follow the fielded wording) |
| `prosthetic`, `vision`, `hearing`, `gang`, `family_gang` | 1 yes, 2 no, missing preserved |
| `funny_ip`, `duplicated`, `foreign_ip`, `blacklisted` | Existing source address-review flags; 1 flagged, 0 otherwise |
| `hits`, `sincerity` | Original response codes; see questionnaire |
| `education` | 1 less than high school; 2 high school; 3 some college; 4 associate; 5 bachelor's; 6 master's; 7 doctorate; 8 professional |
| `race_1` … `race_6` | Selected-option indicators: White, Black, American Indian/Alaska Native, Asian, Native Hawaiian/Pacific Islander, other; multiple selections allowed |
| `pre_economy`, `pre_unemployment`, `pre_inflation` | Assessments of 2019 before the experiment: 1 better, 4 same, 5 worse; not the experimental outcomes |

Partisans include respondents with `pid_dem` or `pid_rep` in 1–2 and independents leaning toward that party. Strict identifiers have `pid3_raw` in 1–2. The screening score adds sleep in categories 1 or 5 and “yes” to each of the five listed low-incidence items. Missing components contribute no positive flag, matching the source rule; they are not recoded to substantive “no” in the public data. At least two flags, or `funny_ip == 1`, defines the flagged sample. Other address flags are retained for audit and do not independently change the primary sample.

## Lucid columns

`preview` is 1 for Qualtrics previews. `age` is age in years. `gender`, `hhi`, `ethnicity`, `hispanic`, `education`, and `political_party` retain the provider's numeric codes; the accompanying demographic codebook supplies their labels. These are not the MTurk demographic codes.

`political_party` maps to Democrats for codes 1, 2, 3, and 6; Republicans for 5, 8, 9, and 10; and nonpartisans for 4 and 7. Strict identifiers use 1, 2, 9, and 10. Codes 6 and 8 are “other” respondents leaning toward a party and are included with leaners.

`attention_1` … `attention_5` indicate selection of extremely, very, moderately, slightly, and not-at-all interested, respectively. Passing requires exactly options 1 and 2 and no others, with a nonmissing response. All experimental response labels are normalized to lower case and matched against the three permitted labels before conversion; unexpected values stop extraction.

## Derived variables and outputs

`scripts/00_utils.R` defines eligibility, party, screen passing, own/opposing cue, scores, and category indicators. `scripts/02_estimate.R` writes `flow.csv`, cell counts and means, category distributions, all effect estimates, the primary family with adjusted p-values, and diagnostics. `effects.csv` includes overlapping sensitivity samples and should not be treated as a set of independent studies. `scripts/04_tables.R` and `scripts/03_figures.R` consume these outputs. `tabs/macros.tex` is the single source for empirical quantities repeated in the manuscript.
