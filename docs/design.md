# Design and interpretation

The unit is a survey record. Assignment invokes President Obama or the Republican Congress while supplying the same unemployment and inflation changes. The treatment is a package of actor, institution, party, and wording. It is not a vague-versus-precise response-option experiment. The random-assignment description comes from the study documentation; routing in the exports is consistent, but the Qualtrics randomizer configuration is unavailable.

The primary universe is consenting, assigned partisans including leaners, excluding Lucid previews. Both economic outcomes are complete in that universe. Own-party and opposing-party cues are defined relative to respondent party. Each party's opposing-minus-own contrast is averaged with weights equal to that party's share of the analytic sample. Conditioning on party permits a randomized cue contrast within party; party identification itself is not randomized. There are no population survey weights.

The primary outcomes are evaluation scores: worse = 0, same = 50, better = 100. The four primary comparisons (two studies by two outcomes) receive Holm-adjusted p-values in `tabs/primary.csv`. Displayed confidence intervals are two-sided 95% Welch–Satterthwaite intervals from independent cell variances. Binary better/same/worse shares, party estimates, and sensitivity analyses are secondary and unadjusted. All outcome differences expressed as percentages are multiplied by 100 and reported in percentage points. The average of the two scores is calculated at the respondent level, retaining within-person covariance.

The primary estimates use full eligible samples. Screen-passed and screen-flagged samples use the documented historical rules. MTurk's screen uses questions measured after the experiment; these conditional estimates can be affected by selection. Vision and hearing items ask about impairments, not only complete blindness or deafness. Passing is not proof of an authentic respondent, and failing is not proof of fraud. Lucid's instructed-response check precedes the experiment. Strict identifiers exclude leaners. Each restricted sample uses its own party composition weights, so changes across samples can reflect both composition and within-party differences.

Additional diagnostics include IP-clustered HC1 uncertainty (cluster-count-minus-one t critical values), within-party permutations conditional on observed arm totals (9,999 draws, fixed seed, plus-one p-values), and exact single-case deletion ranges holding original party weights fixed. These permutation tests presume exchangeability under the described assignment, rather than reconstructing an unavailable randomization algorithm. The two repeated outcomes are never treated as independent observations in a combined regression.

`tabs/balance.csv` reports contrasts in demographic codes, not covariate-adjusted effects. MTurk age and education are ordinal codes; Lucid age is in years. MTurk education is asked after the experiment, although it refers to prior attainment. `tabs/retention.csv` reports differences in screen passing on a 0–1 scale and is explicitly separate from demographic diagnostics. Neither diagnostic is used to choose the primary estimator or exclusions.

## Claim boundaries

- Supported: the cue package changes evaluations of the supplied numerical changes, with less precise evidence for Lucid unemployment.
- Not identified: an effect of vague versus precise options; a pure party-label effect; a difference in factual knowledge; a unique psychological mechanism; a nationally representative effect.
- Common exposure is not verified common comprehension or belief. No direct comprehension/recall measure of these figures was collected.
- Neither answer is designated correct. “Better” is evaluative, especially for inflation, and “about the same” can be a defensible assessment of a small change.
- The supplied figures are not verified historical endpoints. See the manuscript's BLS references. Preserving the actual stimulus is necessary to document the experiment.

No preregistration or pre-analysis plan was available. Full-sample primacy, standardization, uncertainty, multiplicity adjustment, and sensitivity checks are analysis decisions made for this research-note release.
