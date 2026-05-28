# Applied Statistics Research

Quantitative research projects on education, social mobility, and inequality.
Methods include multilevel modelling, Bayesian inference, and longitudinal
analysis, applied to large-scale survey data (PISA, TIMSS, LSYPE, BHPS).

---

## Projects

### Gender and socioeconomic inequality in science achievement
Bayesian multilevel analysis of TIMSS 2023 data on 3,055 Grade 8 students
across 99 schools in England. Investigates whether the gender gap in science
varies across schools with different levels of economic disadvantage.
Six models fitted from frequentist OLS through to Bayesian random-slopes,
compared on a held-out test set.

**Headline finding.** Socioeconomic background is the dominant predictor
(0.30 SD per unit of home resources). The gender gap is consistent across
school types, with a tentatively larger gap in the most disadvantaged
schools (50%+ disadvantaged intake) that the data cannot distinguish from
chance.

`R` · `rstanarm` · `lme4` · `glmnet` · TIMSS 2023

→ [`gender-ses-science-achievement/`](./gender-ses-science-achievement)

---

### Multilevel modelling of educational and social attitudes
Two-part project covering school effects on mathematical achievement
(PISA 2022 Canada, 17,983 students across 747 schools) and longitudinal
change in attitudes towards family and gender roles (BHPS 1991–2007,
399 individuals across nine waves).

**Headline findings.** 17% of variation in maths scores is between schools.
Home possessions is the strongest individual predictor; private-school
attendance adds 0.34 SD after controlling for family background. The gender
gap in attitudes is large (0.31 SD) and stable across the life course, with
attitudes drifting slightly less liberal with age.

`R` · `lme4` · `lmerTest` · PISA 2022 · BHPS

→ [`school-effects-pisa-canada/`](./school-effects-pisa-canada)

---

### Determinants of income at 25
Linear model of weekly earnings at age 25 using LSYPE data on ~16,000
individuals in England, born 1989–90. Separately examines factors
collected during mandatory school years (Wave 1–4) and post-16 transitions
(Wave 5–8), with treatment of informative missingness as a substantive
finding.

**Headline finding.** Single-parent households, routine occupational
backgrounds, and non-white ethnicity all predict lower income at 25,
independent of household income quintile. Attending university at 17 adds
£13/week; having a child by 17 reduces earnings by £30/week.

`R` · LSYPE · longitudinal model selection · missing-data analysis

→ [`income-determinants-uk/`](./income-determinants-uk)

---

## Methods used across projects

Multilevel models (random intercepts, random slopes, cross-level
interactions). Bayesian inference with weakly informative priors and
posterior predictive checks. Model selection via likelihood ratio tests,
backward elimination, and held-out RMSE. Regularised regression (ridge,
lasso) as a comparison benchmark.

