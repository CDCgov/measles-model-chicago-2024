# Model of 2024 Chicago measles outbreak

Code to accompany Masters et al., "Real-Time Use of a Dynamic Model to  Measure the Impact of Public Health Interventions on Measles Outbreak Size and Duration — Chicago, Illinois, 2024", _MMWR_ (2024)

<https://www.cdc.gov/mmwr/volumes/73/wr/mm7319a2.htm>

## Repo organization

- `seirMeasles/`: Code for the dynamic model, organized as an [R package](https://r-pkgs.org/)
- `scripts/`: R scripts to run and analyze the dynamic model (see [Getting Started](#getting-started))
- `input/case_series.csv`: Rash onset dates among shelter residents
- `output/`: By default, output files are written here
- `renv/`, `renv.lock`: [renv](https://rstudio.github.io/renv/articles/renv.html) files
- `.github/`: GitHub continuous integration setup
- `.lintr`: R [code linter](https://lintr.r-lib.org/index.html) configuration
- `.pre-commit-config.yaml`: [pre-commit](https://pre-commit.com/) hooks configuration
- `.secrets.baseline`: Baseline for [`detect-secrets`](https://github.com/Yelp/detect-secrets), used as a pre-commit hook

## Getting started

This is an [renv](https://rstudio.github.io/renv/articles/renv.html)-enabled repo. If you are using R 4.4.0 and have the R package `renv` installed, you should be able to install the model and precise dependency versions using `renv::restore()`.

Otherwise, you will need to `remotes::install_local("seirMeasles")` or `devtools::load_all("seirMeasles")` in order to access the model code.

The workflow is:

1. `source("scripts/run_abc.R")`: Develop intuition about parameter selection using approximate Bayesian computation (ABC).
    - `output/abc.csv` gives IQRs for prior and posterior values for $R_0$ and `int_eff`.
    - `output/abc.png` visualizes the prior and posterior densities for these variables.
2. `source("scripts/run_simulations.R")`: Use default parameters to produce simulated trajectories.
    - Simulations are stored in `output/simulations/`
3. `source("scripts/run_forecasts.R")`: Select a subset of trajectories as forecasts and analyze them.
    - `output/diagnostic_forecasts.png` shows selected forecast trajectories.
    - `output/table1.csv` is Table 1 in the publication.
4. `source("scripts/run_scenarios.R")`: Summarize outcomes across counterfactual scenarios (e.g., to look at cases averted by vaccination and interventions).
    - `output/diagnostic_scenarios.png` shows a subset of counterfactual trajectories, with observed data.
    - `output/table2.csv` is Table 2 in the publication.
    - `output/figure1.png` is similar to Figure 1 in the publication.
    - `output/figure1_summary.csv` are underlying values used to produce Figure 1.

The script `scripts/check_output_hash.R` runs the workflow above and saves a hash of all output files. If you refactor the code without changing functionality, this script can confirm that the outputs are identical.

## Model description

This is a modified version of the Operation Allies Welcome (OAW) model described in [Masters et al. *Lancet Public Health* (2023)](<https://doi.org/10.1016/S2468-2667(23)00130-5>).

### Updates from the OAW model

The OAW model and this model are both stochastic (Gillespie) compartmental (SEIR) models with 5 population classes. This model includes the following modifications:

- Split exposed compartments: This model splits the exposed ($E$) compartment into two compartments ($E_1$ and $E_2$).
  - The mean latent period $1/\sigma$ (i.e., total time in exposed compartments) remains the same, but the variance in the latent period is halved.
  - In a model with a single $E$ compartment, the transition $E \to I$ would occur at rate $\sigma$; here the transitions $E_1 \to E_2$ and $E_2 \to I$ both occur at rates $2 \sigma$.
- Active case-finding intervention: After a certain day (`int_day`), the infection period ($1/\gamma$) is reduced by some factor ("intervention efficacy", `int_eff`).
- Case ascertainment delay
  - Cases are counted after an exponentially distributed delay, starting at the transition $E_2 \to I$.
  - This change was made to more accurately reflect the delay from the start of a patient's infectious period to when they can be ascertained via presentation with rash.

### Model description

- There are 6 disease states
  1.  $S$ (susceptible),
  1.  $S_V$ (primary vaccine failure),
  1.  $E_1$ (exposed),
  1.  $E_2$ (exposed),
  1.  $I$ (infectious), and
  1.  $R$ (removed).
- There are 5 "population classes"
  1.  aged <6 months,
  1.  aged 6-11 months,
  1.  aged 12 months to 11 years,
  1.  aged 12+ years, not pregnant, and
  1.  aged 12+ years, pregnant.
- There are compartments for each of the 5 population classes and the 6 disease states.
  - Exception: $S_V$ is not tracked for the 2 vaccine-ineligible population classes (under 6 months or pregnant).
  - The code also includes three "counting" compartments for cases. These compartments are used only for comparison with the empirical rash onset dates for calibration, reporting scenario projections, and reporting forecasts. They do not affect the model dynamics.
    1. `cases`: cumulative counter of every $E_2 \to I$ transition
    1. `pre_reported_cases`: incremented at every $E_2 \to I$ transition; decremented after an exponentially distributed delay
    1. `reported_cases`: incremented when `pre_reported_cases` is decremented
- Most transitions between compartments are stochastic, following ODEs and the Gillespie algorithm.
  - Infection: $S_i \to E_{1i}$ and $S_{Vi} \to E_{1i}$ transitions, for each population class $i$, follow $\beta S_i I / N$, where $I = \sum\nolimits_i I_i$ and $N$ is the total population size.
    - The transmission rate $\beta$ is derived from $R_0$ and the recovery rate $\gamma$ via $\beta = R_0 / \gamma$.
    - The model assumes homogenous mixing between population classes. The code includes structure for non-homogenous mixing.
  - Latency: $E_{1i} \to E_{2i}$ transitions follow $2 \sigma S_i$.
    - This transition does not correspond to any specific change in real-world disease state; it is merely a mathematical tool for reducing the variability of waiting times in the $E$ compartments.
  - Onset of infectiousness: $E_{2i} \to I_i$ transitions follow $2 \sigma E_{2i}$.
  - Recovery: $I_i \to R_i$ transitions follow $\gamma I_i$.
    - The time-varying intervention increases the recovery rate $\gamma$.
  - The code includes an option to use adaptive tau leaping.
    - Informal experiments showed no substantial improvement to the speed of the code when adaptive tau leaping was used.
- Importations are encoded as deterministic additions to the $I_3$ compartment (aged 12 months to 11 years).
  - The index measles case in this outbreak, representing an importation event into the shelter population, occurred in this age group.
  - See "[Hard coding of vaccinations and importations](#hard-coding-of-vaccinations-and-importations)" below.
- Vaccinations are encoded as deterministic movements from $S$ to $S_V$ and $R$ compartments.
  - The vaccine effectiveness (first dose MMR) is the proportion of vaccinated individuals who transition from $S$ to $R$; the remainder go to $S_V$.
  - Vaccines are assumed to be administered only to individuals in $S$ and $R$ compartments (i.e., $E$ and prodromal $I$ individuals are ignored).
  - The proportions of vaccines that are allocated to $R$ individuals is assumed fixed; see "[No explicit tracking of vaccine record status](#no-explicit-tracking-of-vaccine-record-status)" below.

### Parameters

Default model parameters are in [`params.yaml`](inst/params.yaml).

### Known model limitations

#### No explicit tracking of vaccine record status

Before the outbreak, individuals fell into four categories:

1. Immune (i.e., would not become infected if exposed) with records that they were vaccinated,
1. Immune but without records, either because the records were lost, or because they were unvaccinated but recovered from an infection earlier in life,
1. Susceptible (i.e., not immune) because not vaccinated, with no record of vaccination, and
1. Susceptible with record of vaccination, because of primary or secondary vaccine failure.

In the current model implementation on each vaccination day (3 days of mass vaccination), a fixed proportion of vaccines are allocated to $R$ individuals (i.e., individuals who are immune but do not have records proving their vaccination history). This assumption simplifies the model so that disease state and vaccine record status do not need to be independently tracked.

This assumption is innocuous in this parameterization because the number of individuals infected before or during the vaccination days is small given the rapidity of mass vaccination. In a different parameterization, if many people were infected before or during a mass vaccination campaign, this assumption would bias toward favorable outcomes, by preferentially allocating vaccine to susceptibles.

In an ideal model implementation, which tracks immunity and vaccination record status, vaccines could be distributed proportionally to all individuals without vaccine records, regardless of unobservable disease state.

#### Hard coding of vaccinations and importations

- A more general solution to incorporating vaccination and importation events would be to input a set of arbitrary "move rules." Each rule is (1) a day when a movement occurs between compartments unrelated to the normal rates (e.g., an importation or vaccination) and (2) a function of the current simulation state that outputs the updated simulation state.
  - E.g., the move function for an importation would increment an infected compartment and decrement the corresponding susceptible compartment.

## Project Admin

Scott Olesen, PhD, <ulp7@cdc.gov> (CDC/IOD/ORR/CFA)

## General Disclaimer

This repository was created for use by CDC programs to collaborate on public health related projects in support of the [CDC mission](https://www.cdc.gov/about/organization/mission.htm). GitHub is not hosted by the CDC, but is a third party website used by CDC and its partners to share information and collaborate on software. CDC use of GitHub does not imply an endorsement of any one particular service, product, or enterprise.

## Public Domain Standard Notice

This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice

This repository is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

## Privacy Standard Notice

This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](https://github.com/CDCgov/template/blob/master/DISCLAIMER.md)
and [Code of Conduct](https://github.com/CDCgov/template/blob/master/code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice

Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice

This repository is not a source of government records but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).
