# VisualizationRsChina

This directory is organized as an R analysis project. Existing raw data and
serialized objects were moved without recalculation or content changes on
2026-09-21. Historical PDFs were moved by source script, with the recovery
exception documented below.

## Version V1: organised analysis baseline

This repository is the **V1 organised baseline**.  It freezes the current
sample-selection rule and the corresponding analysis code; it is not a claim
that this is the final sample set for every future analysis.

The starting cohort contains **340 samples**.  The historical review first
identified **61 directly abnormal samples**.  Because the analysis is carried
out on the `Rep3` unified experimental-unit definition, any unit represented by
one of those direct exceptions is removed as a whole.  This expands the
exclusion to **49 unified experimental units, comprising 192 samples**, and
leaves **148 samples** for the V1 analysis.

```text
340 starting samples
  - 61 directly abnormal samples (historical identification)
  -> remove 49 affected Rep3 unified experimental units (192 samples in total)
148 samples retained for V1
```

The direct-exception IDs are retained in
`01.rawdata/Group/01.data_preparation/legacy_problem_sample_ids.tsv`; the
expansion to `Rep3` units is implemented in `00.libs/01.data_preparation.Rmd`.
Raw inputs are intentionally excluded from Git, so the source data must be
obtained separately.  Any later change to the sample-selection rule or retained
sample count should be recorded as a new analysis version, with its rationale
and counts documented rather than modifying the meaning of V1 retrospectively.

## Layout

```text
00.libs/                         ordered analysis scripts and reusable functions
01.rawdata/                      immutable original inputs
02.processed/                    regenerable intermediate outputs
03.figure/<script-stem>/         routine PDF output grouped by producing script
04.datasave/Rdata/               durable legacy RData objects
05.mainfigure/                   reserved for manually curated manuscript figures
```

`05.mainfigure/` has intentionally not been populated.  Existing figures remain
under their script-level directory and retain their historical file names; only
new outputs should use the `M_` (main-figure candidate) or `S_` (secondary)
prefix convention.

## Script order and scope

1. `01.data_preparation.Rmd` prepares the group and taxonomy objects.
2. `03.arg_analysis.Rmd` creates ARG summaries, figures, and the durable
   species-level ARG-host profile required by the integrated workflow.
3. `02.microbiome_analysis.Rmd` creates microbiome summaries, figures, and
   ARG host–microbiome integration analyses.
4. `04.figure_composition.R` composes selected microbiome/ARG outputs.
5. `05.sample_map.R` creates the sample map.
6. `07.sample_filter_exploration.R` contains exploratory sample-filter plots.
7. `90.legacy_arg_transfer.R` is retained for provenance and is not a
   standalone reproducible workflow.

All routine figure scripts now use `00.libs/functions/project_paths.R`, which
enforces PDF output, a white background, and a hard stop before an existing
figure or RData file is overwritten.

`01.data_preparation.Rmd` reads
`01.rawdata/Group/01.data_preparation/legacy_problem_sample_ids.tsv`. This is
the explicit list of 61 directly abnormal samples exported from the historical
`ProblemID.Rdata` workflow (including `CS53`).  The script expands those IDs to
the affected `Rep3` unified experimental units before filtering, which is the
V1 340-to-148 sample-selection rule described above.  This preserves the legacy
decision without a circular dependency on a previously generated RData file.

## Execution check (2026-09-21)

The following scripts completed in fresh temporary output directories without
writing to or overwriting project results:

- `01.data_preparation.Rmd`;
- `03.arg_analysis.Rmd`, in a separate R session using only the two group and
  taxonomy RData outputs from `01` (18 PDFs and 3 new ARG result objects);
- `02.microbiome_analysis.Rmd`, in a separate R session using the results of
  `01` and `03` (20 PDFs and 3 microbiome result objects);
- `04.figure_composition.R` (3 PDFs), `05.sample_map.R` (1 PDF), and
  `07.sample_filter_exploration.R` (6 PDFs).

The normal R console messages about deprecated ggplot2 theme arguments,
unrepeated NMDS optima, and dense network edges are warnings from the retained
legacy plotting/analysis functions; no execution-stopping error occurred.

Compatibility repairs made during this check preserve inputs, sample groups,
filters, and statistical thresholds.  They make cross-script ARG objects
explicit, qualify masked `LorMe04` functions, recreate legacy serialized
ggplot objects from their retained plotting data, and replace retired online
map lookups with fixed city-centre coordinates plus an offline map coordinate
system.

## Curated network outputs

The previous all-taxa network drafts are retained in `02.microbiome_analysis.Rmd`
for provenance but are excluded from batch execution because they generated
uninterpretable networks with tens of thousands of edges and no saved figures.
The current reproducible outputs are:

- `03.figure/02.microbiome_analysis/S_Genus_rh_network.pdf`: genus-level
  rhizosphere network, filtered to 60 candidate taxa, 49 retained nodes, and
  150 retained edges.
- `03.figure/02.microbiome_analysis/S_Species_rh_network.pdf`: stricter
  species-level supplementary network, filtered to 50 candidate taxa, 31
  retained nodes, and 53 retained edges.

For each figure, matching node, edge, and parameter TSV files are under
`04.datasave/network/02.microbiome_analysis/`.  The parameters record the
prevalence filter, Spearman rho cutoff, BH-FDR cutoff, and edge cap used for
that exact output.

## Reproducibility boundaries

- `01.data_preparation.Rmd` still depends on three objects and one CSV in the
  external `01zyz_211126_BP-Tracer/Visualization2` project.  They were not
  copied or modified here.
- `90.legacy_arg_transfer.R` references absent
  `01.rawdata/RsEcoRiskARGTransZYL/` inputs and an external profile script.
  It should be repaired only after those sources are located.
- Root `.RData` and `.Rhistory` are pre-existing RStudio session artifacts and
  were deliberately left untouched; they are not analysis outputs.

## Recovery required for two historical PDFs

During the initial file move, two identically named PDFs from separate legacy
folders were unintentionally replaced while being merged. The surviving files
have been renamed to show their provenance, and future script output names are
now distinct. Recover these two pre-move files from the local snapshot
`com.apple.TimeMachine.2026-09-21-135551.local` if they are needed:

- former `03.figure/fig1/PCA_ARG_st.pdf` ->
  `03.figure/03.arg_analysis/S_ARG_host_PCA_st.pdf`
- former `03.figure/fig2/ARG_host_ppm.pdf` ->
  `03.figure/03.arg_analysis/S_ARG_host_ppm.pdf`

The snapshot was verified to exist, but automated access requires an
administrator-authenticated Time Machine/Finder restore. No other figure,
raw-data, or serialized-object loss was identified.
