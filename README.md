# truthiness

R package to accompany project by Emma Henderson, Dan Simons, and Dale
Barr (in preparation) on the illusory truth effect.

Note: The latest version of this package is built into the [Singularity software container](library://dalejbarr/talklab/illusory-truth). To reproduce the analysis exactly, please see the [analysis recipe](https://osf.io/d3s54/) at the OSF repository for the project (https://osf.io/nvugt/).

## installation

```
devtools::install_github("dalejbarr/truthiness")
```

## basic usage

### data simulation

Simulate ratings data from an experiment with 576 subjects.

```
dat <- gen_data(576)
```

Simulate a set of response files for the same experiment.

```
simulate_resp_files(dat, "resp_files")
```

CSV files with simulated data will be stored within subdirectories
`phase1`, `phase2`, `phase3`, and `phase4`.

### pre-process raw data files from Prolific

To process the raw data, create a new RMarkdown file using the built-in template entitled "Illusory Truth Preprocessing" which you can access through RStudio (by selecting `File -> New File .. -> RMarkdown -> From Template` from the dropdown menu) or through the console by 

```r
infile <- rmarkdown::draft(tf, "illusory-truth-preprocessing", 
	                       "truthiness", edit = FALSE)
```

Alternatively, and more simply, you can just use the `preprocess()` function. See `?preprocess` for details.

**NOTE: Following a pilot study, our assumptions about the file format changed substantially.** We realized that the files would have a different format than we expected when we ran the simulations. Since nothing changed other than the file format, we wrote a second set of functions for the actual data and kept the original simulation functions, which you can call using `preprocess_simulated()` or through the RMarkdown templated name "Illusory Truth Preprocessing (Simulated)".

When you run the pre-processing scripts, you have to specify two subdirectories: a *source* subdirectory containing the raw data files as well as information about manual exclusions and a second *target* subdirectory where the anonymized, pre-processed data will be stored.

The pre-processing scripts assume that your source subdirectory contains the four files `P1.csv`, `P2.csv`, `P3.csv`, and `P4.csv`, corresponding to phases 1 to 4 of the study. This subdirectory may also contain the `.csv` files `manually_exclude_participants.csv` and `manually_exclude_phases.csv` which allow you to manually exclude the full data from specific participants and specific phases from specific participants. Using these files is optional. When you first run the scripts, these two files will be created, but will only have the column headers. Open them and add each new exclusion as a new row in the file, filling in all of the fields and saving as CSV, taking care not to otherwise change the file structure or column headers. The anonymized data will be updated once you re-run the `preprocess()` function (or re-compile the RMarkdown file).

### analyze the data

To analyze the data, use the included RMarkdown template called "Illusory Truth Analysis." You can access this template from the console using

```r
rmdfile <- rmarkdown::draft(tf, "illusory-truth-analysis", 
                            "truthiness", edit = FALSE)
```

### re-analyze Nadarevic & Erdfelder

View statistical results from re-analysis of Nadarevic & Erdfelder (2014).

```
clmm_maximal
```
