# truthiness

R package to accompany project by Emma Henderson, Dan Simons, and Dale
Barr (in preparation) on the illusory truth effect.

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

### re-analyze

View statistical results from re-analysis of Nadarevic & Erdfelder (2014).

```
clmm_maximal
```
