# MappingCalc v2.0

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/MappingCalc)](https://CRAN.R-project.org/package=MappingCalc)
[![GitHub version](https://img.shields.io/github/r-package/v/EdimansyahAbdin/MappingCalc)](https://github.com/EdimansyahAbdin/MappingCalc)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://img.shields.io/badge/ORCID-0000--0002--1016--3298-brightgreen)](https://orcid.org/0000-0002-1016-3298)
<!-- badges: end -->

A web-based calculator and R package for mapping clinical instrument scores to
preference-based health utility values. MappingCalc implements validated
regression and beta-mixture mapping algorithms derived from Singapore population
studies.

---

## Supported Instruments

| Instrument | Full Name | Utility Target |
|---|---|---|
| **PANSS** | Positive and Negative Syndrome Scale | EQ-5D-5L |
| **SQLS** | Schizophrenia Quality of Life Scale | EQ-5D-5L |
| **WHODAS 2.0** | WHO Disability Assessment Schedule 2.0 | EQ-5D-5L |
| **PHQ-8** | Patient Health Questionnaire-8 | EQ-5D-5L |
| **EQ-5D-5L** | EuroQol 5-Dimension 5-Level | Utility index |

---

## Web Application

The hosted Shiny application is freely accessible at:

**https://eastats.shinyapps.io/MappingCalc**

No installation required. Suitable for clinical and research use.

---

## Installation

### From CRAN (Stable Release)

```r
install.packages("MappingCalc")
```

### From GitHub (Latest Development Version)

```r
# Install remotes if not already installed
install.packages("remotes")

# Install MappingCalc from GitHub
remotes::install_github("EdimansyahAbdin/MappingCalc")
```

### Install a Specific Version

```r
# Install a specific tagged release (e.g., v2.0.0)
remotes::install_github("EdimansyahAbdin/MappingCalc@v2.0.0")
```

---

## Quick Start

```r
library(MappingCalc)

# Launch the Shiny application locally
run_app()

# Example: PANSS utility score
panss_utility_score(positive = 20, negative = 18, general = 35)

# Example: EQ-5D-5L utility index from profile
eq5d5l_index_value(mo = 2, sc = 1, ua = 2, pd = 3, ad = 2)
```

---

## Citation

If you use MappingCalc in your research, please cite the package as follows.

You can also obtain the citation directly in R:

```r
citation("MappingCalc")
```

### APA Format

> Abdin et. al. (2026). *MappingCalc: A web-based calculator and R package
> for mapping clinical scales to preference-based utility values*
> (Version 2.0.0) [R package].
> https://CRAN.R-project.org/package=MappingCalc

### BibTeX

```bibtex
@Manual{MappingCalc2026,
  title  = {MappingCalc: A Web-Based Calculator and R Package for
             Mapping Clinical Scales to Preference-Based Utility Values},
  author = {Edimansyah Abdin},
  year   = {2026},
  note   = {R package version 2.0.0},
  url    = {https://CRAN.R-project.org/package=MappingCalc}
}
```

> **Note:** A methods paper describing the algorithms and validation of
> MappingCalc is currently under preparation. Once published, please
> cite the journal article alongside the package. The reference will be
> updated here upon acceptance.

---

## Key References

The mapping algorithms in MappingCalc are based on the following
Singapore population studies:

- Abdin, E., Subramaniam, M., Vaingankar, J. A., Luo, N., & Chong, S. A.
  (2014). Mapping schizophrenia quality of life scale to EQ-5D utility
  scores in a sample of patients with schizophrenia in Singapore.
  *Quality of Life Research*, 23(1), 105-112.

- Abdin, E., Chong, S. A., Seow, E., Peh, C. X., & Subramaniam, M.
  (2016). Mapping the Positive and Negative Syndrome Scale to the
  EQ-5D-5L in patients with schizophrenia.
  *Value in Health*, 19(8), 1010-1016.

- Seow, L. S. E., Abdin, E., Vaingankar, J. A., Picco, L., Chong, S. A.,
  & Subramaniam, M. (2016). Mapping the World Health Organization
  Disability Assessment Schedule 2.0 to EQ-5D-5L utility scores in a
  psychiatric sample. *Value in Health*, 19(8), 1065-1072.

- Abdin, E., Chong, S. A., Seow, E., & Subramaniam, M. (2018).
  Mapping PHQ-8 scores to EQ-5D utility values using beta mixture
  models in a large psychiatric sample.
  *Psychiatry Research*, 268, 531-537.

---

## Author

**Edimansyah Abdin**  
Institute of Mental Health, Singapore  
ORCID: [0000-0002-1016-3298](https://orcid.org/0000-0002-1016-3298)  
Email: edimansyah.bin.abdin@gmail.com

---

## License

This package is licensed under the [MIT License](LICENSE.md).
