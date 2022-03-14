# cordial
A Pleasant Tonic For Convenient Correlation Analysis In R

Computes pairwise Pearson's correlations of a dataset, or 
specified targets simultaneously in parallel. Conveniently includes the 
ability to filter the input dataset and select a subset of columns to 
compute correlations. Outputs Pearson's product moment correlation 
coefficients, p-values, adjusted p-values, and observation counts in 
long-format.

## Installation
**A.** Install directly from the `cordial` GitHub repository:
`devtools::install_github("iibadshah/cordial")`

**B.** Alternatively:
  1. Download all files and folders from `https://github.com/iibadshah/cordial` into a local folder named `cordial` (keeping the exact same folder structure and file names)
  2. Open RStudio > File > Open Project > navigate into the `cordial` folder > select `cordial.Rproj`
  3. Run `devtools::install(build = TRUE)`
