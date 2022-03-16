# cordial
A Pleasant Tonic For Convenient Correlation Analysis In R

Computes pairwise Pearson's correlations of a dataset, or 
specified targets simultaneously in parallel. Conveniently includes the 
ability to filter the input dataset and select a subset of columns to 
compute correlations. Outputs Pearson's product moment correlation 
coefficients, p-values, adjusted p-values, and observation counts in 
long-format.

## Installation
There are multiple options to install the package, which are listed in decreasing order of preference.

**A.** Install from `.zip` or `.tar.gz` bundle:
  1. Click [Releases](https://github.com/iibadshah/cordial/releases)
  2. Download *one* of either the `.zip`, or the `.tar.gz`
  3. Open *RStudio* > *Tools* > *Install Packages* > in the new popup under *Install from:* select *Package Archive File (.zip; .tar.gz)* > browse to and select the downloaded `.zip` or `.tar.gz` > click *Install*
  - **i :** *This will contain the datasets*

**B.** Install directly from the `cordial` GitHub repository:
  1. `devtools::install_github("iibadshah/cordial")`
  - **! :** *This will not contain the datasets*

**C.** Install from source files:
  1. Download all files and folders from `https://github.com/iibadshah/cordial` into a local folder named `cordial` (keeping the exact same folder structure and file names)
  2. Open RStudio > File > Open Project > navigate into the `cordial` folder > select `cordial.Rproj`
  3. Run `devtools::install(build = TRUE)`
  - **! :** *This will not contain the datasets*

## Notes
  - `cordial/data/` only contains empty placeholder files, as the actual data are too large to upload as source files to GitHub.
  - The `.zip` and the `.tar.gz` does contain the actual data.
