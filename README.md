# cordial
A Pleasant Tonic For Convenient Correlation Analysis In R

Computes pairwise Pearson's correlations of a dataset, or 
specified targets simultaneously in parallel. Conveniently includes the 
ability to filter the input dataset and select a subset of columns to 
compute correlations. Outputs Pearson's product moment correlation 
coefficients, p-values, adjusted p-values, and observation counts in 
long-format.

## Installation

Install the full package from the binary archive file:
  1. Click [Releases](https://github.com/iibadshah/cordial/releases)
  2. Download the `.zip` asset *(not the `Source code` which only contains empty placeholder files in `data/`, see [Notes](https://github.com/iibadshah/cordial/blob/main/README.md#notes))*
  3. Install from RStudio (a.), or R console (b.):
     - a. Open *RStudio* > *Tools* > *Install Packages* > in the new popup under *Install from:* select *Package Archive File (.zip; .tar.gz)* > browse to and select the downloaded `.zip` > click *Install*
     - b. install.packages("C:/Path/to/cordial_0.01.03.zip", repos = NULL, type = "win.binary")
          - *Replace* `"C:/Path/to/cordial_0.01.03.zip"` *with a string of the actual path to the actual file. In R code backslashes (*`\`*) must be escaped (*`\\`*), otherwise use forward slashes (*`/`*).*
  - **i :** *This will contain the datasets*

## Notes
  - `cordial/data/` only contains empty placeholder files, as the actual data are too large to upload as source files to GitHub.
  - The `.zip` in the latest [release's](https://github.com/iibadshah/cordial/releases) assets does contain the actual data.
  - Download and install the `.zip` (e.g. `cordial_0.01.03.zip`) **not** the source code (e.g. `Source code (zip)`, or `Source code (tar.gz)`) from the latest [release's](https://github.com/iibadshah/cordial/releases) assets.
