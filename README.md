# cordial
**A Pleasant Tonic For Convenient Correlation Analysis In R**

Computes pairwise Pearson's correlations of a dataset, or 
specified targets simultaneously in parallel. Conveniently includes the 
ability to filter the input dataset and select a subset of columns to 
compute correlations. Outputs Pearson's product moment correlation 
coefficients, p-values, adjusted p-values, and observation counts in 
long-format.

## Installation

Install the full package from the binary archive file:
  1. Click [Releases](https://github.com/iibadshah/cordial/releases)
  2. Download the `.zip` asset *(not the `Source code`, see [Notes](https://github.com/iibadshah/cordial/blob/main/README.md#notes))*
  3. Install:
     - RStudio:
       - Open *RStudio* > *Tools* > *Install Packages* > in the new popup under *Install from:* select *Package Archive File (.zip; .tar.gz)* > browse to and select the downloaded `.zip` > click *Install*
     - R console:
       - Run: `install.packages("C:/Path/to/cordial_0.01.03.zip", repos = NULL, type = "win.binary")`
       - Alternatively, run: `devtools::install_local(path = "C:/Path/to/cordial_0.01.03.zip")`
       - **:information_source: :** *Replace* `"C:/Path/to/cordial_0.01.03.zip"` *with a string of the actual path to the actual file. In R code backslashes (*`\`*) must be escaped (*`\\`*), otherwise use forward slashes (*`/`*).*
  - **:information_source: :** *Installing the [Release](https://github.com/iibadshah/cordial/releases) will include the package datasets*

## Notes
  - The source files in `cordial/data/` only contain *empty* placeholders, as the actual data files are too large to upload as source files to GitHub.
  - The `.zip` in the latest [release's](https://github.com/iibadshah/cordial/releases) assets *does* contain the actual data.
  - Download and install the `.zip` (e.g. `cordial_0.01.03.zip`) ***not*** the source code (e.g. `Source code (zip)`, or `Source code (tar.gz)`) from the latest [release's](https://github.com/iibadshah/cordial/releases) assets.
