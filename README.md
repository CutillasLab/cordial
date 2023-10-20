# cordial
**A Pleasant Tonic For Convenient Correlation Analysis In R**

Computes pairwise Pearson's correlations of a dataset, or 
specified targets simultaneously in parallel. Additionally, fits a 
linear model. Conveniently includes the ability to filter the input 
dataset and select a subset of columns to compute correlations. 
Outputs Pearson's product moment correlation coefficients, p-values, 
adjusted p-values, linear model slope and observation counts in 
long-format.

## Installation
### RStudio
1. Download the Package Archive File (**cordial_x.x.x.tar.gz**) of the latest [release](https://github.com/iibadshah/cordial/releases/latest).
   - *Not the Source code*
3. In RStudio, click: **Tools** menu
4. Select: **Install Packages...**
5. In the **Install from** list box, select: **Package Archive File (.zip; .tar.gz)**
6. Click: **Browse** to select the downloaded `cordial` Package Archive File
7. Select: **Install**

### R console
#### One-liner
Run: `devtools::install_github("iibadshah/cordial@*release")`
   - *You may first need to install `devtools`: `install.packages("devtools")`*
#### Manual
1. Download the Package Archive File (**cordial_x.x.x.tar.gz**) of the latest [release](https://github.com/iibadshah/cordial/releases/latest).
   - *Not the Source code*
2. Run: `devtools::install_local(path = "C:/path/to/cordial_x.x.x.tar.gz")`
   - *Replace the string argument to* `path` *with the actual location*
   - *You may first need to install `devtools`: `install.packages("devtools")`*

## Notes
  - The source files in `cordial/data/` only contain *EMPTY* placeholders, as the actual data files are too large to upload as source files to GitHub.
  - The `.zip` and `.tar.gz` in the latest [release's](https://github.com/iibadshah/cordial/releases/latest) assets *DOES* contain the actual data.
