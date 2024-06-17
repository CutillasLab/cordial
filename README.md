# cordial
**A Pleasant Tonic For Parallel Correlation Analysis In R**

Computes pairwise Pearson's correlations of a dataset, or 
specified targets simultaneously in parallel. Additionally, fits a 
linear model. Conveniently includes the ability to filter the input 
dataset and select a subset of columns to compute correlations. 
Outputs Pearson's product moment correlation coefficients, p-values, 
adjusted p-values, linear model slope and observation counts in 
long-format.

## Installation
### RStudio
1. Download the Package Archive File (**cordial_x.x.x.tar.gz**) of the latest [release](https://github.com/CutillasLab/cordial/releases/latest).
   - *Not the Source code*
3. In RStudio, click: **Tools** menu
4. Select: **Install Packages...**
5. In the **Install from** list box, select: **Package Archive File (.zip; .tar.gz)**
6. Click: **Browse** to select the downloaded `cordial` Package Archive File
7. Select: **Install**

### R console
#### One-liner
Run:
```
devtools::install_url(
 "https://github.com/CutillasLab/cordial/releases/download/v0.1.10/cordial_0.1.10.tar.gz"
)
```
   - *You may first need to install* `devtools`*:* `install.packages("devtools")`

#### Manual
1. Download the Package Archive File (**cordial_x.x.x.tar.gz**) of the latest [release](https://github.com/CutillasLab/cordial/releases/latest).
   - *Not the Source code*
2. Run: `devtools::install_local(path = "C:/path/to/cordial_x.x.x.tar.gz")`
   - *Replace the string argument to* `path` *with the actual location*
   - *You may first need to install* `devtools`*:* `install.packages("devtools")`

### Notes
  - The source files in `cordial/data/` does *NOT* contain the `crispr_DT` data, as it exceeds the GitHub file size limit.
  - The `.tar.gz` in the assets of the latest [release](https://github.com/CutillasLab/cordial/releases/latest) *DOES* contain the data.
  - Due to the excessive size of included datasets, [Git Large File Storage (LFS)](https://git-lfs.com/) has been implemented. This causes known issues when installing packages directly from GitHub (`bad restore file magic number (file may be corrupted) -- no data loaded`); therefore, installations *must* use the Package Archive File (**cordial_x.x.x.tar.gz**) from the latest [release](https://github.com/CutillasLab/cordial/releases/latest).
  - If installation error occurs stating that certain required packages are missing, try manually installing the missing packages listed prior to reattempting to install `cordial`, e.g.:
```
devtools::install_cran(c("ggplot2", "ggrepel", "magrittr", "tidyr", "purrr", "future", "furrr", "collapse", "data.table"))
```
  - If error persists, it may also be required to install additional build tools specific to your operating system (OS) and R version:
    - **Windows:** [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)
    - **macOS:** [https://cran.r-project.org/bin/macosx/tools](https://cran.r-project.org/bin/macosx/tools)
    - **Linux:** [https://cran.r-project.org/bin/linux/](https://cran.r-project.org/bin/linux/)

## Quickstart
### Load
  - To load `cordial`:
    - `library(cordial)`
  - After loading the package, explore the help documentation by entering into the console the name of the package or function prefixed with a question mark, e.g.:
    - `?cordial`
    - `?cor_map`

### Functions
#### `cor_map()`
To get pairwise correlations for *all target (column) permutations*.

  - Parameters:
    - `dataset`: A wide-format [`data.table`](https://rdatatable.gitlab.io/data.table/).
    - `select_cols`: A `vector` of column names (`character`), or indices (`numeric`) to select; must omit non-numeric columns.
    - `filter_rows`: A named `list`. Values specify which rows to subset. Names correspond to column names in `dataset`, or `metadata` if supplied.
    - `metadata`: An optional `data.table` with values corresponding to rows matching in `dataset`.
    - `self`: A `character` string. `"yes"` includes self-correlations; `"no"` omits.
    - `method`: A `character` string. Correction method for p-value adjustment, passed to `stats::p.adjust()`.

#### `cor_target_map()`
To get all correlations for a *selection of key targets*.

  - Supply the key targets as a vector (e.g., `c("COLUMN1", "COLUMN2")`) to the additional `target` parameter.
  - Other parameters are shared with `cor_map()`.

#### `cor_target()`
To get all correlations for a *single key target*.

  - Supply a character string of the key target (e.g., `"COLUMN1"`) to the additional `target` parameter.
  - Other parameters are shared with `cor_map()`.
  
#### Notes
Correlations for *all* columns will be calculated, unless a subset is supplied to `select_cols`, i.e.:

  - *corr*(*X*, *Y*) = *corr*(*`select_cols`*, *`select_cols`*)
  
The additional `target` parameter (`cor_target_map()`, `cor_target()`) allows further subsetting of the pairwise correlations to make, i.e.:

  - *corr*(*X*, *Y*) = *corr*(*`target`*, *`select_cols`*)
