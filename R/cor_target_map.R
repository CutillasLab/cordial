cor_target_map <- function(
  dataset,
  target,
  select_cols = colnames(dataset),
  filter_rows = NULL,
  metadata = NULL,
  self = "yes",
  method = "BH",
  fun = cordial::cor_targets
) {

  # Pearson correlation analysis for a vector of targets (in parallel)
  # Calls `cor_targets()`

  # Load packages
  require(magrittr)
  require(purrr)
  require(future)
  require(furrr)
  require(collapse)
  require(data.table)

  # Argument error check
  if (!(is.character(target)) || (length(target) <= 1L)) {
    stop(
      '
      Error: `target` must be a "character" vector:
      ℹ `target` must be of type "character".
      ℹ `target` must be a vector of length > 1.
      ℹ Did you mean to supply a single target?
      ℹ Use `cor_target()` instead for a single target.
      ✖ `target` is not a "character" vector.
      ',
      call. = FALSE
    )
  }

  # Map in parallel
  future_cor <- future::future(
    {
      furrr::future_map(
        .x = target,
        .f = ~ fun(
          dataset = dataset,
          target = .x,
          select_cols = select_cols,
          filter_rows = filter_rows,
          metadata = metadata,
          self = self,
          method = method
        ),
        .options = furrr::furrr_options(globals = FALSE)
      )
    },
    packages = c("magrittr", "purrr", "furrr", "collapse", "data.table", "cordial"),
    globals = list(
      dataset = dataset,
      target = target,
      select_cols = select_cols,
      filter_rows = filter_rows,
      metadata = metadata,
      self = self,
      method = method,
      fun = fun
    )
  )
  value_cor <- future::value(future_cor)

  # Create single output data.table
  # (excluding 'try errors')
  result_DT <- Filter(
    function(x) data.table::is.data.table(x), value_cor
  ) %>%
    data.table::rbindlist()

  # Return output
  return(result_DT)
}
