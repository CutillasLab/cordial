#' Start a parallel multisession future
#'
#' Begins a multisession \code{\link[future:plan]{future::plan}} for
#' parallel processing, replacing any previously set
#' \code{\link[future]{plan}}.
#'
#' \code{\link{start_parallel}} creates a multisession
#' \code{\link[future]{plan}} for asynchronous (parallel) processing in
#' separate \code{R} sessions running in the background of the same machine.
#'
#' If a multisession \code{\link[future]{plan}} is set,
#' \code{\link{cor_target}} and \code{\link{cor_target_map}} execute in
#' parallel; the result is returned to the main \code{R} session.
#'
#' Run \code{\link{start_parallel}} at the beginning of working with
#' \code{cordial} functions. Alternatively, users may set their own
#' \code{\link[future]{plan}} for more control. To return to sequential
#' processing, see \code{\link{end_parallel}}.
#'
#' @param logical_cores A logical scalar. The number of parallel \code{R}
#' sessions is set to the number of physical CPUs/cores if \code{FALSE}
#' (default), or logical CPUs/cores if \code{TRUE}.
#'
#' @return A \code{\link[future:multisession]{MultisessionFuture}}.
#'
#' @seealso \itemize{
#' \item \code{\link{end_parallel}} for sequential processing.
#' \item \code{\link{cor_target}} for correlation analysis of a single target
#' in parallel.
#' \item \code{\link{cor_target_map}} for correlation analysis of multiple
#' targets in parallel.
#' \item \code{\link[future:plan]{future::plan}} for setting the future plan
#' directly.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create parallel multisessions equal to the number of physical cores
#'
#' start_parallel()       # Default
#' start_parallel(FALSE)  # Same as above
#'
#'
#' # Create parallel multisessions equal to the number of logical cores
#'
#' start_parallel(TRUE)
#' }
start_parallel <- function(logical_cores = FALSE) {
  # Load package
  require(future)

  # Create parallel future plan set to the number of physical cores
  future::plan(multisession, workers = future::availableCores(logical = logical_cores))

  # Check number of workers
  print(paste0('Start parallel multisession - workers: ', future::nbrOfWorkers()))
}
