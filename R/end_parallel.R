#' Create a sequential future
#'
#' Begins a sequential \code{\link[future:plan]{future::plan}}, replacing
#' any previously set \code{\link[future]{plan}}.
#'
#' \code{\link{end_parallel}} creates a sequential \code{\link[future]{plan}}
#' for synchronous processing in the current \code{R} session.
#'
#' Run \code{\link{end_parallel}} at the end of working with \code{cordial}
#' functions to return to sequential processing. Alternatively, users may set
#' their own \code{\link[future]{plan}} explicitly for more control.
#'
#' @return A \code{\link[future:sequential]{SequentialFuture}}.
#'
#' @seealso \itemize{
#' \item \code{\link{start_parallel}} for parallel processing.
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
#' # Begin sequential processing
#'
#' end_parallel()
#' }
end_parallel <- function() {
  # Load package
  require(future)

  # End parallel future plan
  future::plan(sequential)

  # Check number of workers
  print(paste0('End parallel multisession - workers: ', future::nbrOfWorkers()))
}
