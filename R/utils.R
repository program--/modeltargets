#' @title Quote a chain of (abritrary number of) pipes.
#' @param x Starting function for the pipe chain.
#' @param ... Subsequent functions appended to the pipe chain.
#' @return A quoted pipe chain starting with `x` to the last element of `...`,
#'         or `x` itself if `...` is empty.
#' @examples
#' \dontrun{
#' # Creating pipe chain
#' .pipe_between(2, runif())
#' #> 2 %>% runif()
#'
#' # Evaluating pipe chain later on
#' eval(.pipe_between(2, runif()))
#' #> [1] 0.5180003 0.5353272
#'
#' # Longer pipe chain
#' long_chain <- .pipe_between(2, runif(), sum(), log())
#' long_chain
#' #> 2 %>% runif() %>% sum() %>% log()
#'
#' eval(long_chain)
#' #> [1] -0.4767814
#' }
#'
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#' @export
.pipe_between <- function(x, ...) {
    x     <- rlang::enexpr(x)
    funcs <- rlang::enexprs(...)

    for (f in funcs) {
        x <- substitute(
            lhs %>% rhs,
            list(lhs = x, rhs = f)
        )
    }

    x
}