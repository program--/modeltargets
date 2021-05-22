#' @title Recipe Target
#' @aliases tar_recipe tar_recipe.default tar_recipe.formula
#' @param name Name of target
#' @param ... Additional functions that the recipe is passed to,
#'            i.e. recipe step functions.
#' @param tar_args Arguments passed to `tar_target`.
#' @inheritParams recipes::recipe
#' @return A `tar_target` object.
#' @examples
#' \dontrun{
#'     # Basic recipe
#'     tar_recipe(
#'         make_recipe,
#'         x = mtcars,
#'         formula = mpg ~ .,
#'         step_dummy(gear, carb),
#'         tar_args = list(format = "qs")
#'     )
#' }
#' @export
tar_recipe <- function(name, x, ...) {
    UseMethod("tar_recipe", x)
}

#' @rdname tar_recipe
#' @export
tar_recipe.default <- function(name, x, ...) {
    rlang::abort("`x` should be a data frame, matrix, or tibble")
}

#' @rdname tar_recipe
#' @export
tar_recipe.data.frame <- function(name, x, formula = NULL,
                                  ..., tar_args = list()) {
    name <- .check_name(
        rlang::enexpr(name),
        "tidymodels_recipe"
    )

    rec <- substitute(
        recipes::recipe(x = x, formula = fm),
        env = list(fm = formula)
    )

    command <- eval(substitute(
        .pipe_between(rec, ...),
        env = list(rec = rec)
    ))

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}

#' @rdname tar_recipe
#' @export
tar_recipe.formula <- function(name, formula, data, ..., tar_args = list()) {
    name <- .check_name(
        rlang::enexpr(name),
        "tidymodels_recipe"
    )

    rec <- substitute(
        recipes::recipe(formula = fm, data = data),
        env = list(fm = formula)
    )

    command <- eval(substitute(
        .pipe_between(rec, ...),
        env = list(rec = rec)
    ))

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}

#' @rdname tar_recipe
#' @export
tar_recipe.matrix <- function(name, x, ..., tar_args = list()) {
    name <- .check_name(
        rlang::enexpr(name),
        "tidymodels_recipe"
    )

    rec <- rlang::expr(recipes::recipe(x = x))

    command <- eval(substitute(
        .pipe_between(rec, ...),
        env = list(rec = rec)
    ))

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}