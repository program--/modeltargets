#' @export
tar_recipe <- function(x, ...) {
    UseMethod("tar_recipe", x)
}

#' @export
tar_recipe.data.frame <- function(x, formula = NULL, ..., tar_args = NULL) {
    name <- paste0("make_recipe_", deparse(substitute(x)))

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

#' @export
tar_recipe.formula <- function(formula, data, ..., tar_args = NULL) {
    name <- paste0("make_recipe_", deparse(substitute(x)))

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

#' @export
tar_recipe.matrix <- function(x, ..., tar_args = NULL) {
    name <- paste0("make_recipe_", deparse(substitute(x)))

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