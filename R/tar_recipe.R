tar_recipe <- function(x, ...) {
    UseMethod("tar_recipe", x)
}

tar_recipe.data.frame <- function(x, formula = NULL, ..., tar_args = NULL) {
    name <- paste0("make_recipe_", deparse(substitute(x)))

    rec <- substitute(
        recipes::recipe(x = df, formula = fm),
        env = list(df = x, fm = formula)
    )

    command <- .pipe_between(rec, ...)

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args)
    )
}

tar_recipe.formula <- function(formula, data, ..., tar_args = NULL) {
    name <- paste0("make_recipe_", deparse(substitute(x)))

    rec <- substitute(
        recipes::recipe(formula = fm, data = dt),
        env = list(fm = formula, dt = data)
    )

    command <- .pipe_between(rec, ...)

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args)
    )
}

tar_recipe.matrix <- function(x, ..., tar_args = NULL) {
    name <- paste0("make_recipe_", deparse(substitute(x)))

    rec <- substitute(
        recipes::recipe(x = mat),
        env = list(mat = x)
    )

    command <- .pipe_between(rec, ...)

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args)
    )
}