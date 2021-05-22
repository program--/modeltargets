#' @title Fitting Target
#' @param name Name of target
#' @param tar_args Arguments passed to `tar_target`.
#' @inheritParams parsnip::fit.model_spec
#' @return A `tar_target` object.
#' @export
tar_fit <- function(name, object, formula, data,
                    control = parsnip::control_parsnip(),
                    ...,
                    tar_args = list()) {

    if (missing(formula)) {
        formula <- NULL
    } else {
        formula <- rlang::enexpr(formula)
    }

    name <- .check_name(
        rlang::enexpr(name),
        paste0("tidymodels_fit_", deparse(substitute(object)))
    )

    object <- rlang::enexpr(object)
    data   <- rlang::enexpr(data)

    if (is.null(formula)) {
        command <- substitute(
            parsnip::fit(object = obj, data = arg_data, control = ctrl),
            env = list(
                obj = object,
                arg_data = data,
                ctrl = control
            )
        )
    } else {
        command <- substitute(
            parsnip::fit(object = arg_obj, formula = arg_form,
                         data = arg_data, control = arg_ctrl),
            env = list(
                arg_obj = object,
                arg_form = formula,
                arg_data = data,
                arg_ctrl = control
            )
        )
    }

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}

#' @title Fitting XY Target
#' @inheritParams parsnip::fit_xy.model_spec
#' @inheritParams tar_fit
#' @return A `tar_target` object.
#' @export
tar_fit_xy <- function(object, x, y,
                       control = parsnip::control_parsnip(),
                       ...,
                       tar_args = list()) {
    object <- rlang::enexpr(object)
    x      <- rlang::enexpr(x)
    y      <- rlang::enexpr(y)
    name   <- paste0("tidymodels_fit_xy_", deparse(substitute(object)))

    command <- eval(substitute(
        fit_xy(object = obj, x = arg_x, y = arg_y, control = ctrl),
        env = list(
            obj = object,
            arg_x = x,
            arg_y = y,
            ctrl = control
        )
    ))

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}