#' @title Predict Target
#' @param name Name of target
#' @param tar_args Arguments passed to `tar_target`.
#' @inheritParams parsnip::predict.model_fit
#' @inheritDotParams parsnip::predict.model_fit
#' @export
tar_predict <- function(
    name,
    object,
    new_data,
    type = NULL,
    opts = list(),
    ...,
    tar_args = list()
) {
    name <- .check_name(
        rlang::enexpr(name),
        paste0("tidymodels_predict_", deparse(substitute(object)))
    )

    object   <- rlang::enexpr(object)
    new_data <- rlang::enexpr(new_data)

    command <- substitute(
        predict(object = arg_obj, new_data = arg_data,
                type = arg_type, opts = arg_opts),
        env = list(
            arg_obj = object,
            arg_data = new_data,
            arg_type = type,
            arg_opts = opts
        )
    )

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}