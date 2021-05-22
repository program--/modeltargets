#' @title Augment Target
#' @param name Name of target
#' @param tar_args Arguments passed to `tar_target`.
#' @inheritParams parsnip::augment.model_fit
#' @export
tar_augment <- function(name, x, new_data, ..., tar_args = list()) {
    name <- .check_name(
        rlang::enexpr(name),
        paste0("tidymodels_augment_", deparse(substitute(x)))
    )

    x        <- rlang::enexpr(x)
    new_data <- rlang::enexpr(new_data)

    command <- substitute(
        augment(x = arg_obj, new_data = arg_data),
        env = list(
            arg_obj  = x,
            arg_data = new_data
        )
    )

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}