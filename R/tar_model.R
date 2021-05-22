#' @title Model Target
#' @param name Name of target
#' @param model Parsnip model function.
#' @param ... Additional functions that `model` is passed to.
#' @param tar_args Arguments passed to `tar_target`.
#' @return A `tar_target` object.
#' @examples
#' \dontrun{
#'     # Create a ranger model that is saved to a .qs
#'     tar_model(
#'         make_model_rf,
#'         rand_forest,
#'         set_mode("regression"),
#'         set_engine("ranger"),
#'         tar_args = list(format = "qs")
#'     )
#'
#'     # Create a nnet neural network model
#'     tar_model(
#'         make_model_mlp,
#'         mlp,
#'         set_args(hidden_units = 5),
#'         set_engine("nnet"),
#'         set_mode("regression")
#'     )
#' }
#' @export
tar_model <- function(name, model, ..., tar_args = list()) {
    name <- .check_name(
        rlang::enexpr(name),
        "make_workflow"
    )

    model <- substitute(model)
    name  <- paste0("make_model_", deparse(substitute(model)))

    parsnip_model <- substitute(
        `::`(parsnip, arg_model)(),
        env = list(arg_model = model)
    )

    command <- eval(substitute(
        .pipe_between(model, ...),
        env = list(model = parsnip_model)
    ))

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}