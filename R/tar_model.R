#' @export
tar_model <- function(model, ..., tar_args = NULL) {
    model <- substitute(model)
    name  <- paste0("make_model_", deparse(substitute(model)))

    parsnip_model <- substitute(
        parsnip::model(),
        env = list(model = model)
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