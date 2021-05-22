#' @title Recipe Target
#' @param name Name of target
#' @param ... Functions piped to from `workflows::workflow()`
#'            such as `workflows::add_model()`. See example.
#' @param tar_args Arguments passed to `tar_target`.
#' @return A `tar_target` object.
#' @examples
#' \dontrun{
#' list(
#'     # `x` is some template data frame
#'     tar_recipe(
#'         make_recipe,
#'         V1 ~ V2 + V3,
#'         data = x
#'     ),
#'     # Creating a model
#'     tar_model(
#'         make_model_rf,
#'         rand_forest,
#'         set_mode("regression"),
#'         set_engine("ranger"),
#'         tar_args = list(format = "qs")
#'     ),
#'     # Creating the workflow
#'     tar_workflow(
#'         make_workflow_rf,
#'         workflows::add_model(make_model_rf),
#'         workflows::add_recipe(make_recipe)
#'     )
#' )
#' }
#' @export
tar_workflow <- function(name, ..., tar_args = list()) {
    name <- .check_name(
        rlang::enexpr(name),
        "tidymodels_workflow"
    )

    command <- eval(substitute(
        .pipe_between(arg_wkflow, ...),
        env = list(arg_wkflow = quote(workflows::workflow()))
    ))

    do.call(
        targets::tar_target_raw,
        c(list(name, command),
          tar_args),
        quote = TRUE
    )
}