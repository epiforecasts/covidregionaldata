replacePublicR6Method <- function(r6Instance, fName, fun) {
  selfEnv <- environment(r6Instance[[fName]])$self
  properEnv <- environment(r6Instance[[fName]])
  unlockBinding(fName, selfEnv)
  selfEnv[[fName]] <- fun
  environment(selfEnv[[fName]]) <- properEnv
  lockBinding(fName, selfEnv)
}

dummy_class <- function(class) {
  class$download()
  class$data$raw <- purrr::map(class$data$raw, ~ dplyr::slice_tail(., n = 1000))
  class$clean()
  class$process()
  replacePublicR6Method(class, "download", function() {
    return(invisible(NULL))
  })
  class$verbose <- FALSE
  return(class)
}
