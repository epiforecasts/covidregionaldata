replacePublicR6Method <- function(r6Instance, fName, fun) {
  selfEnv <- environment(r6Instance[[fName]])$self
  properEnv <- environment(r6Instance[[fName]])
  unlockBinding(fName, selfEnv)
  selfEnv[[fName]] <- fun
  environment(selfEnv[[fName]]) <- properEnv
  lockBinding(fName, selfEnv)
}
