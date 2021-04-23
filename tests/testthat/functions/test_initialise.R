# relies on a global test_class to be able available
test_initialise <- function(class) {
  test_class <- class
  test_fn <- function(class, level, totals, localise,
                      verbose, steps, regions, get, type) {
    class <- test_class$clone()
    class$verbose <- verbose
    class$steps <- steps
    class$totals <- totals
    class$localise <- localise
    if (!missing(regions)) {
      class$target_regions <- regions
    }
    class$get()
    return(class)
  }
  return(test_fn)
}
