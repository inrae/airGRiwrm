CreateSupervisor <- function(griwrm) {
  # Create Supervisor environment in the parent of GlobalEnv
  e <- new.env(parent = parent.env(globalenv()))

  # Hidden variable to detect which environment it is
  e$.isSupervisor <- "3FJKmDcJ4snDbVBg"

  # Add pointer to itself in order to assign variable from function environment
  e$supervisor <- e

  e$griwrm <- griwrm

  # Controller list
  e$Controllers <- list()
  class(e$Controllers) <- c("Controllers", class(e$Controllers))

  # Copy functions
  e$createController <- createController
  environment(e$createController) <- e
  return(e)
}
