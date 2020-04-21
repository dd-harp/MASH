step_mainloop <- function(mainloop) {
  done <- rep(FALSE, 1:length(modules))
  names(done) <- names(modules)

  while (!all(done)) {
    inputs_available <- done %*% adjacency
    inputs_met <- inputs_available == inputs_required
    to_run <- inputs_met & !done
    data_cache <- run_modules(to_run, data_cache)

    outputs_needed <- adjacency %*% done
    remove_from_cache(data_cache, !outputs_needed)
  }
  mainloop
}
