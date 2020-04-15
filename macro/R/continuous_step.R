initialize_times <- function(individuals, is_enabled, when) {
  when_col <- ncol(individuals) - length(when)
  times_end_col <- ncol(individuals)
  for (person_idx in 1:nrow(individuals)) {
    state <- as.list(individuals[person_idx])
    newly_enabled <- vapply(
      is_enabled,
      FUN = function(x) x(state),
      FUN.VALUE = vector(mode = "logical", length = 1))
    new_when <- vapply(
      when[newly_enabled],
      FUN = function(x) x(state),
      FUN.VALUE = vector(mode = "numeric", length = 1))
    individuals[person_idx, when_col, times_end_col] <- c(min(new_when), new_when)
  }
  individuals
}


continuous_step <- function(individuals, is_enabled, when, fire) {
  # The current time and next firing times are part of the next state of the system.
  # If a firing time is Inf, that means it isn't scheduled.
  current_time <- 0
  individuals <- initialize_times(individuals, is_enabled, when)

  step_cnt <- 5
  trajectory <- vector(mode = "list", length = step_cnt)
  for (step_idx in 1:step_cnt) {
    next_person <- as.list(individuals[order(when)][1])
    current_time <- next_person$when
    if (is.infinite(current_time)) break
    times <- next_person[4:length(next_person)]
    to_fire <- which.min(times)
    trajectory[[step_idx]] <- list(event = names(to_fire), when = current_time)
    individual <- fire[[to_fire]](individual, current_time)
    was_enabled <- is.finite(times)
    newly_enabled <- vapply(
      is_enabled,
      FUN = function(x) x(individual),
      FUN.VALUE = vector(mode = "logical", length = 1))
    times[was_enabled & !newly_enabled] <- Inf
    newly_enabled <- when[!was_enabled & newly_enabled]
    new_times <- current_time +
      vapply(
        newly_enabled,
        FUN = function(x) x(individual),
        FUN.VALUE = vector(mode = "numeric", length = 1))
    individuals[order(when)][1][3:ncol(individuals)] <- c(min(new_times), new_times)
  }
  do.call(rbind, trajectory)
}
