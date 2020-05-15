pull_term <- function(terms) {
  if (is.call(terms) && terms[[1]] == "+") {
    left <- terms[[2]]
    right <- terms[[3]]
    return(c(pull_term(left), pull_term(right)))
  } else if (is.call(terms) && terms[[1]] == "-") {
    left <- terms[[2]]
    # Move the minus sign to be a factor on the second term.
    # The second term is always by itself, while the first can be more terms.
    right <- call("*", -1, terms[[3]])
    return(c(pull_term(left), pull_term(right)))
  } else {
    return(terms)
  }
}


pull_factor <- function(terms) {
  if (class(terms) == "call" && terms[[1]] == "*") {
    left <- terms[[2]]
    right <- terms[[3]]
    return(c(pull_factor(left), pull_factor(right)))
  } else {
    return(list(terms))
  }
}


combine_factors <- function(factors) {
  if (length(factors) == 2) {
    return(call("*", factors[[1]], factors[[2]]))
  } else if (length(factors) > 2) {
    return(call("*", factors[[1]], factors[2:length(factors)]))
  } else {
    return(factors[[1]])
  }
}


term_to_argument <- function(factors, states) {
  for (fac_idx in 1:length(factors)) {
    for (state_idx in 1:length(states)) {
      if (factors[[fac_idx]] == states[[state_idx]]) {
        column_name <- factors[[fac_idx]]
        factors[[fac_idx]] <- NULL
        argument <- list(combine_factors(factors))
        names(argument) <- as.character(column_name)
        return(argument)
      }
    }
  }
  stop("Factor not found")
}


#' This takes a list of equations and sorts them into a standard form.
#'
#' @param equations This is a list of what R calls formulas.
#'
#' This assumes the equations are linear with coefficients that can
#' be any expression. The left-hand side of the equation, where the
#' derivative would go, should just be the variable itself.
#'
#' @examples
#' normalize_equations(list(
#'   U ~ -h * U + r * X,
#'   X ~ h * U - r * X
#' ))
#' @export
normalize_equations <- function(equations) {
  # Each equation is a call where the operator is 1 and the left-hand side is 2. rhs is 3.
  states <- lapply(equations, function(x) x[[2]])
  states_index <- 1:length(states)
  names(states_index) <- as.character(states)

  righthand <- vector(mode = "list", length = length(states_index))
  names(righthand) <- as.character(states)

  for (eq_idx in 1:length(equations)) {
    rhs <- equations[[eq_idx]][[3]]
    terms <- pull_term(rhs)
    args <- list()
    for (term_idx in 1:length(terms)) {
      factors <- pull_factor(terms[[term_idx]])
      # cat(paste(factors, "\n"))
      args <- c(args, term_to_argument(factors, states))
    }
    righthand[[eq_idx]] <- args
  }
  righthand
}


build_cme <- function(normed) {
  cme_matrix <- function(parameters) with(parameters, matrix(c(1), nrow = 1))
  list_action <- vector(mode = "list", length = length(normed) + 1)
  list_action[[1]] <- as.name("c")
  list_idx <- 2
  state_name <- names(normed)
  for (col in 1:length(normed)) {
    col_name <- state_name[col]
    for (row in 1:length(normed)) {
      if (col_name %in% names(normed[[row]])) {
        list_action[[list_idx]] <- normed[[row]][[col_name]]
      } else {
        list_action[[list_idx]] <- 0
      }
      list_idx <- list_idx + 1
    }
  }
  body(cme_matrix)[[3]][[2]] <- as.call(list_action)
  body(cme_matrix)[[3]][[3]] <- length(normed)
  cme_matrix
}


cme_maker <- function(equations) {
  normalized <- normalize_equations(equations)
  build_cme(normalized)
}


build_small_step <- function(normed) {
  cme_matrix <- function(parameters) with(parameters, matrix(c(1), nrow = 1))
  list_action <- vector(mode = "list", length = length(normed) + 1)
  list_action[[1]] <- as.name("c")
  list_idx <- 2
  state_name <- names(normed)
  for (col in 1:length(normed)) {
    col_name <- state_name[col]
    for (row in 1:length(normed)) {
      if (col_name %in% names(normed[[row]])) {
        if (row == col) {
          list_action[[list_idx]] <- call("+", call("*", normed[[row]][[col_name]], as.name("t")), 1)
        } else {
          list_action[[list_idx]] <- call("*", normed[[row]][[col_name]], as.name("t"))
        }
      } else {
        list_action[[list_idx]] <- ifelse(row == col, 1, 0)
      }
      list_idx <- list_idx + 1
    }
  }
  body(cme_matrix)[[3]][[2]] <- as.call(list_action)
  body(cme_matrix)[[3]][[3]] <- length(normed)
  cme_matrix
}


small_step_maker <- function(equations) {
  normalized <- normalize_equations(equations)
  build_small_step(normalized)
}


build_exact_exponential <- function(normed) {
  cme_matrix <- function(parameters) with(parameters, expm::expm(matrix(c(1), nrow = 1)))
  list_action <- vector(mode = "list", length = length(normed)^2 + 1)
  list_action[[1]] <- as.name("c")
  list_idx <- 2
  state_name <- names(normed)
  for (col in 1:length(normed)) {
    col_name <- state_name[col]
    for (row in 1:length(normed)) {
      if (col_name %in% names(normed[[row]])) {
        list_action[[list_idx]] <- call("*", normed[[row]][[col_name]], as.name("t"))
      } else {
        list_action[[list_idx]] <- 0
      }
      list_idx <- list_idx + 1
    }
  }
  body(cme_matrix)[[3]][[2]][[2]] <- as.call(list_action)
  body(cme_matrix)[[3]][[2]][[3]] <- length(normed)
  cme_matrix
}


exact_exponential <- function(equations) {
  normalized <- normalize_equations(equations)
  build_exact_exponential(normalized)
}



build_single_jump <- function(normed) {
  template_function <- function(parameters) {
    with(parameters, matrix(c(1), nrow = 1))
  }

  state_name <- names(normed)

  # Define variables at the top.
  assignments <- list()
  for (col in 1:length(normed)) {
    col_name <- state_name[col]
    # Check for a diagonal entry b/c that's the main rate.
    if (col_name %in% names(normed[[col]])) {
      col_rate <- paste0(col_name, "_rate")
      assignments[[col_rate]] <- bquote(
        .(n) <- .(v), list(n = as.name(col_rate), v = normed[[col]][[col_name]]))
      col_exp <- paste0(col_name, "_exp")
      assignments[[col_exp]] <- bquote(
        .(n) <- exp(.(v) * t), list(n = as.name(col_exp), v = as.name(col_rate)))
    }
  }

  list_action <- as.list(numeric(length(normed)^2 + 1))
  list_action[[1]] <- as.name("c")
  list_idx <- 2
  for (col in 1:length(normed)) {
    col_name <- state_name[col]
    # Check for a diagonal entry b/c that's the main rate.
    if (col_name %in% names(normed[[col]])) {
      for (row in 1:length(normed)) {
        if (row == col ) {
          survival_term <- as.name(paste0(col_name, "_exp"))
          if (list_action[[list_idx]] == 0) {
            list_action[[list_idx]] <- survival_term
          } else {
            list_action[[list_idx]] <- call("+", list_action[[list_idx]], survival_term)
          }
        } else if (col_name %in% names(normed[[row]])) {
          cumulative_incidence <- call(
            "*",
            call(
              "/",
              normed[[row]][[col]],
              as.name(paste0(col_name, "_rate"))
              ),
            call(
              "-",
              1,
              as.name(paste0(col_name, "_exp"))
              )
            )
          if (list_action[[list_idx]] == 0) {
            list_action[[list_idx]] <- cumulative_incidence
          } else {
            list_action[[list_idx]] <- call("+", list_action[[list_idx]], cumulative_incidence)
          }
        } else {
          list_action[[list_idx]] <- 0
        }
        list_idx <- list_idx + 1
      }
    }  # If there is no diagonal, the column must be zero.
  }
  with_clause <- body(template_function)[[2]]
  for (assign_idx in 1:length(assignments)) {
    body(template_function)[[1 + assign_idx]] <- assignments[[assign_idx]]
  }
  with_clause[[3]][[2]] <- as.call(list_action)
  with_clause[[3]][[3]] <- length(normed)
  body(template_function)[[length(assignments) + 2]] <- with_clause
  template_function
}


single_jump <- function(equations) {
  normalized <- normalize_equations(equations)
  build_single_jump(normalized)
}
