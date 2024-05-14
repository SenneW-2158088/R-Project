# Opstellen van de 64x64 markov matrix
p <- matrix(0, nrow = 64, ncol = 64)


simulation <- function(start_state, end_state) {
  current_state <- start_state
  tries <- 0

  while (!all(current_state == end_state)) {
    tries <- tries + 1

    # calculate next state
    new_coin <- ifelse(runif(1) < 0.5, 0, 1)

    # Update state
    current_state <- c(current_state[-1], new_coin)
  }

  return(tries)
}

string_to_bits <- function(state) {

  chars <- strsplit(state, "")[[1]]
  bits <- integer(length(chars))
  for (i in seq_along(chars)) {
    if (chars[i] == "K") {
      bits[i] <- 0
    } else {
      bits[i] <- 1
    }
  }

  return(bits)
}

simulate <- function(n_simulations = 1) {
  start_state <- string_to_bits("MKKKKM")
  end_state <- string_to_bits("MMMMMM")

  tries <- numeric(n_simulations)

  for (sim in seq_len(n_simulations)) {
    tries[sim] <- simulation(start_state, end_state)
  }

  return(mean(tries))
}

print(simulate())

