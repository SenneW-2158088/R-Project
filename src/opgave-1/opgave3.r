simulation <- function(start_state, end_state) {
  current_state <- start_state
  tries <- 0

  while (!all(current_state == end_state)) {
    tries <- tries + 1

    # Current coin
    current_coin <- current_state[1]

    # calculate next state

    new_coin <- 1 # bas altijd 1 -> munt

    # als harry aan de beurt is random
    if (current_coin == 1) {
      # calculate next state
      new_coin <- ifelse(runif(1) < 0.5, 0, 1)
    }

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

simulate <- function(n_simulations = 10000) {
  start_state <- string_to_bits("MKKKKM")
  end_state <- string_to_bits("MMMMMM")

  tries <- numeric(n_simulations)

  for (sim in seq_len(n_simulations)) {
    tries[sim] <- simulation(start_state, end_state)
  }

  return(mean(tries))
}

print(simulate())
