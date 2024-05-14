# Opstellen van de 64x64 markov matrix
p_size <- 10
p <- matrix(0, nrow = 64, ncol = 64)

bits_as_state <- function(bits) {
  s <- ""
  for (bit in bits) {
    if (bit == 0) {
      s <- paste0(s, "K")
    } else {
      s <- paste0(s, "M")
    }
  }

  return(s)
}

index_to_string <- function(index) {
  bin_state <- as.integer(intToBits(index)[1:6])
  return(bits_as_state(rev(bin_state)))
}

states <- sapply(0:63, index_to_string)

# state van munten
for (i in 0:63) {
  bin_state <- as.integer(intToBits(i)[1:6])
  leftmost <- bin_state[1]
  new_base <- bitwShiftL(i, 1) %% 64

  if (leftmost == 0) {
    p[i + 1, new_base + 1] <- 0.5
    p[i + 1, new_base + 2] <- 0.5
  } else {
    p[i + 1, new_base + 1] <- 0.5
    p[i + 1, new_base + 2] <- 0.5
  }
}

colnames(p) <- states
rownames(p) <- states

print(p)
