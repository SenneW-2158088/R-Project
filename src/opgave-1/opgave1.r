# Opstellen van de 64x64 markov matrix
p <- matrix(0, nrow = 64, ncol = 64)


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

print(p)

