# Globals

servers <- list(
  list("exponential", 0.5), # Exponential, lambda = 0.5
  list("uniform", 5, 10), # Uniform, a = 5, b = 10
  list("uniform", 6, 12), # Uniform, a = 6, b = 12
  list("gamma", 7, 2) # Gamma, alpha = 7, lambda = 2
)

arrival_rate <- 2.5
max_server_wait <- 6 * 60


# Get the process time for the given server
get_server_time <- function(server) {
  if (server[[1]] == "exponential") {
    return(rexp(1, rate = server[[2]]))
  } else if (server[[1]] == "uniform") {
    return(runif(1, min = server[[2]], max = server[[3]]))
  } else if (server[[1]] == "gamma") {
    finish_time <- sum(-1 / server[[2]] * log(runif(server[[3]])))
    return(finish_time)
  }
}


# Simulate a server queuing system
simulation <- function(simulation_time) {
  arrival_time <- 0

  server_times <- rep(0, length(servers))
  jobs <- list()

  # Performance metrics
  waiting_times <- c()

  # Run simulation
  while (arrival_time < simulation_time) {
    # New arrival of job
    # arrival_time <- arrival_time + rexp(1, rate = arrival_rate)
    arrival_time <- arrival_time - arrival_rate * log(runif(1))
    print(arrival_time)

    # Break if arrival later than simulation time
    if (arrival_time >= simulation_time) {
      break
    }


    ## Remove jobs time > 6 min
    # Add waiting times to the
    to_be_removed <- which(jobs > arrival_time + max_server_wait)
    for (removed in to_be_removed) {
      waiting_times <- c(waiting_times, max_server_wait)
    }

    jobs <- which(jobs < arrival_time + max_server_wait)

    ## Add job
    jobs <- c(jobs, arrival_time)

    ## Process jobs with available servers
    # Get available server count
    available_servers <- which(server_times < arrival_time)
    available_servers <- sample(available_servers) # Shuffle servers



    for (server_id in available_servers) {
      # get server for job
      selected_server <- servers[[server_id]] # Get server from index

      if (length(jobs) > 0) {
        # process a job
        job <- jobs[[1]]
        jobs <- jobs[-1] # remove job from queue

        # waiting time
        waiting_time <- server_times[[server_id]] - job
        waiting_times <- c(waiting_times, waiting_time)

        # calculate service time
        service_time <- get_server_time(selected_server)
        server_times[server_id] <- arrival_time + service_time
      } # End if
    } # End for
  } # End while

  return(list(
    average_waiting_times = mean(waiting_times)
  ))
}

s <- simulate(600.0)
