# (b)
# - servers: 4
#   Service times:
#    - 1: Exponential: \lambda = 0.5 min^-1
#    - 2: Uniform:     a = 5 min, b = 10 min
#    - 3: Uniform:     a = 6 min, b = 12 min
#    - 4: Gamma:       \alpha = 7, \lambda = 2 min^-1
# - Average interarrival time of jobs: 2.5 min
# - Arrive independently of each other, according to a Poisson process
# - A server available? Jobs are equally likely to be processed by any of them
# - No server available? Job enters queue, leaves system after
#   6 min of waiting (if not started)
# - Server runs from 8:00 to 18:00 (10 hours)
# - Run at least 1000 Monte Carlo simultations and estimate:
# a: The expected waiting time for a randomly selected job;
# b: the expected response time;
# d: the expected length of a queue (excluding the jobs receiving service),
#    when a new job arrives;
# f: the probability that at least one server is available, when a job arrives;
# h: the expected number of jobs processed by each server;
# j: the expected number of jobs still remaining in the system at 6:03 pm;

k <- 4 # number of server
mu <- 2.5 # mean interrarrival time
service_time_params <- list(
  list("exponential", 0.5),  # Exponential, lambda = 0.5
  list("uniform", 5, 10),    # Uniform, a = 5, b = 10
  list("uniform", 6, 12),    # Uniform, a = 6, b = 12
  list("gamma", 7, 2)        # Gamma, alpha = 7, lambda = 2
)

# initialization

arrivals <- c() # arrival timestamp
starts <- c() # service starts
finishes <- c() # service finishes, departure times
selected_servers <- c() # assigned server
job <- 0 # the job number is initialized
arrival_time <- 0 # arrival time of a new job
server_available <- rep(0, k) # times when each server becomes available

calc_finish_time <- function(server_id) {
  param <- service_time_params[server_id]
  if (param[[1]] == "exponential") {
    return(rexp(1, rate = param[[2]]))
  } else if (param[[1]] == "uniform") {
    return(runif(1, min = param[[2]], max = param[[3]]))
  } else if (param[[1]] == "gamma") {
    finish_time <- sum(-1 / param[2] * log(runif(param[3])))
    return(finish_time)
  }
}

# while loop until arrival time reached 10 hours or 600 minutes
while (arrival_time < 600) { # until end of day
  job <- job + 1 # next job
  arrival_time <- arrival_time - mu * log(runif(1)) # arrival time of job j
  arrivals <- c(arrivals, arrival_time)
  # random assignment of new job j to a server
  # two cases: all servers are busy or not
  # number of free servers at time T
  n_free <- sum(server_available < arrival_time)
  selected_server <- 1 # the server that will take job j
  if (n_free) {
    for (v in (2:k)) {
      if (server_available[v] < server_available[selected_server]) {
        selected_server <- v
      }
    }
    # withdraw job if waiting time more than 6 min
    if ((server_available[selected_server] - arrival_time) > 6) {
      starts <- c(starts, 0)
      finishes <- c(finishes, arrival_time + 15)
    } else {
      starts <- c(starts, server_available[selected_server])
    }
  } else {
    selected_server <- sample(k, 1) # random server
    while (server_available[selected_server] > arrival_time) {
      selected_server <- sample(k, 1)
    }
    starts <- c(starts, arrival_time)
  }
  selected_servers <- c(selected_servers, selected_server)
  if (selected_server > 0) {
    finish_time <- calc_finish_time(selected_server)
    finishes <- c(finishes, starts[job] + finish_time) # time finished
    server_available[selected_server] <- starts[job] + finish_time
  }
}
