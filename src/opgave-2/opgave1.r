# Globals
servers <- list(
  list("exponential", 0.5), # Exponential, lambda = 0.5
  list("uniform", 5, 10), # Uniform, a = 5, b = 10
  list("uniform", 6, 12), # Uniform, a = 6, b = 12
  list("gamma", 7, 2) # Gamma, alpha = 7, lambda = 2
)

arrival_rate <- 1 / 2.5 # 2.5 minutes on average between arrivals
max_server_wait <- 6 # 6 minutes max wait time in queue

# Get the process time for the given server
get_server_time <- function(server) {
  if (server[[1]] == "exponential") {
    return(rexp(1, rate = server[[2]]))
  } else if (server[[1]] == "uniform") {
    return(runif(1, min = server[[2]], max = server[[3]]))
  } else if (server[[1]] == "gamma") {
    return(rgamma(1, shape = server[[2]], rate = server[[3]]))
  }
}

# Simulate a server queuing system
simulation <- function(simulation_time) {
  arrival_time <- 0

  server_times <- rep(0, length(servers))
  job_queue <- c()

  # Performance metrics
  waiting_times <- c()
  response_times <- c()
  queue_lengths <- c()
  available_server_counts <- c()
  jobs_processed <- rep(0, length(servers))
  final_queue_length <- 0
  final_system_length <- 0

  # Run simulation
  while (arrival_time < simulation_time) {
    # New arrival of job
    arrival_time <- arrival_time + rexp(1, rate = arrival_rate)

    # Break if arrival later than simulation time
    if (arrival_time >= simulation_time) {
      break
    }

    # Record queue length and availability of servers when a new job arrives
    queue_lengths <- c(queue_lengths, length(job_queue))
    available_servers <- which(server_times <= arrival_time)
    available_servers <- sample(available_servers)

    available_server_counts <- c(available_server_counts, length(available_servers))

    # Remove jobs that waited too long and record their waiting times
    waiting_too_long <- which((arrival_time - job_queue) > max_server_wait)
    if (length(waiting_too_long) > 0) {
      waiting_times <- c(waiting_times, rep(max_server_wait, length(waiting_too_long)))
      job_queue <- job_queue[-waiting_too_long]
    }

    # Process jobs with available servers
    for (server_id in available_servers) {
      if (length(job_queue) > 0) {
        # get server for job
        selected_server <- servers[[server_id]] # Get server from index

        # process a job
        job <- job_queue[1]
        job_queue <- job_queue[-1] # remove job from queue

        # waiting time
        waiting_time <- arrival_time - job
        waiting_times <- c(waiting_times, waiting_time)

        # calculate service time
        service_time <- get_server_time(selected_server)
        response_time <- waiting_time + service_time
        response_times <- c(response_times, response_time)

        server_times[server_id] <- arrival_time + service_time
        jobs_processed[server_id] <- jobs_processed[server_id] + 1
      }
    }

    # Add new job to queue
    job_queue <- c(job_queue, arrival_time)
  }

  # Capture final queue length and jobs still in the system at the end of the simulation time
  final_queue_length <- length(job_queue)
  final_system_length <- sum(server_times > simulation_time) + final_queue_length

  list(
    avg_waiting_time = mean(waiting_times),
    avg_response_time = mean(response_times),
    avg_queue_length = mean(queue_lengths),
    prob_server_available = mean(available_server_counts > 0),
    jobs_processed = jobs_processed,
    final_queue_length = final_queue_length,
    final_system_length = final_system_length
  )
}

# Run simulations
num_simulations <- 1000
simulation_time <- 10 * 60 # 10 hours in minutes
results <- replicate(num_simulations, simulation(simulation_time), simplify = FALSE)

# Calculate metrics
avg_waiting_time <- mean(sapply(results, function(res) res$avg_waiting_time))
avg_response_time <- mean(sapply(results, function(res) res$avg_response_time))
avg_queue_length <- mean(sapply(results, function(res) res$avg_queue_length))
prob_server_available <- mean(sapply(results, function(res) res$prob_server_available))
avg_jobs_processed <- colMeans(do.call(rbind, lapply(results, function(res) res$jobs_processed)))
final_queue_length <- mean(sapply(results, function(res) res$final_queue_length))
final_system_length <- mean(sapply(results, function(res) res$final_system_length))

# Output results
cat("Expected waiting time:", avg_waiting_time, "\n")
cat("Expected response time:", avg_response_time, "\n")
cat("Expected queue length when a new job arrives:", avg_queue_length, "\n")
cat("Probability that at least one server is available when a job arrives:", prob_server_available, "\n")
cat("Expected number of jobs processed by each server:", avg_jobs_processed, "\n")
cat("Expected number of jobs still remaining in the system at 18:03:", final_system_length, "\n")
