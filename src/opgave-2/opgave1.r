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
# exponential
exp_lambda <- c(0.5)
# uniform
uni_a <- c(5, 10)
uni_b <- c(10, 12)
# gamma
gamma_alpha <- 7
gamma_lambda <- 2

# initialization

arrival <- c() # arrival timestamp
start <- c() # service starts
finish <- c() # service finishes, departure times
server <- c() # assigned server
j <- 0 # the job number is initialized
T <- 0 # arrival tiem of a new job
A = rep(0, k) # times when each server becomes available

# while loop until arrival time reached 10 hours or 600 minutes

while (T < 600) { # until end of day
  j <- j + 1 # next job
  T <- T - mu * log(runif(1))
  arrival <- c(arrival, T)
}

