# (b)
# - servers: 4
#   Service times:
#    - 1: Exponential: \lambda = 0.5 min^-1
#    - 2: Uniform:     a = 5 min, b = 10 min
#    - 3: Uniform:     a = 6 min, b = 12 min
#    - 4: Gamma:       \alpha = 7, \lambda = 2 min^-1
# - Average interval time: 2.5 min
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
