GenData <- function(N=200,theta){
  # N: number of samples, by default is 200
  # theta: successful rate

  # Example:
  # data <- GenData(theta=0.5)

  return(rbinom(N, 1, theta))
}


BayesDecision <- function(data,cutoff){
  # data: a length-N vector contains 0 and 1, which corresponds to whther the i-th patient was cured by the treatment.
  # cutoff: Stopping rule. If the posterior probability of theta>0.5 is over cutoff or below 1-cutoff

  # Example:
  # data <- GenData(theta=0.5)
  # Results <- BayesDecision(data,0.95)
  # NumPatietns <- Results$numPat
  # Decision <- Results$decision
  # Maximum number of patients
  N <- length(data)

  # prior distribution of theta: Beta(1,1)
  a <- 1
  b <- 1

  for (i in 1:N){ # suppose patients are treated one-by-one
    if (data[i] == 1){ # if the i-th is cured by the treatment
      # update the parameters a and b
      a <- a + 1
    } else { # if the i-th is NOT cured by the treatment
      # update the parameters a and b
      b <- b + 1
    }
    # probability of theta>1/2 based on the posterior distribution
    Ptheta <- 1 - pbeta(0.5, a, b)
    if (Ptheta>cutoff | Ptheta<1-cutoff){
      break;
    }
  }
  return(list(numPat=i, decision=(Ptheta>cutoff)))
}

# data <- GenData(theta=0.5)
# Results <- BayesDecision(data,0.95)
# NumPatietns <- Results$numPat
# Decision <- Results$decision
# Maximum number of patients

mcn <- 1000
for(cur_c in seq(0.95, 1.0, 0.001)){
  mc_decision <- NULL
  mc_numpat <- NULL
  for(cur_mcn in seq(1, mcn)) {
    data <- GenData(theta = 0.5)
    # function is two sided test, so need to use cutoff == 0.975 to get 5% overall
    results <- BayesDecision(data, cur_c)
    NumPatients <- results$numPat
    Decision <- results$decision
    mc_decision[cur_mcn] <- Decision
    mc_numpat[cur_mcn] <- NumPatients
  }

  if(mean(mc_decision) < 0.05) {
    print(paste0('Cutoff = ', cur_c, 'when mean(mc_decision) is ', mean(mc_decision)))s
    break
  }
}
print(mean(mc_numpat))

sim_cutoff <- cur_c
mcn <- 1000
mc_decision <- NULL
mc_numpat <- NULL
mc_mean_prob <- NULL
theta_list <- seq(0.1, 0.9, 0.05)
for(cur_theta_index in seq(1, length(theta_list))) {
  for(cur_mcn in seq(1, mcn)) {
    data <- GenData(theta = theta_list[cur_theta_index])
    # function is two sided test, so need to use cutoff == 0.975 to get 5% overall
    results <- BayesDecision(data, sim_cutoff)
    NumPatients <- results$numPat
    Decision <- results$decision
    mc_decision[cur_mcn] <- Decision
    mc_numpat[cur_mcn] <- NumPatients
  }
  mc_mean_prob[cur_theta_index] <- mean(mc_decision)
}

plot(theta_list, mc_mean_prob)



