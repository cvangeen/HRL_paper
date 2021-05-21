#Moment-matching function to identify best group level-priors for "EmpPrior_Block4.stan"

#starting pointL pick whatever actual numbers
params <- c(0.1, 0.5)

#create dataframe of alpha_pos
subject <- c(1:165)
alpha_pos <- read.csv("~/Documents/GitHub/hierarchicalRL/Models_stan/M3/4EmpPriors/v3_alpha_pos_m3.csv")
alpha_pos <- alpha_pos[1:165,]
alpha_pos <- as.data.frame(alpha_pos[,-1])

#create dataframe of alpha_neg
subject <- c(1:165)
alpha_neg <- read.csv("~/Documents/GitHub/hierarchicalRL/Models_stan/M3/4EmpPriors/v3_alpha_neg_m3.csv")
alpha_neg <- alpha_neg[1:165,]
alpha_neg <- as.data.frame(alpha_neg[,-1])

#function for alphas (beta distributed) - insert alpha_pos or alpha_neg
moment_matching_function <- function(params, alpha){
  
  #paramters of distribution
  params[1]
  params[2]
  subject <- c(1:165)
  log_lik <- data.frame(matrix(0, max(subject), 1))
  
  k <- 1
  for (s in subject) {
    log_lik[k,1] <- dbeta(alpha[s,], params[1], params[2], log = TRUE)
    k <- k + 1
  }

  lik_sum <-(-sum(log_lik))
  return(lik_sum)
}

#optimize parameters
optim_alpha_pos <- optim(par = params, fn = moment_matching_function, alpha = alpha_pos)
optim_alpha_neg <- optim(par = params, fn = moment_matching_function, alpha = alpha_neg)

###for betas

#starting point: pick whatever
params <- c(0.1, 0.5)

#create dataframe for beta0
subject <- c(1:165)
beta_0 <- read.csv("~/Documents/GitHub/hierarchicalRL/Models_stan/M3/4EmpPriors/v3_beta0_m3.csv")
beta_0 <- beta_0[1:165,]
beta_0 <- as.data.frame(beta_0[,-1])

#create dataframe for beta1
subject <- c(1:165)
beta_1 <- read.csv("~/Documents/GitHub/hierarchicalRL/Models_stan/M3/4EmpPriors/v3_beta1_m3.csv")
beta_1 <- beta_1[1:165,]
beta_1 <- as.data.frame(beta_1[,-1])

#create dataframe for beta2
subject <- c(1:165)
beta_2 <- read.csv("~/Documents/GitHub/hierarchicalRL/Models_stan/M3/4EmpPriors/v3_beta2_m3.csv")
beta_2 <- beta_2[1:165,]
beta_2 <- as.data.frame(beta_2[,-1])

#function for betas (normally distributed) - insert whichever beta (0, 1 or 2)
moment_matching_function_2 <- function(params, beta){
  
  #paramters of distribution
  params[1]
  params[2]
  subject <- c(1:165)
  log_lik <- data.frame(matrix(0, max(subject), 1))
  
  k <- 1
  for (s in subject) {
    log_lik[k,1] <- dnorm(beta[s,], params[1], params[2], log = TRUE)
    k <- k + 1
  }
  
  lik_sum <-(-sum(log_lik))
  return(lik_sum)
}

#optimize parameters
optim_beta_0 <- optim(par = params, fn = moment_matching_function_2, beta = beta_0)
optim_beta_1 <- optim(par = params, fn = moment_matching_function_2, beta = beta_1)
optim_beta_2 <- optim(par = params, fn = moment_matching_function_2, beta = beta_2)
