#Create log likelihood csv for the output of a stan model

#Block 4, 25 trials for all 205 subjects
require('ggplot2')
require('loo')

#load in stanfit (change this accordingly)
fit <- readRDS("nameOfFit.rds")
#extract log likelihood
log_lik <- extract_log_lik(fit, parameter_name = "log_lik_new", merge_chains = TRUE)
#change this to n_subj 1:40 is looking at model fit on just D2
n_subj <- c(1:205)
n_trials <- c(1:25)

#sum log likelihood across all trials for each subject for each sample
samples_sum <- data.frame(matrix(0, 4000, ncol = max(n_subj)))
m <- 0

for (i in n_subj){
  samples_sum[,i] <- as.data.frame(rowMeans(log_lik[,(m+1):(m+25)], na.rm = TRUE))
  m <- m+25
}

#average log likelihood across samples for each subject (get one value per subject)
subj_loglik <- as.data.frame(colMeans(samples_sum))
subj_loglik$sd <- 0

#get standard deviation of averaging across samples (related to width of posterior)
k <- 1
for (i in n_subj){
  subj_loglik[i,2] <- sd(samples_sum[,k])
  k <- k + 1
}

#put this all into a dataframe
names(subj_loglik) <- c("sum", "sd")
subj_loglik$subj <- c(1:(max(n_subj)))
subj_loglik$upper <- subj_loglik$sum + subj_loglik$sd
subj_loglik$lower <- subj_loglik$sum - subj_loglik$sd

#save
write.csv(subj_loglik, "NameOfFile.csv")
