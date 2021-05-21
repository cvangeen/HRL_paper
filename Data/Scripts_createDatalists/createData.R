##create datalist for hierarchical project with data from 2015 Gershman paper

#First part: create datalist for joint D1 + D2
#load D1 (contains 4 studies, 166 participants in all)
GershmanData_d1 <- read.csv("~/Documents/Github/hierarchicalRL/Data/Behavioral_Data/GershmanData_d1.csv", stringsAsFactors = FALSE)

#remove random column that gets created
GershmanData_d1$X.1 <- NULL

#remove subject that only has 75 trials
removeSubj <- GershmanData_d1$name[which(GershmanData_d1$N == 75)]
index <- which(GershmanData_d1$name == removeSubj)
GershmanData_d1_cleaned <- GershmanData_d1[-c(index:(index+75)),]

#remove extra headers
index2 <- which(GershmanData_d1_cleaned$name == "name")
GershmanData_d1_cleaned <- GershmanData_d1_cleaned[-c(index2),]

#turn name column into something sensible(ish)
subj <- c(1:165)
k <- 1
for (i in subj){
  GershmanData_d1_cleaned$name[k:(k+(100-1))] <- i
  k <- 100 + k
}

#add trial number column 
i <- 1
GershmanData_d1_cleaned$trial_number[which(GershmanData_d1_cleaned$name == i)] <- c(1:100)

##load D2 (1 study with 4 blocks, 40 participants)

#load test block to get trial number up to 100
GershmanData_d2_b1 <- read.csv("~/Documents/Github/hierarchicalRL/Data/Behavioral_Data/GershmanData_d2_block1_test.csv", stringsAsFactors = FALSE)
GershmanData_d2_b2 <- read.csv("~/Documents/Github/hierarchicalRL/Data/Behavioral_Data/GershmanData_d2_block2_test.csv", stringsAsFactors = FALSE)
GershmanData_d2_b3 <- read.csv("~/Documents/Github/hierarchicalRL/Data/Behavioral_Data/GershmanData_d2_block3_test.csv", stringsAsFactors = FALSE)
GershmanData_d2_b4 <- read.csv("~/Documents/Github/hierarchicalRL/Data/Behavioral_Data/GershmanData_d2_block4_test.csv", stringsAsFactors = FALSE)

#remove random column that gets created
GershmanData_d2_b1$X.1 <- NULL
GershmanData_d2_b2$X.1 <- NULL
GershmanData_d2_b3$X.1 <- NULL
GershmanData_d2_b4$X.1 <- NULL

#remove extra headers
index <- which(GershmanData_d2_b1$N == "N")
GershmanData_d2_b1 <- GershmanData_d2_b1[-c(index),]

index <- which(GershmanData_d2_b2$N == "N")
GershmanData_d2_b2 <- GershmanData_d2_b2[-c(index),]

index <- which(GershmanData_d2_b3$N == "N")
GershmanData_d2_b3 <- GershmanData_d2_b3[-c(index),]

index <- which(GershmanData_d2_b4$N == "N")
GershmanData_d2_b4 <- GershmanData_d2_b4[-c(index),]

#create a name column for subject ID
#block 1
GershmanData_d2_b1$name <- 0
subj <- c(1:40)
k <- 1
for (i in subj){
  GershmanData_d2_b1$name[k:(k+(25-1))] <- i
  k <- 25 + k
}

#block 2
GershmanData_d2_b2$name <- 0
k <- 1
for (i in subj){
  GershmanData_d2_b2$name[k:(k+(25-1))] <- i
  k <- 25 + k
}

#block 3
GershmanData_d2_b3$name <- 0
k <- 1
for (i in subj){
  GershmanData_d2_b3$name[k:(k+(25-1))] <- i
  k <- 25 + k
}

#block 4
GershmanData_d2_b4$name <- 0
k <- 1
for (i in subj){
  GershmanData_d2_b4$name[k:(k+(25-1))] <- i
  k <- 25 + k
}

#add trial number column in to each dataframe
i <- 1
GershmanData_d2_b1$trial_number[which(GershmanData_d2_b1$name == i)] <- c(1:25)

i <- 1
GershmanData_d2_b2$trial_number[which(GershmanData_d2_b2$name == i)] <- c(26:50)

i <- 1
GershmanData_d2_b3$trial_number[which(GershmanData_d2_b3$name == i)] <- c(51:75)

i <- 1
GershmanData_d2_b4$trial_number[which(GershmanData_d2_b4$name == i)] <- c(76:100)

#combine 4 blocks into one
GershmanData_d2 <- rbind(GershmanData_d2_b1, GershmanData_d2_b2, GershmanData_d2_b3, GershmanData_d2_b4)
GershmanData_d2 <- GershmanData_d2[order(GershmanData_d2$name, GershmanData_d2$trial_number),]

#prepare data to be merged with D1
#add number of subjects from D1 to name column so that there is no overlap
GershmanData_d2$name <- GershmanData_d2$name+165

#make all columns match
GershmanData_d2$game <- GershmanData_d2$block
GershmanData_d2$block <- NULL
GershmanData_d2$C <- NULL

#merge D1 and D2
GershmanData_all <- rbind(GershmanData_d2, GershmanData_d1_cleaned)
GershmanData_all$name <- as.numeric(GershmanData_all$name)

#order by subject number
#1-165 come from D1, 166 - 205 come from D2
GershmanData_all <- GershmanData_all[order(GershmanData_all$name),]

#column for stickiness
GershmanData_all$stick <- lag(GershmanData_all$c, n = 1L, default = NA, order_by = NULL) == 1
GershmanData_all$stick[which(GershmanData_all$stick == "TRUE")] <- 0.5
GershmanData_all$stick[which(GershmanData_all$stick == "0")] <- -0.5

##create datalist for stan model

#get variables
subj_Gershman <- c(unique(GershmanData_all$name))
subj_Gershman <- as.numeric(subj_Gershman)
trials_Gershman <- c(unique(GershmanData_all$trial_number))

#trials/subj matrix
trialNum_Gershman <- array(length(trials_Gershman),dim=c(length(subj_Gershman)))

#reward matrix
reward_Gershman <- matrix(0,nrow = max(trials_Gershman),length(subj_Gershman))
GershmanData_all$name <- as.numeric(GershmanData_all$name)
GershmanData_all$r <- as.numeric(GershmanData_all$r)
k <- 1
for (i in subj_Gershman) {
  reward_Gershman[,k] <- GershmanData_all$r[GershmanData_all$name==i]
  k <- k+1
}
reward_Gershman <- t(reward_Gershman)

#state matrix
state_Gershman <- matrix(0,nrow = max(trials_Gershman),length(subj_Gershman))
GershmanData_all$game <- as.numeric(GershmanData_all$game)
k <- 1
for (i in subj_Gershman) {
  state_Gershman[,k] <- GershmanData_all$game[GershmanData_all$name==i]
  k <- k+1
}
state_Gershman <- t(state_Gershman)

#choice matrix
#choice matrix with 1 and 2 (1 is left, 2 is right)
choice_Gershman <- matrix(0,nrow = max(trials_Gershman),ncol = length(subj_Gershman))
k <- 1
GershmanData_all$c <- as.numeric(GershmanData_all$c )
for (i in subj_Gershman) {
  choice_Gershman[,k] <- GershmanData_all$c[GershmanData_all$name==i]
  k <- k + 1
}
choice_Gershman <- t(choice_Gershman)

#choice_two matrix (1 is left, right is 0)
choice_two_Gershman <- choice_Gershman
choice_two_Gershman[choice_two_Gershman == 2] <- 0

#stickiness matrix
stickiness_Gershman <- matrix(0,nrow = max(trials_Gershman),ncol = length(subj_Gershman))
k <- 1
GershmanData_all$stick <- as.numeric(GershmanData_all$stick)
for (i in subj_Gershman) {
  stickiness_Gershman[,k] <- GershmanData_all$stick[GershmanData_all$name==i]
  k <- k + 1
}
stickiness_Gershman <- t(stickiness_Gershman)

#make stickiness for all the first trials for each subject be 0 (since stickiness is impossible for trial 1) 
for (i in c(1:length(subj_Gershman))) {
  stickiness_Gershman[i,1] <- 0
}

#create datalist with sitckiness for all subjects (D1 + D2)
v3_m3_datalist_Gershman <- list("NS" = 205, "MT" = 100, "NStim" = 4, "NC" = 2, "NT" = trialNum_Gershman,"K" = 3, 
                             "stim" = state_Gershman, "rew" = reward_Gershman, "choice" = choice_Gershman, 
                             "choice_two" = choice_two_Gershman, "stickiness" = stickiness_Gershman)

#create datalist without stickiness for all subjects (D1 + D2)
v3_datalist_Gershman <- list("NS" = 205, "MT" = 100, "NStim" = 4, "NC" = 2, "NT" = trialNum_Gershman,"K" = 3, 
                                "stim" = state_Gershman, "rew" = reward_Gershman, "choice" = choice_Gershman, 
                                "choice_two" = choice_two_Gershman)



##Part two: DO THIS if creating datalist for just D2
#create column for stickiness 
GershmanData_d2$stick <- lag(GershmanData_d2$c, n = 1L, default = NA, order_by = NULL) == 1
GershmanData_d2$stick[which(GershmanData_d2$stick == "TRUE")] <- 0.5
GershmanData_d2$stick[which(GershmanData_d2$stick == "0")] <- -0.5


##create datalist for just D2
subj_Gershman_d2 <- c(unique(GershmanData_d2$name))
subj_Gershman_d2 <- as.numeric(subj_Gershman_d2)
trials_Gershman_d2 <- c(unique(GershmanData_d2$trial_number))

#trials/subj matrix
trialNum_Gershman_d2 <- array(length(trials_Gershman_d2),dim=c(length(subj_Gershman_d2)))

#reward matrix
reward_Gershman_d2 <- matrix(0,nrow = max(trials_Gershman_d2),length(subj_Gershman_d2))
GershmanData_d2$name <- as.numeric(GershmanData_d2$name)
GershmanData_d2$r <- as.numeric(GershmanData_d2$r)
k <- 1
for (i in subj_Gershman_d2) {
  reward_Gershman_d2[,k] <- GershmanData_d2$r[GershmanData_d2$name==i]
  k <- k+1
}
reward_Gershman_d2 <- t(reward_Gershman_d2)

#state matrix
state_Gershman_d2 <- matrix(0,nrow = max(trials_Gershman_d2),length(subj_Gershman_d2))
GershmanData_d2$game <- as.numeric(GershmanData_d2$game)
k <- 1
for (i in subj_Gershman_d2) {
  state_Gershman_d2[,k] <- GershmanData_d2$game[GershmanData_d2$name==i]
  k <- k+1
}
state_Gershman_d2 <- t(state_Gershman_d2)

#choice matrix
#choice matrix with 1 and 2 (2 is right, 1 is left)
choice_Gershman_d2 <- matrix(0,nrow = max(trials_Gershman_d2),ncol = length(subj_Gershman_d2))
k <- 1
GershmanData_d2$c <- as.numeric(GershmanData_d2$c )
for (i in subj_Gershman_d2) {
  choice_Gershman_d2[,k] <- GershmanData_d2$c[GershmanData_d2$name==i]
  k <- k + 1
}
choice_Gershman_d2 <- t(choice_Gershman_d2)

#choice_two matrix (0 is right, 1 is left)
choice_two_Gershman_d2 <- choice_Gershman_d2
choice_two_Gershman_d2[choice_two_Gershman_d2 == 2] <- 0

#stickiness matrix
stickiness_Gershman_d2 <- matrix(0,nrow = max(trials_Gershman_d2),ncol = length(subj_Gershman_d2))
k <- 1
GershmanData_d2$stick <- as.numeric(GershmanData_d2$stick)
for (i in subj_Gershman_d2) {
  stickiness_Gershman_d2[,k] <- GershmanData_d2$stick[GershmanData_d2$name==i]
  k <- k + 1
}
stickiness_Gershman_d2 <- t(stickiness_Gershman_d2)

#make all the first trials for each subject be 0s (since stickiness is impossible for trial 1)
for (i in c(1:length(subj_Gershman_d2))) {
  stickiness_Gershman_d2[i,1] <- 0
}

#create datalist with stickiness for just D2
v3_m3_datalist_Gershman_d2 <- list("NS" = 40, "MT" = 100, "NStim" = 4, "NC" = 2, "NT" = trialNum_Gershman_d2,
                                "K" = 3, "stim" = state_Gershman_d2, "rew" = reward_Gershman_d2, "choice" = choice_Gershman_d2, 
                                "choice_two" = choice_two_Gershman_d2, "stickiness" = stickiness_Gershman_d2)

#create datalist without stickiness for just D2
v3_datalist_Gershman_d2 <- list("NS" = 40, "MT" = 100, "NStim" = 4, "NC" = 2, "NT" = trialNum_Gershman_d2,
                                   "K" = 3, "stim" = state_Gershman_d2, "rew" = reward_Gershman_d2, "choice" = choice_Gershman_d2, 
                                   "choice_two" = choice_two_Gershman_d2)

