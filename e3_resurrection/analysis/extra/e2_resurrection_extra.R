#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The capacity of personal identity
#Experiment 2

## clear workspace
rm(list = ls())  

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)} 
if (!require(lsmeans)) {install.packages("lsmean"); require(lsmeans)}
if (!require(nnet)) {install.packages("nnet"); require(nnet)}
if (!require(mlogit)) {install.packages("mlogit"); require(mlogit)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}

##================ import data ================================================================================================

dir <- setwd("/Users/julian/Dropbox (Personal)/Research/Intuition/single_identity/e2_resurrection/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

dim(data) #208

##======================== counts and exclusions =============================================================================

#exclude those who failed attention check
data <- subset(data, data$trialStruct.attention == 0 & data$trialStruct.comp_original_you == 3 & data$trialStruct.comp_number_copies == 2)
dim(data) #176

##======================== prep data for analysis ================================================================================

#since condition was within subjects string it out into one long vector
cond1 <- rep(1, times = dim(data)[1])
cond2 <- rep(2, times = dim(data)[1])
cond <- c(cond1,cond2)

subject1 <- seq(1, dim(data)[1], by=1)
subject2 <- seq(1, dim(data)[1], by=1)
subject <- c(subject1, subject2)

identity_before <- data$trialStruct.identity_before
identity_after <- data$trialStruct.identity_after
identity <- c(identity_before, identity_after)

identity_name <- rep(0, times = length(identity))

#code choice names
for(i in 1:length(identity)) {
  if(identity[i] == 1) {
    identity_name[i] = '1_original'
  } 
  else if(identity[i] == 2) {
    identity_name[i] = '2_copy'
  }
  else if(identity[i] == 3) {
    identity_name[i] = '3_neither'
  }
  else if(identity[i] == 4) {
    identity_name[i] = '4_both'
  }
}

#code choices for logistic regressions
identity_b_original <- rep(9, times = length(identity))
identity_b_copy <- rep(9, times = length(identity))
identity_b_neither <- rep(9, times = length(identity))
identity_b_both <- rep(9, times = length(identity))

for(i in 1:length(identity)) {
  #original = 1, everything else zero
  if(identity[i] == 1) {
    identity_b_original[i] = 1
    identity_b_copy[i] = 0
    identity_b_neither[i] = 0
    identity_b_both[i] = 0
  }
  #copy = 1, everything else zero
  else if(identity[i] == 2) {
    identity_b_original[i] = 0
    identity_b_copy[i] = 1
    identity_b_neither[i] = 0
    identity_b_both[i] = 0
  }
  #neither = 1, everything else zero
  else if(identity[i] == 3) {
    identity_b_original[i] = 0
    identity_b_copy[i] = 0
    identity_b_neither[i] = 1
    identity_b_both[i] = 0
  }
  #both = 1, everything else zero
  else if(identity[i] == 4) {
    identity_b_original[i] = 0
    identity_b_copy[i] = 0
    identity_b_neither[i] = 0
    identity_b_both[i] = 1
  }
}

##========================================== export csv for check analysis in spss ======================================

export_mat <- array(0,dim=c(length(identity),8))
colnames(export_mat) <- c('ss', 'cond', 'identity', 'identity_name', 'identity_b_original', 'identity_b_copy', 'identity_b_neither', 'identity_b_both')
export_mat[,1] <- subject
export_mat[,2] <- cond
export_mat[,3] <- identity
export_mat[,4] <- identity_name
export_mat[,5] <- identity_b_original
export_mat[,6] <- identity_b_copy
export_mat[,7] <- identity_b_neither
export_mat[,8] <- identity_b_both
  
#write csv for spss analysis
#write.csv(data, file = "data_S3.csv")

##========================================== assign variable names ======================================================

age <- as.numeric(data$trialStruct.age); mean(age,na.rm = TRUE) #35.2
gender <- as.factor(data$trialStruct.sex); table(gender)[2]/sum(table(gender)) #56%
cond <- as.factor(cond)
identity <- as.numeric(identity)
edu_science <- as.numeric(data$trialStruct.edu_science)
edu_phil <- as.numeric(data$trialStruct.edu_phil)

##========================================== plot ======================================================================

###### 1. identity: who are you? ########

#prepare data for plotting
tab <- matrix(NA,2,4)
colnames(tab) <- c('original', 'copy', 'neither', 'both')
rownames(tab) <- c('before', 'after')
tab[1,] <- c(length(identity[identity == 1 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 2 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 3 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 4 & cond == 1])/length(identity[cond == 1]) )
tab[2,] <- c(length(identity[identity == 1 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 2 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 3 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 4 & cond == 2])/length(identity[cond == 2]) )
tab

quartz()
#plot barplot
tab_transpose <- t(tab)
barplot(tab_transpose, main="Who are you?", xlab = "Condition", ylab= "Proportion", beside=TRUE, legend = rownames(tab_transpose))

##========================================= analysis =============================================================

###### 1. identity: who are you? ########

# wt multinomial logistic regression
#data$persp <- relevel(data$persp, ref = 'third')
#logitdata <- mlogit.data(data, choice = "identity_name", shape = "wide") #multinomial logistic regression
#logitMod <- mlogit(identity_name ~ 1 | living*persp, data = logitdata, reflevel = 1 )
#summary(logitMod)

#glm for each of the four possible answers

#original
mod_original <- glm(identity_b_original ~ cond, data = data, family=binomial())
summary(mod_original)

glmer_original <- glmer(identity_b_original ~ cond + (1 | subject), family=binomial)
summary(glmer_original)

#copy
mod_copy <- glm(identity_b_copy ~ cond, data = data, family=binomial())
summary(mod_copy)

glmer_copy <- glmer(identity_b_copy ~ cond + (1 | subject), family=binomial)
summary(glmer_copy)

#neither
mod_neither <- glm(identity_b_neither ~ cond, data = data, family=binomial())
summary(mod_neither)

glmer_neither <- glmer(identity_b_neither ~ cond + (1 | subject), family=binomial)
summary(glmer_neither)

#both
mod_both <- glm(identity_b_both ~ cond, data = data, family=binomial())
summary(mod_both)

glmer_both <- glmer(identity_b_both ~ cond + (1 | subject), family=binomial)
summary(glmer_both)

####======================================= end =========================================================

rm(list = ls()) 

