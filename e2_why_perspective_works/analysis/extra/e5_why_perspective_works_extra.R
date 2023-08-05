#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The capacity of personal identity
#Experiment 5

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

##================ import data ================================================================================================

dir <- setwd("/Users/julian/Dropbox (Personal)/Research/Intuition/single_identity/e5_why_perspective_works/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

dim(data) #2157

##======================== counts and exclusions =============================================================================

# num ss recruited
dim(data) 

#exclude those who failed attention check
data <- subset(data, data$trialStruct.attention == 0 & data$trialStruct.comp_original_you == 2 & data$trialStruct.comp_number_copies == 2)

dim(data)

##======================== prep data for analysis ================================================================================

#code condition names
for(i in 1:dim(data)[1]) {
  if(data$trialStruct.identity[i] == 1) {
    data$identity_name[i] = '1_original'
  } 
  else if(data$trialStruct.identity[i] == 2) {
    data$identity_name[i] = '2_copy'
  }
  else if(data$trialStruct.identity[i] == 3) {
    data$identity_name[i] = '3_neither'
  }
  else if(data$trialStruct.identity[i] == 4) {
    data$identity_name[i] = '4_both'
  }
}
data$identity_name <- as.factor(data$identity_name)

#code choices for logistic regressions ('b' = binary)
for(i in 1:dim(data)[1]) {
  #original = 1, everything else zero
  if(data$trialStruct.identity[i] == 1) {
    data$identity_b_original[i] = 1
    data$identity_b_copy[i] = 0
    data$identity_b_neither[i] = 0
    data$identity_b_both[i] = 0
  }
  #copy = 1, everything else zero
  else if(data$trialStruct.identity[i] == 2) {
    data$identity_b_original[i] = 0
    data$identity_b_copy[i] = 1
    data$identity_b_neither[i] = 0
    data$identity_b_both[i] = 0
  }
  #neither = 1, everything else zero
  else if(data$trialStruct.identity[i] == 3) {
    data$identity_b_original[i] = 0
    data$identity_b_copy[i] = 0
    data$identity_b_neither[i] = 1
    data$identity_b_both[i] = 0
  }
  #both = 1, everything else zero
  else if(data$trialStruct.identity[i] == 4) {
    data$identity_b_original[i] = 0
    data$identity_b_copy[i] = 0
    data$identity_b_neither[i] = 0
    data$identity_b_both[i] = 1
  }
}

##========================================== assign variable names ======================================================

age <- as.numeric(data$trialStruct.age); mean(age,na.rm = TRUE) #37
gender <- as.factor(data$trialStruct.sex); table(gender)[2]/sum(table(gender)) #55

persp <- as.factor(data$trialStruct.perspective_cond)
cond <- as.factor(data$trialStruct.cond_num)
identity <- as.numeric(data$trialStruct.identity)
b_identity <- as.numeric(data$identity_binary) #binary identity
moral <- as.numeric(data$trialStruct.moral)
edu_science <- as.numeric(data$trialStruct.edu_science)
edu_phil <- as.numeric(data$trialStruct.edu_phil)
identity_b_original <- as.numeric(data$identity_b_original)
identity_b_copy <- as.numeric(data$identity_b_copy)
identity_b_neither <- as.numeric(data$identity_b_neither)
identity_b_both <- as.numeric(data$identity_b_both)

data$persp <- persp

##========================================== plot ======================================================================

###### 1. identity: who are you? ########

#prepare data for plotting
tab <- matrix(NA,3,4)
colnames(tab) <- c('original', 'copy', 'neither', 'both')
rownames(tab) <- c('empathy', 'self', 'full')
tab[1,] <- c(length(identity[identity == 1 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 2 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 3 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 4 & cond == 1])/length(identity[cond == 1]) )
tab[2,] <- c(length(identity[identity == 1 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 2 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 3 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 4 & cond == 2])/length(identity[cond == 2]) )
tab[3,] <- c(length(identity[identity == 1 & cond == 3])/length(identity[cond == 3]), length(identity[identity == 2 & cond == 3])/length(identity[cond == 3]), length(identity[identity == 3 & cond == 3])/length(identity[cond == 3]), length(identity[identity == 4 & cond == 3])/length(identity[cond == 3]) )
tab

#plot barplot
tab_transpose <- t(tab)
barplot(tab_transpose, main="Who are you?", xlab = "Condition", ylab= "Proportion", beside=TRUE, legend = rownames(tab_transpose))

####### 2. Ok to terminate? ##########

#prepare data for plotting
condNames <- c('Empathy', 'Self', 'Full')
moral_mat <- array(0,dim=c(3,5))
colnames(moral_mat) <- c('cond','mean','sd','n','sem')

for (i in 1:4) { 
  moral_mat[i,] <- c(i,mean(moral[cond == i]),sd(moral[cond == i]),length(moral[cond == i]),0)
  moral_mat[i,5] <- moral_mat[i,3]/sqrt(moral_mat[i,4])
}

moral.mat <- as.data.frame(moral_mat, stringsAsFactors=F); moral.mat

#plot barplot
title <- c('Wrong to tax the copy?') 
p1<-ggplot(moral.mat,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1, 9)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean")

#morality boxplot
boxplot(moral ~ cond, data = data, lwd = 2, ylab = 'How ok it is to kill the copy', names=condNames, main="Moral judgment boxplot")
stripchart(identity ~ cond, vertical = TRUE, data = data, 
           method = "jitter", add = TRUE, pch = 20, col = 'blue')

##========================================= analysis =============================================================

###### 1. identity: who are you? ########

#glm for each of the four possible answers

#original
length(identity_b_original[identity_b_original==1 & persp=='first'])/length(identity_b_original[persp=='first'])
length(identity_b_original[identity_b_original==1 & persp=='third'])/length(identity_b_original[persp=='third'])

mod_original <- glm(identity_b_original ~ persp, data = data, family=binomial())
summary(mod_original)

#copy
mod_copy <- glm(identity_b_copy ~ persp, data = data, family=binomial())
summary(mod_copy)

#neither
mod_neither <- glm(identity_b_neither ~ persp, data = data, family=binomial())
summary(mod_neither)

#both
length(identity_b_both[identity_b_both==1 & persp=='first'])/length(identity_b_both[persp=='first'])
length(identity_b_both[identity_b_both==1 & persp=='third'])/length(identity_b_both[persp=='third'])

mod_both <- glm(identity_b_both ~ persp, data = data, family=binomial())
summary(mod_both)


####### 2. Ok to terminate? ##########

#anova
moral_mod <- aov(moral ~ persp, data = data)
summary(moral_mod)
etaSquared(moral_mod) # see second column

#paired comparisons
mean(moral[persp == 'first'])
sd(moral[persp == 'first'])
n_first <- length(moral[persp == 'first'])

mean(moral[persp == 'third'])
sd(moral[persp == 'third'])
n_third <- length(moral[persp == 'third'])

#alive vs. dead
alive_v_dead <- t.test(moral ~ persp, var.equal=TRUE, paired=FALSE); alive_v_dead
tes(as.numeric(alive_v_dead[1]), n_third, n_third) #cohen's d

####======================================= end =========================================================

rm(list = ls()) 

