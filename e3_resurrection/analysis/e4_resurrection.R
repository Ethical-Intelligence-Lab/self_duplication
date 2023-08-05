#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
#Experiment 4

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

dir <- setwd("/Users/julian/Documents/github/juliandefreitas/serial_self/e4_resurrection/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

n_before_excl <- dim(data); n_before_excl #208

##======================== counts and exclusions ================================================================================

#exclude those who failed attention check
data <- subset(data, data$trialStruct.attention == 0 & 
                 data$trialStruct.comp_original_you == 3 & data$trialStruct.comp_number_copies == 2)

n_after_excl <- dim(data); n_after_excl #176
n_excl <- n_before_excl - n_after_excl; n_excl

age <- as.numeric(data$trialStruct.age); mean(age,na.rm = TRUE) #35.1
gender <- as.factor(data$trialStruct.sex); table(gender)[2]/sum(table(gender)) #56%

##======================== prep data for analysis ================================================================================

#since condition was within subjects string it out into one long vector
cond1 <- rep(1, times = dim(data)[1])
cond1_name <- rep('2_dead', times = dim(data)[1])
cond2 <- rep(2, times = dim(data)[1])
cond2_name <- rep('1_revived', times = dim(data)[1])
cond <- c(cond1,cond2)
cond_name <- c(cond1_name, cond2_name)

subject1 <- seq(1, dim(data)[1], by=1)
subject2 <- seq(1, dim(data)[1], by=1)
subject <- c(subject1, subject2)

identity_before <- data$trialStruct.identity_before
identity_after <- data$trialStruct.identity_after
identity <- c(identity_before, identity_after)

identity_name <- rep(0, times = length(identity))
two_or_one <- rep('a', times = length(identity))
identity_b_original <- rep(9, times = length(identity))
identity_b_copy <- rep(9, times = length(identity))
identity_b_neither <- rep(9, times = length(identity))
identity_b_both <- rep(9, times = length(identity))

#code choice names &
#code choices for logistic regressions
for(i in 1:length(identity)) {
  if(identity[i] == 1) {
    identity_name[i] = '1_original'
    two_or_one[i] = 'one'
    #original = 1, everything else zero
    identity_b_original[i] = 1
    identity_b_copy[i] = 0
    identity_b_neither[i] = 0
    identity_b_both[i] = 0
  } 
  else if(identity[i] == 2) {
    identity_name[i] = '2_copy'
    two_or_one[i] = 'one'
    #copy = 1, everything else zero
    identity_b_original[i] = 0
    identity_b_copy[i] = 1
    identity_b_neither[i] = 0
    identity_b_both[i] = 0
  }
  else if(identity[i] == 3) {
    identity_name[i] = '3_neither'
    two_or_one[i] = 'neither'
    identity_b_original[i] = 0
    identity_b_copy[i] = 0
    identity_b_neither[i] = 1
    identity_b_both[i] = 0
  }
  else if(identity[i] == 4) {
    identity_name[i] = '4_both'
    two_or_one[i] = 'two'
    identity_b_original[i] = 0
    identity_b_copy[i] = 0
    identity_b_neither[i] = 0
    identity_b_both[i] = 1
  }
}

two_or_one <- as.factor(two_or_one)

cond <- as.factor(cond)
identity <- as.numeric(identity)
edu_science <- as.numeric(data$trialStruct.edu_science)
edu_phil <- as.numeric(data$trialStruct.edu_phil)

#export data for multinomial regression in SPSS
export_mat <- array(0,dim=c(length(identity),4))
export_mat <- as.data.frame(export_mat)
colnames(export_mat) <- c('ss', 'cond_name', 'identity_name', 'two_or_one')
export_mat[,1] <- subject
export_mat[,2] <- cond_name
export_mat[,3] <- identity_name
export_mat[,4] <- two_or_one

write.csv(export_mat, 'data_e4.csv')

##========================================== plot ======================================================================

###### 1. identity: who are you? ########

#prepare data for plotting
tab <- matrix(NA,2,4)
colnames(tab) <- c('Original', 'Copy', 'Neither', 'Both')
rownames(tab) <- c('before', 'after')
tab[1,] <- c(length(identity[identity == 1 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 2 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 3 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 4 & cond == 1])/length(identity[cond == 1]) )
tab[2,] <- c(length(identity[identity == 1 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 2 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 3 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 4 & cond == 2])/length(identity[cond == 2]) )
tab

#plot barplot
tab_transpose <- t(tab)
barplot(tab_transpose, main="Who are you?", xlab = "Condition", ylab= "Proportion", beside=TRUE, legend = rownames(tab_transpose))

num_conds = length(unique(cond))
num_options = length(unique(identity))

condNames <- c('Dead', 'Revived')
identity_mat <- array(0,dim=c(8,3))
colnames(identity_mat) <- c('cond','answer','mean')
counter <- 1

for (i in 1:num_conds) { 
  for (j in 1:num_options) { 
    identity_mat[counter,] <- c(i, j,length(identity[identity == j & cond == i])/length(identity[cond == i]))
    counter <- counter + 1
  }
}
identity.mat <- as.data.frame(identity_mat, stringsAsFactors=F); identity.mat

#center all plot titles
theme_update(plot.title = element_text(hjust = 0.5))

#plot barplot
#quartz()
p1<-ggplot(identity.mat, aes(x=factor(cond),y=mean,fill=factor(answer)))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(0, 0.5))+
  theme_classic() 
p1+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("State of the Original")+ylab("Proportion of Identifications")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("aquamarine3", "tomato3","darkcyan", "tomato4"), name="",labels=c('Original', 'Copy', 'Neither', 'Both'))

##========================================= analysis =============================================================

###### 1. identity: who are you? ########

#glm for each of the four possible answers
# https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet

#original
length(identity_b_original[identity_b_original==1 & cond==2])/length(identity_b_original[cond==2]) #after
length(identity_b_original[identity_b_original==1 & cond==1])/length(identity_b_original[cond==1]) #before

#glmer_original <- glmer(identity_b_original ~ cond + (1 | subject), family=binomial) #mixed glm, because condition is within-ss
#summary(glmer_original)

#copy
length(identity_b_copy[identity_b_copy==1 & cond==2])/length(identity_b_copy[cond==2]) #after
length(identity_b_copy[identity_b_copy==1 & cond==1])/length(identity_b_copy[cond==1]) #before

#glmer_copy <- glmer(identity_b_copy ~ cond + (1 | subject), family=binomial)
#summary(glmer_copy)

#neither
length(identity_b_neither [identity_b_neither==1 & cond==2])/length(identity_b_neither[cond==2]) #after
length(identity_b_neither[identity_b_neither==1 & cond==1])/length(identity_b_neither[cond==1]) #before

#glmer_neither <- glmer(identity_b_neither ~ cond + (1 | subject), family=binomial)
#summary(glmer_neither)

#both
length(identity_b_both[identity_b_both==1 & cond==2])/length(identity_b_both[cond==2]) #after 
length(identity_b_both[identity_b_both==1 & cond==1])/length(identity_b_both[cond==1]) #before

#glmer_both <- glmer(identity_b_both ~ cond + (1 | subject), family=binomial)
#summary(glmer_both)

####======================================= end =========================================================

rm(list = ls()) 

