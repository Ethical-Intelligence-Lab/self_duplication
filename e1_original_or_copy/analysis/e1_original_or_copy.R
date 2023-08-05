#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
#Experiment 1

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

dir <- setwd("/Users/julian/Documents/github/juliandefreitas/serial_self/e1_original_or_copy/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

dim(data) #400

##======================== counts and exclusions =============================================================================

#exclude those who failed attention check
data <- subset(data, (data$trialStruct.attention == 0) & (data$trialStruct.comp_number_copies == 2))

#exclude those who failed comprehension check
for(i in 1:dim(data)[1]) {
  if( (data$trialStruct.cond_num[i] == 1) & (data$trialStruct.comp_original_you[i] == 1) ) {
    data$exclude[i] = 0
  }  
  else if ( (data$trialStruct.cond_num[i] == 2) & (data$trialStruct.comp_original_you[i] == 1) ) {
    data$exclude[i] = 0
  } 
  else if ( (data$trialStruct.cond_num[i] == 3) & (data$trialStruct.comp_original_you[i] == 2) ) {
    data$exclude[i] = 0
  } 
  else if ( (data$trialStruct.cond_num[i] == 4) & (data$trialStruct.comp_original_you[i] == 2) ) {
    data$exclude[i] = 0
  } 
  else {
    data$exclude[i] = 1
  }
}

data <- subset(data, data$exclude == 0)
dim(data) 

age <- as.numeric(data$trialStruct.age); mean(age,na.rm = TRUE) #35.9
gender <- as.factor(data$trialStruct.sex); table(gender)[2]/sum(table(gender)) #48.9

##======================== prep data for analysis ================================================================================

#code condition names
for(i in 1:dim(data)[1]) {
  if(data$trialStruct.identity[i] == 1) {
    data$identity_name[i] = '1_original'
    data$two_or_one[i] = 'one'
    data$identity_b_original[i] = 1
    data$identity_b_copy[i] = 0
    data$identity_b_neither[i] = 0
    data$identity_b_both[i] = 0
  } 
  else if(data$trialStruct.identity[i] == 2) {
    data$identity_name[i] = '2_copy'
    data$two_or_one[i] = 'one'
    data$identity_b_original[i] = 0
    data$identity_b_copy[i] = 1
    data$identity_b_neither[i] = 0
    data$identity_b_both[i] = 0
  }
  else if(data$trialStruct.identity[i] == 3) {
    data$identity_name[i] = '3_neither'
    data$two_or_one[i] = 'neither'
    data$identity_b_original[i] = 0
    data$identity_b_copy[i] = 0
    data$identity_b_neither[i] = 1
    data$identity_b_both[i] = 0
  }
  else if(data$trialStruct.identity[i] == 4) {
    data$identity_name[i] = '4_both'
    data$two_or_one[i] = 'two'
    data$identity_b_original[i] = 0
    data$identity_b_copy[i] = 0
    data$identity_b_neither[i] = 0
    data$identity_b_both[i] = 1
  }
}
data$identity_name <- as.factor(data$identity_name)
data$two_or_one <- as.factor(data$two_or_one)

##========================================== assign variable names ======================================================

living <- as.factor(data$trialStruct.original_cond)
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

data$living <- living
data$persp <- persp

#export subset of data for multinomial logistic regression in SPSS
data_export <- data[,c(30:37)]
write.csv(data_export, 'data_e1.csv')

##========================================== plot ======================================================================

###### 1. identity: who are you? ########

num_conds = length(unique(cond))
num_options = length(unique(identity))

condNames <- c('Alive-Third P', 'Alive-First P', 'Dead-Third P', 'Dead-First P')
identity_mat <- array(0,dim=c(16,3))
colnames(identity_mat) <- c('cond','answer','mean')
counter <- 1

#get proportions of each choice chosen
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
  theme_bw()+coord_cartesian(ylim=c(0, 1))+
  theme(axis.title.x = element_blank()) + 
  theme_classic() 
p1+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  ylab("Proportion of Identifications")+xlab ("")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  scale_fill_manual(values = c("aquamarine3", "tomato3","darkcyan", "tomato4"), name="",labels=c('Original', 'Copy', 'Neither', 'Both'))

####### 2. Ok to terminate? ##########

#prepare data for plotting
condNames <- c('Alive-Third P', 'Alive-First P', 'Dead-Third P', 'Dead-First P')
moral_mat <- array(0,dim=c(4,5))
colnames(moral_mat) <- c('cond','mean','sd','n','sem')

for (i in 1:4) { 
  moral_mat[i,] <- c(i,mean(moral[cond == i]),sd(moral[cond == i]),length(moral[cond == i]),0)
  moral_mat[i,5] <- moral_mat[i,3]/sqrt(moral_mat[i,4])
}

moral.mat <- as.data.frame(moral_mat, stringsAsFactors=F); moral.mat

#plot barplot
#quartz()
p1<-ggplot(moral.mat, aes(x=factor(cond),y=mean))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(1, 9))+
  theme_classic() 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge",width = 0.5)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("")+ylab("Moral Wrongness")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

##========================================= analysis =============================================================

###### 1. identity: who are you? ########

#original
length(identity_b_original[identity_b_original==1 & living=='alive'])/length(identity_b_original[living=='alive'])
length(identity_b_original[identity_b_original==1 & living=='dead'])/length(identity_b_original[living=='dead'])
length(identity_b_original[identity_b_original==1 & persp=='first'])/length(identity_b_original[persp=='first'])
length(identity_b_original[identity_b_original==1 & persp=='third'])/length(identity_b_original[persp=='third'])

#copy
length(identity_b_copy[identity_b_copy==1 & living=='alive'])/length(identity_b_copy[living=='alive'])
length(identity_b_copy[identity_b_copy==1 & living=='dead'])/length(identity_b_copy[living=='dead'])
length(identity_b_copy[identity_b_copy==1 & persp=='first'])/length(identity_b_copy[persp=='first'])
length(identity_b_copy[identity_b_copy==1 & persp=='third'])/length(identity_b_copy[persp=='third'])

#both
length(identity_b_both[identity_b_both==1 & persp=='first'])/length(identity_b_both[persp=='first'])
length(identity_b_both[identity_b_both==1 & persp=='third'])/length(identity_b_both[persp=='third'])

####### 2. Ok to terminate? ##########

#anova
moral_mod <- aov(moral ~ living*persp, data = data)
summary(moral_mod)
etaSquared(moral_mod) # see second column

#paired comparisons: 

#1. alive
mean(moral[living == 'alive'])
sd(moral[living == 'alive'])
n_alive <- length(moral[living == 'alive'])

mean(moral[living == 'dead'])
sd(moral[living == 'dead'])
n_dead <- length(moral[living == 'dead'])

alive_v_dead <- t.test(moral ~ living, var.equal=TRUE, paired=FALSE); alive_v_dead
tes(as.numeric(alive_v_dead[1]), n_alive, n_dead) #cohen's d

#2. perspective
mean(moral[persp == 'first'])
sd(moral[persp == 'first'])
n_first <- length(moral[persp == 'first'])

mean(moral[persp == 'third'])
sd(moral[persp == 'third'])
n_third <- length(moral[persp == 'third'])

first_v_third <- t.test(moral ~ persp, var.equal=TRUE, paired=FALSE); first_v_third
tes(as.numeric(first_v_third[1]), n_first, n_third) #cohen's d

####======================================= end =========================================================

rm(list = ls()) 

