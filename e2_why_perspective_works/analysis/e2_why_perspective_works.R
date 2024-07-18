#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
#Experiment 3

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

##================ import baseline condition from e1 ===================================================================================

# Set wd to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set working directory to ../e1_original_or_copy/data/
setwd("../../e1_original_or_copy/data/")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data1 <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data1 <- (data1 <- do.call(rbind, data1))

dim(data1) #400

data1 <-subset(data1, data1$trialStruct.cond_num == 3)
data1$trialStruct.cond_num <- 1

data1 <- subset(data1, data1$trialStruct.attention == 0 & 
                 data1$trialStruct.comp_original_you == 2 & data1$trialStruct.comp_number_copies == 2)

dim(data1)

##================ import data from E3 ================================================================================================

# Set wd to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set working directory to ../e1_original_or_copy/data/
setwd("../data/")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data2 <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data2 <- (data2 <- do.call(rbind, data2))

dim(data2) #301
data2$trialStruct.cond_num <- data2$trialStruct.cond_num + 1

#exclude those who failed attention and comprehension checks
data2 <- subset(data2, data2$trialStruct.attention == 0)
n_initial <- dim(data2)[1]; n_initial

data2 <- subset(data2, (data2$trialStruct.comp_original_you == 2 & data2$trialStruct.comp_number_copies == 2))
n_final <- dim(data2)[1]; n_final

age <- as.numeric(data2$trialStruct.age); mean(age,na.rm = TRUE) 
gender <- as.factor(data2$trialStruct.sex); table(gender)[2]/sum(table(gender)) 

#combine data1 and data2
data <- rbind(data1, data2)
dim(data)
table(data$trialStruct.cond_num)

##======================== prep data for analysis ================================================================================

#code condition names and
#code choices for logistic regressions ('b' = binary)
for(i in 1:dim(data)[1]) {
  if(data$trialStruct.identity[i] == 1) {
    data$identity_name[i] = '1_original'
    data$two_or_one[i] = 'one'
    #original = 1, everything else zero
    data$identity_b_original[i] = 1
    data$identity_b_copy[i] = 0
    data$identity_b_neither[i] = 0
    data$identity_b_both[i] = 0
  } 
  else if(data$trialStruct.identity[i] == 2) {
    data$identity_name[i] = '2_copy'
    data$two_or_one[i] = 'one'
    #copy = 1, everything else zero
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

#code perspective condition as continuous variable
for(i in 1:length(persp)) {
  if(persp[i] == 'third') {
    data$persp_num[i] = 1
  }
  else if(persp[i] == 'empathy') {
    data$persp_num[i] = 2
  }
  else if(persp[i] == 'self') {
    data$persp_num[i] = 3
  }
  else if(persp[i] == 'full') {
    data$persp_num[i] = 4
  }
}

persp_num <- as.factor(data$persp_num)
data$persp_num <- persp_num

##========================================== plot ======================================================================

###### 1. identity: who are you? ########

#prepare data for plotting
tab <- matrix(NA,4,4)
colnames(tab) <- c('original', 'copy', 'neither', 'both')
rownames(tab) <- c('Third', 'Perspective', 'Feels the same', 'Is the same')
tab[1,] <- c(length(identity[identity == 1 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 2 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 3 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 4 & cond == 1])/length(identity[cond == 1]) )
tab[2,] <- c(length(identity[identity == 1 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 2 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 3 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 4 & cond == 2])/length(identity[cond == 2]) )
tab[3,] <- c(length(identity[identity == 1 & cond == 3])/length(identity[cond == 3]), length(identity[identity == 2 & cond == 3])/length(identity[cond == 3]), length(identity[identity == 3 & cond == 3])/length(identity[cond == 3]), length(identity[identity == 4 & cond == 3])/length(identity[cond == 3]) )
tab[4,] <- c(length(identity[identity == 1 & cond == 4])/length(identity[cond == 4]), length(identity[identity == 2 & cond == 4])/length(identity[cond == 4]), length(identity[identity == 4 & cond == 4])/length(identity[cond == 4]), length(identity[identity == 4 & cond == 4])/length(identity[cond == 4]) )
tab

#plot barplot
num_conds = length(unique(cond))
num_options = length(unique(identity))
condNames <-  c('Third Person', 'Perspective Manip', 'Feels the same', 'Feels continuous')
identity_mat <- array(0,dim=c(16,3))
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
  theme_bw()+coord_cartesian(ylim=c(0, 1))+
  theme_classic() 
p1+scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("")+ylab("Proportion of Identifications")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  scale_fill_manual(values = c("aquamarine3", "tomato3","darkcyan", "tomato4"), name="",labels=c('Original', 'Copy', 'Neither', 'Both'))

####### 2. Wrong to Tax? ##########

#prepare data for plotting
condNames <- c('Third', 'Empathy', 'Self', 'Full')
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
length(identity_b_original[identity_b_original==1 & persp=='third'])/length(identity_b_original[persp=='third'])
length(identity_b_original[identity_b_original==1 & persp=='empathy'])/length(identity_b_original[persp=='empathy'])
length(identity_b_original[identity_b_original==1 & persp=='self'])/length(identity_b_original[persp=='self'])
length(identity_b_original[identity_b_original==1 & persp=='full'])/length(identity_b_original[persp=='full'])

#copy
length(identity_b_copy[identity_b_copy==1 & persp=='third'])/length(identity_b_copy[persp=='third'])
length(identity_b_copy[identity_b_copy==1 & persp=='empathy'])/length(identity_b_copy[persp=='empathy'])
length(identity_b_copy[identity_b_copy==1 & persp=='self'])/length(identity_b_copy[persp=='self'])
length(identity_b_copy[identity_b_copy==1 & persp=='full'])/length(identity_b_copy[persp=='full'])

#both
length(identity_b_both[identity_b_both==1 & persp=='third'])/length(identity_b_both[persp=='third'])
length(identity_b_both[identity_b_both==1 & persp=='empathy'])/length(identity_b_both[persp=='empathy'])
length(identity_b_both[identity_b_both==1 & persp=='self'])/length(identity_b_both[persp=='self'])
length(identity_b_both[identity_b_both==1 & persp=='full'])/length(identity_b_both[persp=='full'])

##=========================== Multinomial Regression ===========================##

# Taking as numeric since this is an ordinal variable
data$persp_num_numeric <- as.numeric(data$persp_num)

# Training the multinomial model
multinom_model <- multinom(identity_name ~ persp_num_numeric, data = data)

# Checking the model
summary(multinom_model)

# Calculate p-value from standard error
z <- summary(multinom_model)$coefficients / summary(multinom_model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
print("p-values: ")
print(p)

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
