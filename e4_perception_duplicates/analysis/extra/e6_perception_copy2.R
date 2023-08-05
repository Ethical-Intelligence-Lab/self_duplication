#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - Capacity limits of personal identity
#Experiment 6

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}
if (!require(jsonlite)) {install.packages("jsonlite"); require(jsonlite)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(reshape2)) {install.packages("reshape2"); require(reshape2)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)}
if (!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}
if (!require(devtools)) {install.packages("devtools"); require(devtools)}
if (!require(ggpubr)) {install.packages("ggpubr"); require(ggpubr)}
if (!require(ggsignif)) {install.packages("ggsignif"); require(ggsignif)}

##================================================================================================================
                                              ##IMPORT DATA##
##================================================================================================================

## set directory to data folder
dir <- setwd("/Users/julian/Dropbox (Personal)/Research/Intuition/single_identity/e6_perception_duplicates/data")

datalist = list()
#import data using jsonlite [automate this, by defining list of data frames]
files <- list.files(pattern=('*txt'))
for (i in 1:length(files)) {
    curData <- fromJSON(files[i], simplifyDataFrame = TRUE)
    datalist[[i]] <- curData$trialStruct
}

data = do.call(rbind, datalist)
head(data)
dim(data)
length(unique(data$workerId)) #201 subjects

#check that we have equal numbers for each condition
table(data$image)
table(data$label)
table(data$agentCond)
table(data$matchCond)

##================================================================================================================
                                                ##DATA PREP##
##================================================================================================================

### perform exclusions: attention, comprehension, and rts < 200 (this ends up performing both subject and trial)
### note, we exclude based on mean accuracy further below
### note, data$comp_mental_content actually refers to comp_number_copies
data <- subset(data,(data$attentionMCQ=="0" & data$comp_original_you==3 &
                       data$comp_mental_content==2 & data$comp=='D')) 
length(unique(data$workerId)) #1 subjects
dim(data)
data$rt[is.na(data$rt)] <- 0

#mark which trials had reasonable rts
for(i in 1:dim(data)[1]) {
  if(data$rt[i] >= 100) {
    data$badRt[i] = 0
  }
  else {
    data$badRt[i] = 1
  }
}

### exclude subjects with lower than 55% average accuracy
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
perf_thresh <- 0.55
trial_thresh <- 0.5

ss_excl_mat <- array(0,dim=c(length(workers),2))
colnames(ss_excl_mat) <- c('mean_acc', 'rt_err_prop')

#if their accuracy < 55% or total bad RTs > 50% of trials, exclude them from dataset
for(i in 1:length(workers)) {
  ss_excl_mat[i,1] <- c(mean(acc_use[worker==workers[i]])) #get accuracy for each worker
  ss_excl_mat[i,2] <- sum(data$badRt[data$workerId == workers[i]])/length(data$rt[data$workerId == workers[i]])
  if( (ss_excl_mat[i,1] < perf_thresh) | (ss_excl_mat[i,2] > trial_thresh) ) {
    data <- subset(data,data$workerId != workers[i]) 
  }
}
ss_excl_mat

age <- as.numeric(data$age); mean(age,na.rm = TRUE) #32.10
gender <- as.factor(data$sex); table(gender)[1]/sum(table(gender)) #0.51

#assign variable names
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
identity_bef <- data$identity_fetus #these variables were just mistakenly named
identity_aft <- data$identity_pvs
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
numTrials <- dim(data)[1]
matchCond <- as.factor(data$matchCond)
agentCond <- as.factor(data$agentCond)

#create numeric version of condition
for(i in 1:length(agentCond)) {
  if(agentCond[i] == 'original') {
    data$agentCond_n[i] = 1
  }
  else if(agentCond[i] == 'copy') {
    data$agentCond_n[i] = 2
  }
  else if(agentCond[i] == 'stranger') {
    data$agentCond_n[i] = 3
  }
}
agentCond_n <- as.factor(data$agentCond_n)
ans <- as.factor(data$ans)
corrAns <- as.factor(data$corrAns)
rts <- log(as.numeric(data$rt))
agentConds_n <- as.factor(unique(agentCond_n))
matchConds <- unique(matchCond)

#create matrix for collecting d prime-relevant values, and rts
d_mat <- array(0,dim=c(3*length(workers),7))
colnames(d_mat) <- c('worker', 'agentCond', 'hits', 'false_alarms', 'd', 'rt', 'after_resp')
d.mat <- as.data.frame(d_mat, stringsAsFactors=FALSE); d.mat

#for each worker and agent condition, get average hits, false alarms, d-prime, and rt
#hits: proportion of true hits you said were hits (out of all true hits)
#false alarms: proportin of non hits you said were hits (out of all non hits)
#get rts for trials that were correct
counter <- 1
for(i in 1:length(workers)) {
  for(j in 1:length(agentConds_n)) {
        length_h <- length(acc_use[worker==workers[i] & agentCond_n==agentConds_n[j] & corrAns=='y'])
        length_fa <- length(acc_use[worker==workers[i] & agentCond_n==agentConds_n[j] & corrAns=='n'])
        
        h <- length(acc_use[worker==workers[i] & agentCond_n==agentConds_n[j] & ans=='y' & corrAns=='y'])/length_h
        fa <- length(acc_use[worker==workers[i] & agentCond_n==agentConds_n[j] & ans=='y' & corrAns=='n'])/length_fa
        rt <- mean(rts[worker==workers[i] & agentCond_n==agentConds_n[j] & acc==1], na.rm=TRUE)
       
        #correction for if h=1 or fa=0, by (Macmillan & Kaplan, 1985)
        #if 0, 0.5/𝑛; if 1, (𝑛−0.5)/𝑛 , where n is the number of signal or noise trials
        #see https://stats.stackexchange.com/questions/134779/d-prime-with-100-hit-rate-probability-and-0-false-alarm-probability
        if(h==1) {
          h <- (length_h-0.5)/length_h
        }
        
        if(fa==0) {
          fa <- 0.05/length_fa
        }
        
        d.mat[counter,] <- c (workers[i], agentConds_n[j], h, fa, qnorm(h) - qnorm(fa), rt, mean(identity_aft[worker==workers[i]]))
        counter = counter + 1
  }
} 

#subset d.mat by whether people identified with the original or both 
d.mat_o <- subset(d.mat,(d.mat$after==1))
d.mat_b <- subset(d.mat,(d.mat$after==4))

#create matrix for collectin performance difference (self - other) and overall performance
#note, for performance difference we're ignoring people who identified with copy or neither, so perf diff = 0
iden_mat <- array(0,dim=c(length(workers),5))
colnames(iden_mat) <- c('before', 'after','zeros','att_diff', 'total_perf')
for(i in 1:length(workers)) {
  iden_mat[i,] <- c( mean(identity_bef[worker==workers[i]]), mean(identity_aft[worker==workers[i]]),0, 0, 0)
}
iden.mat <- as.data.frame(iden_mat, stringsAsFactors=FALSE); iden.mat

for(i in 1:length(workers)) {
  if(unique(d.mat$after_resp[d.mat$worker == i]) == 1) { #if they identified with original...
    iden.mat[i,4] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] - d.mat$d[d.mat$worker==i & d.mat$agentCond == 3] #original - stranger  
  }
  else if(unique(d.mat$after_resp[d.mat$worker == i]) == 4) { #if they identified with both...
    iden.mat[i,4] <- max(d.mat$d[d.mat$worker==i & d.mat$agentCond == 1], (d.mat$d[d.mat$worker==i & d.mat$agentCond == 2])) - d.mat$d[d.mat$worker==i & d.mat$agentCond == 3] #(best of original or copy) - stranger  
  }
  
  iden.mat[i,5] <- d.mat$d[d.mat$worker==i & d.mat$agentCond == 1] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 2] + d.mat$d[d.mat$worker==i & d.mat$agentCond == 3]
}

#sort identity mat by original - copy
iden_ordered <- iden.mat[order(-iden.mat$att_diff),]
iden_ordered$counter <- c(1:length(workers))
iden_ordered

##================================================================================================================
                                      ##DATA PREP TO REPLICATE REVIVAL EFFECT##
##================================================================================================================

cond1 <- rep(1, times = dim(iden.mat)[1])
cond2 <- rep(2, times = dim(iden.mat)[1])
cond <- c(cond1,cond2)

subject1 <- seq(1, dim(iden.mat)[1], by=1)
subject2 <- seq(1, dim(iden.mat)[1], by=1)
subject <- c(subject1, subject2)

identity_before <- iden.mat$before
identity_after <- iden.mat$after
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

###================================================================================================================
                                                      ##ANALYSIS##
##================================================================================================================

######### Replication of revival judgment effect

#glm for each of the four possible answers
# https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet

#original
length(identity_b_original[identity_b_original==1 & cond==1])/length(identity_b_original[cond==1]) #before
length(identity_b_original[identity_b_original==1 & cond==2])/length(identity_b_original[cond==2]) #after

glmer_original <- glmer(identity_b_original ~ cond + (1 | subject), family=binomial)
summary(glmer_original)###copy

#copy
length(identity_b_copy[identity_b_copy==1 & cond==1])/length(identity_b_copy[cond==1]) #before
length(identity_b_copy[identity_b_copy==1 & cond==2])/length(identity_b_copy[cond==2]) #after

glmer_copy <- glmer(identity_b_copy ~ cond + (1 | subject), family=binomial)
summary(glmer_copy)

#neither
length(identity_b_neither[identity_b_neither==1 & cond==1])/length(identity_b_neither[cond==1]) #before
length(identity_b_neither [identity_b_neither==1 & cond==2])/length(identity_b_neither[cond==2]) #after

glmer_neither <- glmer(identity_b_neither ~ cond + (1 | subject), family=binomial)
summary(glmer_neither)

#both
length(identity_b_both[identity_b_both==1 & cond==1])/length(identity_b_both[cond==1]) #before
length(identity_b_both[identity_b_both==1 & cond==2])/length(identity_b_both[cond==2]) #after 

glmer_both <- glmer(identity_b_both ~ cond + (1 | subject), family=binomial)
summary(glmer_both)

# PERCEPTUAL EFFECTS
################### GROUP DIFFERENCES

p_mat <- rep(9, times = 6)
star_mat <- rep(9, times = 6)

####### chose original #####
mean(d.mat_o$d[d.mat_o$agentCond==1])
sd(d.mat_o$d[d.mat_o$agentCond==1])
n_o_1 = length(d.mat_o$d[d.mat_o$agentCond==1])

mean(d.mat_o$d[d.mat_o$agentCond==2])
sd(d.mat_o$d[d.mat_o$agentCond==2])
n_o_2 = length(d.mat_o$d[d.mat_o$agentCond==2])

mean(d.mat_o$d[d.mat_o$agentCond==3])
sd(d.mat_o$d[d.mat_o$agentCond==3])
n_o_3 = length(d.mat_o$d[d.mat_o$agentCond==3])

att_1_o <- t.test(d.mat_o$d[d.mat_o$agentCond==1 | d.mat_o$agentCond==2] ~ d.mat_o$agentCond[d.mat_o$agentCond==1 | d.mat_o$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2_o <- t.test(d.mat_o$d[d.mat_o$agentCond==1 | d.mat_o$agentCond==3] ~ d.mat_o$agentCond[d.mat_o$agentCond==1 | d.mat_o$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3_o <- t.test(d.mat_o$d[d.mat_o$agentCond==2 | d.mat_o$agentCond==3] ~ d.mat_o$agentCond[d.mat_o$agentCond==2 | d.mat_o$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1_o[1]), n_o_1, n_o_2) #cohen's d
tes(as.numeric(att_2_o[1]), n_o_1, n_o_3)
tes(as.numeric(att_3_o[1]), n_o_2, n_o_3)

# chose both
mean(d.mat_b$d[d.mat_b$agentCond==1])
sd(d.mat_b$d[d.mat_b$agentCond==1])
n_b_1 = length(d.mat_b$d[d.mat_b$agentCond==1])

mean(d.mat_b$d[d.mat_b$agentCond==2])
sd(d.mat_b$d[d.mat_b$agentCond==2])
n_b_2 = length(d.mat_b$d[d.mat_b$agentCond==2])

mean(d.mat_b$d[d.mat_b$agentCond==3])
sd(d.mat_b$d[d.mat_b$agentCond==3])
n_b_3 = length(d.mat_b$d[d.mat_b$agentCond==3])

att_1_b <- t.test(d.mat_b$d[d.mat_b$agentCond==1 | d.mat_b$agentCond==2] ~ d.mat_b$agentCond[d.mat_b$agentCond==1 | d.mat_b$agentCond==2], var.equal=TRUE, paired=TRUE); att_1
att_2_b <- t.test(d.mat_b$d[d.mat_b$agentCond==1 | d.mat_b$agentCond==3] ~ d.mat_b$agentCond[d.mat_b$agentCond==1 | d.mat_b$agentCond==3], var.equal=TRUE, paired=TRUE); att_2
att_3_b <- t.test(d.mat_b$d[d.mat_b$agentCond==2 | d.mat_b$agentCond==3] ~ d.mat_b$agentCond[d.mat_b$agentCond==2 | d.mat_b$agentCond==3], var.equal=TRUE, paired=TRUE); att_3

tes(as.numeric(att_1_b[1]), n_b_1, n_b_2) #cohen's d
tes(as.numeric(att_2_b[1]), n_b_1, n_b_3)
tes(as.numeric(att_3_b[1]), n_b_2, n_b_3)

p_mat <- c(att_1_o[3], att_2_o[3], att_3_o[3], att_1_b[3], att_2_b[3], att_3_b[3])
for(i in 1:length(p_mat)) {
  if(p_mat[i] > 0.05) {
    star_mat[i] = 'ns'
  }
  else if( (p_mat[i] < 0.05) & (p_mat[i] > 0.01) ) { 
    star_mat[i] = '*'
  }
  else if( (p_mat[i] < 0.01) & (p_mat[i] > 0.001) ) { 
    star_mat[i] = '**'
  }
  else if(p_mat[i] < 0.001) { 
    star_mat[i] = '***'
  }
}

####### GROUP DIFFERENCES, BY RESPONSE: ORIGINAL - STRANGER

mean(iden_ordered$att_diff[iden_ordered$after==1])
sd(iden_ordered$att_diff[iden_ordered$after==1])

mean(iden_ordered$att_diff[iden_ordered$after==4])
sd(iden_ordered$att_diff[iden_ordered$after==4])

att_2 <- t.test(iden_ordered$att_diff[iden_ordered$aft==1 | iden_ordered$aft==4] ~ iden_ordered$aft[iden_ordered$aft==1 | iden_ordered$aft==4], var.equal=TRUE, paired=FALSE); att_2

####### GROUP DIFFERENCES, BY RESPONSE: OVERALL PERFORMANCE

mean(iden_ordered$total_perf[iden_ordered$aft==1])
sd(iden_ordered$total_perf[iden_ordered$aft==1])
n_o_all <- length(iden_ordered$total_perf[iden_ordered$aft==1])

mean(iden_ordered$total_perf[iden_ordered$aft==4])
sd(iden_ordered$total_perf[iden_ordered$aft==4])
n_b_all <- length(iden_ordered$total_perf[iden_ordered$aft==4])

att_2 <- t.test(iden_ordered$total_perf[iden_ordered$aft==1 | iden_ordered$aft==4] ~ iden_ordered$aft[iden_ordered$aft==1 | iden_ordered$aft==4], var.equal=TRUE, paired=FALSE); att_2
tes(as.numeric(att_2[1]), n_o_all, n_b_all) #cohen's d

##================================================================================================================
                                                        ##PLOT##
##================================================================================================================

#sort identity mat by original - copy, for plotting
iden_ordered_perf <- iden.mat[order(-iden.mat$total_perf),]
iden_ordered_perf$counter <- c(1:length(workers))
iden_ordered_perf

#subset identity mat by whether participants say 1 (original) or 4 (both)
iden_ordered_original <- subset(iden_ordered,(iden_ordered$after == 1))
iden_ordered_both <- subset(iden_ordered,(iden_ordered$after == 4))

#subset identity mat by whether participants say 1 (original) or 4 (both)
iden_ordered_perf_original <- subset(iden_ordered_perf,(iden_ordered_perf$after == 1))
iden_ordered_perf_both <- subset(iden_ordered_perf,(iden_ordered_perf$after == 4))

# create matrix to plot self - original difference group effect for those who said original, copy, and both
diff_mat <- array(0,dim=c(2,5))
colnames(diff_mat) <- c('cond','mean','sd','n','sem')
diff.mat <- as.data.frame(diff_mat, stringsAsFactors=FALSE); diff.mat

diff.mat[1,] <- c(1,mean(iden_ordered$att_diff[iden_ordered$after==1]),sd(iden_ordered$att_diff[iden_ordered$after==1]),length(iden_ordered$att_diff[iden_ordered$after==1]),0)
diff.mat[1,5] <- diff.mat[1,3]/sqrt(diff.mat[1,4])

diff.mat[2,] <- c(2,mean(iden_ordered$att_diff[iden_ordered$after==4]),sd(iden_ordered$att_diff[iden_ordered$after==4]),length(iden_ordered$att_diff[iden_ordered$after==4]),0)
diff.mat[2,5] <- diff.mat[2,3]/sqrt(diff.mat[2,4])

# create matrix to plot total performance for those who said original, copy, and both
total_perf_mat <- array(0,dim=c(2,5))
colnames(total_perf_mat) <- c('cond','mean','sd','n','sem')
total_perf.mat <- as.data.frame(total_perf_mat, stringsAsFactors=FALSE); total_perf.mat

total_perf.mat[1,] <- c(1,mean(iden_ordered$total_perf[iden_ordered$after==1]),sd(iden_ordered$total_perf[iden_ordered$after==1]),length(iden_ordered$total_perf[iden_ordered$after==1]),0)
total_perf.mat[1,5] <- total_perf.mat[1,3]/sqrt(total_perf.mat[1,4])

total_perf.mat[2,] <- c(2,mean(iden_ordered$total_perf[iden_ordered$after==4]),sd(iden_ordered$total_perf[iden_ordered$after==4]),length(iden_ordered$total_perf[iden_ordered$after==4]),0)
total_perf.mat[2,5] <- total_perf.mat[2,3]/sqrt(total_perf.mat[2,4])

#make d mat for plotting: identified with original
d_mat_plot_o <- array(0,dim=c(3, 5))
colnames(d_mat_plot_o) <- c('cond','mean','sd','n','sem')

for(i in 1:length(unique(agentCond_n))) {
  d_mat_plot_o[i, ] <- c(i, mean(d.mat_o$d[d.mat_o$agentCond == i]), sd(d.mat_o$d[d.mat_o$agentCond == i]), length(d.mat_o$d[d.mat_o$agentCond == i]), 0)
  d_mat_plot_o[i, 5] <- d_mat_plot_o[i,3]/sqrt(d_mat_plot_o[i,4])
}
d.plot_o <- as.data.frame(d_mat_plot_o, stringsAsFactors=FALSE); d_mat_plot_o

#make d mat for plotting: identified with both
d_mat_plot_b <- array(0,dim=c(3, 5))
colnames(d_mat_plot_b) <- c('cond','mean','sd','n','sem')

for(i in 1:length(unique(agentCond_n))) {
  d_mat_plot_b[i, ] <- c(i, mean(d.mat_b$d[d.mat_b$agentCond == i]), sd(d.mat_b$d[d.mat_b$agentCond == i]), length(d.mat_b$d[d.mat_b$agentCond == i]), 0)
  d_mat_plot_b[i, 5] <- d_mat_plot_b[i,3]/sqrt(d_mat_plot_b[i,4])
}
d.plot_b <- as.data.frame(d_mat_plot_b, stringsAsFactors=FALSE); d_mat_plot_b

### REVIVAL EFFECT
#prepare data for plotting
tab <- matrix(NA,2,4)
colnames(tab) <- c('original', 'copy', 'neither', 'both')
rownames(tab) <- c('before', 'after')
tab[1,] <- c(length(identity[identity == 1 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 2 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 3 & cond == 1])/length(identity[cond == 1]), length(identity[identity == 4 & cond == 1])/length(identity[cond == 1]) )
tab[2,] <- c(length(identity[identity == 1 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 2 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 3 & cond == 2])/length(identity[cond == 2]), length(identity[identity == 4 & cond == 2])/length(identity[cond == 2]) )
tab

#plot barplot
tab_transpose <- t(tab)
barplot(tab_transpose, main="Who are you?", xlab = "Condition", ylab= "Proportion", beside=TRUE, legend = rownames(tab_transpose))

################## GROUPED PLOTS ##################

#1st plot
p1.11<-ggplot(d.plot_o,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()

p1.111<-p1.11+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+
  geom_signif(data=d.plot_o,
              aes(xmin=1, xmax=2, annotations=star_mat[1], y_position=2.5),
              textsize = 5, vjust = 0.0,
              manual=TRUE) +
  ggtitle ("Identified with Original") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))

p1.1111<-p1.111+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

#######4th plot
p1.14<-ggplot(d.plot_b,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.114<-p1.14+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+
  geom_signif(data=d.plot_o,
              aes(xmin=1, xmax=2, annotations=star_mat[4], y_position=2.5),
              textsize = 5, vjust = 0.0,
              manual=TRUE) +
  ggtitle ("Identified with Both") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1114<-p1.114+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

#quartz()
figure<-ggarrange(p1.1111,p1.1114, nrow=1,ncol=2,common.legend = TRUE, legend="top") 
annotate_figure(figure,top = NULL,left = text_grob("d' Performance", color="black", face ="bold",size=20)) 

################## GROUP ORIGINAL - COPY DIFFERENCE 
condNames <- c('Original', 'Both')

#quartz()
p1<-ggplot(diff.mat, aes(x=factor(cond),y=mean))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(0,1))+
  theme_classic() 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge",width = 0.5)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("Version Identified With")+ylab("d' Difference (Original - Copy)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

############## TOTAL PERFORMANCE
#quartz()
condNames <- c('Original', 'Both')

p2<-ggplot(total_perf.mat, aes(x=factor(cond),y=mean))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(0,9))+
  theme_classic() 
p2+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge",width = 0.5)+
  geom_signif(data=d.plot_o,
              aes(xmin=1, xmax=2, annotations='ns', y_position=7.5),
              textsize = 8, vjust = 0.0,
              manual=TRUE) +
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(legend.key=element_blank(), legend.box="horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust=1,vjust=0.8))+
  xlab("Version Identified With")+ylab("d' Total (Original+Copy+Stranger)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

figure<-ggarrange(p1,p2, nrow=1,ncol=2,common.legend = TRUE, legend="top") 
annotate_figure(figure,top = NULL,left = text_grob("d' Performance", color="black", face ="bold",size=20)) 

###### INDIVIDUAL SUBJECTS, ORDERED BY ORIGINAL - COPY DIFFERENCE ##################

#Identified with original
p1.11<-ggplot(iden_ordered_original,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()

p1.111<-p1.11+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with original") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))

p1.1111<-p1.111+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

#######Identified with copy
p1.14<-ggplot(iden_ordered_both,aes(x=factor(counter),y=att_diff)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(-2, 3))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.114<-p1.14+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Both") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1114<-p1.114+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

figure<-ggarrange(p1.1111,p1.1114, nrow=1,ncol=2,common.legend = TRUE, legend="top") 
annotate_figure(figure,top = NULL,left = text_grob("d' Original-Copy ", color="black", face ="bold",size=20)) 


###### INDIVIDUAL SUBJECTS, ORDERED BY TOTAL PERFORMANCE ##################

#Identified with original
p1.11<-ggplot(iden_ordered_perf_original,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 12))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()

p1.111<-p1.11+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with original") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))

p1.1111<-p1.111+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 14))

#######Identified with copy
p1.14<-ggplot(iden_ordered_perf_both,aes(x=factor(counter),y=total_perf)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 12))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.114<-p1.14+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "18", face = "plain")) +
  ggtitle ("Identified with Both") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.title = element_text(size = 18))
p1.1114<-p1.114+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

figure<-ggarrange(p1.1111,p1.1114, nrow=1,ncol=2,common.legend = TRUE, legend="top") 
annotate_figure(figure,top = NULL,left = text_grob("d' Original-Copy ", color="black", face ="bold",size=20)) 

##================================================================================================================
                                                        ##END##
##================================================================================================================




















