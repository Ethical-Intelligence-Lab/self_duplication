#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The capacity of personal identity
#Experiment 10

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
dir <- setwd("/Users/julian/Dropbox (Personal)/Research/Intuition/single_identity/e10_memory_duplicates/data")

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
length(unique(data$workerId)) #202 subjects

#check that we have equal numbers for each condition
table(data$label)
table(data$agentCond)
table(data$selfCond)

##================================================================================================================
                                                ##EXCLUSIONS##
##================================================================================================================

## (1) attention and comprehension
data <- subset(data,(data$attentionMCQ=="0" & data$comp_original_you == 3 
                     & data$comp_num_copies == 2 & data$comp=="B")) 
length(unique(data$workerId)) #1 subjects

data$rt[is.na(data$rt)] <- 0

## (2) < 55 accuracy or RTs < 100ms
#mark which trials had reasonable rts
for(i in 1:dim(data)[1]) {
  if(data$rt[i] >= 100) {
    data$badRt[i] = 0
  }
  else {
    data$badRt[i] = 1
  }
}

worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
perf_thresh <- 0.40
cond <- as.factor(data$selfCond)
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

#check it worked
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0

ss_excl_mat <- array(0,dim=c(length(workers),2))
colnames(ss_excl_mat) <- c('mean_acc', 'rt_err_prop')

for(i in 1:length(workers)) {
  ss_excl_mat[i,1] <- c(mean(acc_use[worker==workers[i]])) #get accuracy for each worker
  ss_excl_mat[i,2] <- sum(data$badRt[data$workerId == workers[i]])/length(data$rt[data$workerId == workers[i]])
}
ss_excl_mat

length(unique(data$workerId)) #120 subjects
dim(data)

##================================================================================================================
                                             ##PREP DATA FOR ANALYSIS##
##================================================================================================================

### get identity ratings for each subject
worker <- as.factor(data$workerId)
workers <- as.factor(unique(worker))
identity_bef <- data$identity_before
identity_aft <- data$identity_after
iden_mat <- array(0,dim=c(length(workers),5))
colnames(iden_mat) <- c('before', 'after','zeros','ltm_diff', 'total_perf')
for(i in 1:length(workers)) {
  iden_mat[i,] <- c( mean(identity_bef[worker==workers[i]]), mean(identity_aft[worker==workers[i]]),0, 0, 0)
}
iden.mat <- as.data.frame(iden_mat, stringsAsFactors=FALSE); iden.mat

#assign variable names
acc <- as.numeric(data$acc)
acc_use <- acc
acc_use[is.na(acc_use)] <- 0
numTrials <- dim(data)[1]
matchCond <- as.factor(data$matchCond)
agentCond <- as.factor(data$agentCond)
condNum <- as.factor(data$selfCond)

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
age <- as.numeric(data$age); mean(age,na.rm = TRUE) #32.10
gender <- as.factor(data$sex); table(gender)[1]/sum(table(gender)) #0.51

#create matrix for collecting d prime-relevant values, and rts
d_mat <- array(0,dim=c(3*length(workers), 5))
colnames(d_mat) <- c('worker', 'mainCond', 'agentCond', 'acc', 'rt')
d.mat <- as.data.frame(d_mat, stringsAsFactors=FALSE); d.mat

#for each worker and agent condition, get average hits, false alarms, d-prime, and rt
#hits: proportion of true hits you said were hits (out of all true hits)
#false alarms: proportin of non hits you said were hits (out of all non hits)
#get rts for trials that were correct

corr_answers <- c('original', 'copy', 'stranger')

counter <- 1
for(i in 1:length(workers)) {
  for(j in 1:length(agentConds_n)) {
    mean_acc <- mean(acc_use[worker==workers[i] & agentCond_n==j])
    rt <- mean(rts[worker==workers[i] & agentCond_n==j], na.rm=TRUE)
    
    d.mat[counter,] <- c(workers[i], unique(identity_aft[worker==workers[i]]), j, mean_acc, rt)
    counter = counter + 1
  }
} 

# collect total performance and performance difference (cond 1 - 2) for each subject
perf_mat <- array(0, dim=c(length(workers), 3))
colnames(perf_mat) <- c('mainCond', 'perf_diff', 'total_perf')
perf.mat <- as.data.frame(perf_mat, stringsAsFactors=FALSE); perf.mat

for(i in 1:length(workers)) {
  perf.mat[i,1] <- unique(d.mat$mainCond[d.mat$worker==i])
  if(unique(d.mat$mainCond[d.mat$worker==i]) == 1) {
    #performance for original you - performance for stranger John
    perf.mat[i,2] <- d.mat$acc[d.mat$worker==i & d.mat$agentCond == 1] - d.mat$acc[d.mat$worker==i & d.mat$agentCond == 2] 
  }
  else if(unique(d.mat$mainCond[d.mat$worker==i]) == 4) {
    #best of performance for original or copy - performance for stranger John
    perf.mat[i,2] <- max(d.mat$acc[d.mat$worker==i & d.mat$agentCond == 1], d.mat$acc[d.mat$worker==i & d.mat$agentCond == 2]) - d.mat$acc[d.mat$worker==i & d.mat$agentCond == 3] 
  }
  perf.mat[i,3] <- d.mat$acc[d.mat$worker==i & d.mat$agentCond == 1] + d.mat$acc[d.mat$worker==i & d.mat$agentCond == 2] + d.mat$acc[d.mat$worker==i & d.mat$agentCond == 3]
}

##================================================================================================================
                                                      ##ANALYSIS##
##================================================================================================================

#------- GROUP MEANS--------#
#We only look at conditions in which participants said they were original or both
#Since the other conditions (saying they're the copy or neither) don't have enough subjects
p_mat <- rep(9, times = 7)
star_mat <- rep(9, times = 7)

#one self 
d_one <- subset(d.mat, d.mat$mainCond==1)

mean(d_one$acc[d_one$agentCond==1]) #future-you1
sd(d_one$acc[d_one$agentCond==1])
n_o_1 <- length(d_one$acc[d_one$agentCond==1])

mean(d_one$acc[d_one$agentCond==2]) #stranger-john
sd(d_one$acc[d_one$agentCond==2])
n_o_2 <- length(d_one$acc[d_one$agentCond==2])

mean(d_one$acc[d_one$agentCond==3]) #stranger-bill
sd(d_one$acc[d_one$agentCond==3])
n_o_3 <- length(d_one$acc[d_one$agentCond==3])

att_1_o <- t.test(d_one$acc[d_one$agentCond==1 | d_one$agentCond==2] ~ d_one$agentCond[d_one$agentCond==1 | d_one$agentCond==2], var.equal=TRUE, paired=TRUE); att_1_o
att_2_o <- t.test(d_one$acc[d_one$agentCond==1 | d_one$agentCond==3] ~ d_one$agentCond[d_one$agentCond==1 | d_one$agentCond==3], var.equal=TRUE, paired=TRUE); att_2_o
att_3_o <- t.test(d_one$acc[d_one$agentCond==2 | d_one$agentCond==3] ~ d_one$agentCond[d_one$agentCond==2 | d_one$agentCond==3], var.equal=TRUE, paired=TRUE); att_3_o

tes(as.numeric(att_1_o[1]), n_o_1, n_o_2) #cohen's d
tes(as.numeric(att_2_o[1]), n_o_1, n_o_3) #cohen's d
tes(as.numeric(att_3_o[1]), n_o_2, n_o_3) #cohen's d

#two selves
d_two <- subset(d.mat, d.mat$mainCond==4) 

mean(d_two$acc[d_two$agentCond==1]) #original
sd(d_two$acc[d_two$agentCond==1])
n_b_1 <- length(d_two$acc[d_two$agentCond==1])

mean(d_two$acc[d_two$agentCond==2]) #copy
sd(d_two$acc[d_two$agentCond==2])
n_b_2 <- length(d_two$acc[d_two$agentCond==2])

mean(d_two$acc[d_two$agentCond==3]) #stranger
sd(d_two$acc[d_two$agentCond==3])
n_b_3 <- length(d_two$acc[d_two$agentCond==3])

att_1_b <- t.test(d_two$acc[d_two$agentCond==1 | d_two$agentCond==2] ~ d_two$agentCond[d_two$agentCond==1 | d_two$agentCond==2], var.equal=TRUE, paired=TRUE); att_1_b
att_2_b <- t.test(d_two$acc[d_two$agentCond==1 | d_two$agentCond==3] ~ d_two$agentCond[d_two$agentCond==1 | d_two$agentCond==3], var.equal=TRUE, paired=TRUE); att_2_b
att_3_b <- t.test(d_two$acc[d_two$agentCond==2 | d_two$agentCond==3] ~ d_two$agentCond[d_two$agentCond==2 | d_two$agentCond==3], var.equal=TRUE, paired=TRUE); att_3_b

tes(as.numeric(att_1_b[1]), n_b_1, n_b_2) #cohen's d
tes(as.numeric(att_2_b[1]), n_b_1, n_b_3) #cohen's d
tes(as.numeric(att_3_b[1]), n_b_2, n_b_3) #cohen's d

#------- TOTAL PERFORMANCE--------#
#summed performance across three agent conditions

mean(perf.mat$total_perf[perf.mat$mainCond == 1])
sd(perf.mat$total_perf[perf.mat$mainCond == 1])
total_n_o <- length(perf.mat$total_perf[perf.mat$mainCond == 1])

mean(perf.mat$total_perf[perf.mat$mainCond == 4])
sd(perf.mat$total_perf[perf.mat$mainCond == 4])
total_n_b <- length(perf.mat$total_perf[perf.mat$mainCond == 4])

perf_1 <- t.test(perf.mat$total_perf[perf.mat$mainCond==1 | perf.mat$mainCond == 4] ~ perf.mat$mainCond[perf.mat$mainCond==1 | perf.mat$mainCond == 4], var.equal=TRUE, paired=FALSE); perf_1
tes(as.numeric(perf_1[1]), total_n_o, total_n_b) #cohen's d

p_mat <- c(att_1_o[3], att_2_o[3], att_3_o[3], att_1_b[3], att_2_b[3], att_3_b[3], perf_1[3])
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

#-------PERFORMANCE DIFF --------#
#performance of original - stranger John (self cond 1)
#or best performanc of original or copy - stranger John (self cond 2)

mean(perf.mat$perf_diff[perf.mat$mainCond == 1])
sd(perf.mat$perf_diff[perf.mat$mainCond == 1])
diff_n_o <- length(perf.mat$perf_diff[perf.mat$mainCond == 1])

mean(perf.mat$perf_diff[perf.mat$mainCond == 4])
sd(perf.mat$perf_diff[perf.mat$mainCond == 4])
diff_n_b <- length(perf.mat$perf_diff[perf.mat$mainCond == 4])

perf_2 <- t.test(perf.mat$perf_diff[perf.mat$mainCond==1 | perf.mat$mainCond == 4] ~ perf.mat$mainCond[perf.mat$mainCond==1 | perf.mat$mainCond == 4], var.equal=TRUE, paired=FALSE); perf_2
tes(as.numeric(perf_2[1]), diff_n_o, diff_n_b) #cohen's d

##================================================================================================================
                                             ##PREPARE DATA FOR PLOTTING##
##================================================================================================================

#make d mat for plotting: self condition 1
d_mat_plot_one <- array(0,dim=c(3, 5))
colnames(d_mat_plot_one) <- c('cond','mean','sd','n','sem')

for(i in 1:length(unique(agentCond_n))) {
  d_mat_plot_one[i, ] <- c(i, mean(d_one$acc[d_one$agentCond == i]), sd(d_one$acc[d_one$agentCond == i]), length(d_one$acc[d_one$agentCond == i]), 0)
  d_mat_plot_one[i, 5] <- d_mat_plot_one[i,3]/sqrt(d_mat_plot_one[i,4])
}
d_mat_plot_one
d.plot_o <- as.data.frame(d_mat_plot_one, stringsAsFactors=FALSE); d.one

#make d mat for plotting: self condition 2
d_mat_plot_two <- array(0,dim=c(3, 5))
colnames(d_mat_plot_two) <- c('cond','mean','sd','n','sem')

for(i in 1:length(unique(agentCond_n))) {
  d_mat_plot_two[i, ] <- c(i, mean(d_two$acc[d_two$agentCond == i]), sd(d_two$acc[d_two$agentCond == i]), length(d_two$acc[d_two$agentCond == i]), 0)
  d_mat_plot_two[i, 5] <- d_mat_plot_two[i,3]/sqrt(d_mat_plot_two[i,4])
}
d_mat_plot_two
d.plot_b <- as.data.frame(d_mat_plot_two, stringsAsFactors=FALSE); d.two

# make total performance mat for plotting
perf_mat_total <- array(0,dim=c(2, 5))
colnames(perf_mat_total) <- c('cond','mean','sd','n','sem')
total_perf.mat <- as.data.frame(perf_mat_total, stringsAsFactors=FALSE); perf_mat_total

total_perf.mat[1, ] <- c(1, mean(perf.mat$total_perf[perf.mat$mainCond == 1]), sd(perf.mat$total_perf[perf.mat$mainCond == 1]), length(perf.mat$total_perf[perf.mat$mainCond == 1]), 0)
total_perf.mat[1,5] <- total_perf.mat[1,3]/sqrt(total_perf.mat[1,4])

total_perf.mat[2, ] <- c(2, mean(perf.mat$total_perf[perf.mat$mainCond == 4]), sd(perf.mat$total_perf[perf.mat$mainCond == 4]), length(perf.mat$total_perf[perf.mat$mainCond == 4]), 0)
total_perf.mat[2,5] <- total_perf.mat[2,3]/sqrt(total_perf.mat[2,4])

total_perf.mat

# make performance diff mat for plotting
perf_mat_diff <- array(0,dim=c(2, 5))
colnames(perf_mat_diff) <- c('cond','mean','sd','n','sem')
perf_mat_diff <- as.data.frame(perf_mat_diff, stringsAsFactors=FALSE); perf_mat_diff

perf_mat_diff[1, ] <- c(1, mean(perf.mat$perf_diff[perf.mat$mainCond == 1]), sd(perf.mat$perf_diff[perf.mat$mainCond == 1]), length(perf.mat$perf_diff[perf.mat$mainCond == 1]), 0)
perf_mat_diff[1,5] <- perf_mat_diff[1,3]/sqrt(perf_mat_diff[1,4])

perf_mat_diff[2, ] <- c(2, mean(perf.mat$perf_diff[perf.mat$mainCond == 4]), sd(perf.mat$perf_diff[perf.mat$mainCond == 4]), length(perf.mat$perf_diff[perf.mat$mainCond == 4]), 0)
perf_mat_diff[2,5] <- perf_mat_diff[2,3]/sqrt(perf_mat_diff[2,4])

perf_mat_diff

# make ordered performance diff mat for plotting
perf_one <- subset(perf.mat,perf.mat$mainCond==1)
perf_one_diff_ordered <- perf_one[order(-perf_one$perf_diff),]
perf_one_diff_ordered$counter <- c(1:dim(perf_one)[1])
perf_one_total_ordered <- perf_one[order(-perf_one$total_perf),]
perf_one_total_ordered$counter <- c(1:dim(perf_one)[1])
  
perf_two <- subset(perf.mat,perf.mat$mainCond==4)
perf_two_diff_ordered <- perf_two[order(-perf_two$perf_diff),]
perf_two_diff_ordered$counter <- c(1:dim(perf_two)[1])
perf_two_total_ordered <- perf_two[order(-perf_two$total_perf),]
perf_two_total_ordered$counter <- c(1:dim(perf_two)[1])

##================================================================================================================
                                            ##PLOT DATA##
##================================================================================================================

#identified with original
p1.11<-ggplot(d.plot_o,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 1))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()

p1.111<-p1.11+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  theme(legend.text = element_text(size = "13", face = "plain"),
        legend.margin=margin(0,0,0,0),legend.box.margin=margin(15,15,15,15)) +
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+
  geom_signif(data=d.plot_o,
              aes(xmin=1, xmax=2, annotations=star_mat[1], y_position=0.70),
              textsize = 10, vjust = 0.3,
              manual=TRUE) +
  ggtitle ("Identified with Original") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size = 26))

p1.1111<-p1.111+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size = 24))

#identified with both
p1.14<-ggplot(d.plot_b,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 1))+
  theme(axis.title.y = element_blank())+ 
  theme(axis.title.x = element_blank())+ 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()
p1.114<-p1.14+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("")+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+
  geom_signif(data=d.plot_o,
              aes(xmin=1, xmax=2, annotations=star_mat[4], y_position=0.70),
              textsize = 8, vjust = -0.4,
              manual=TRUE) +
  ggtitle ("Identified with Both") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size = 26))
p1.1114<-p1.114+theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())

#total performance
p1.15<-ggplot(total_perf.mat,aes(x=factor(cond),y=mean)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",face="bold")+
  coord_cartesian(ylim=c(0, 2))+
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme_classic()

p1.115<-p1.15+scale_fill_discrete(name = "", labels = c ("Original", "Copy", "Stranger")) +
  xlab ("") + ylab ("") +
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+
  geom_signif(data=d.plot_o,
              aes(xmin=1, xmax=2, annotations=star_mat[7], y_position=1.8),
              textsize = 8, vjust = -0.4,
              manual=TRUE) +
  ggtitle ("Total Performance:\n Identified with Original v. Both") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size = 22))+
  theme(axis.text.y = element_text(size = 24))

p1.1115<-p1.115+ theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())

quartz()
figure<-ggarrange(p1.1111,p1.1114,p1.1115, nrow=1,ncol=3,common.legend = TRUE, legend='top') 
annotate_figure(figure,top = NULL,left = text_grob("Mean Accuracy", color="black",size=27, rot=90)) 

##================================================================================================================
                                                        ##END##
##================================================================================================================




















