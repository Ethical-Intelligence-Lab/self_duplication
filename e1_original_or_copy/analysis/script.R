#====================================================================================
# Julian De Freitas, 2019
# Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
# Experiment 1
#====================================================================================

## clear workspace
rm(list = ls())  

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('rjson',       
               'tidyverse',
               'ltm', 
               'heplots',            
               'lmtest',        
               'compute.es',        
               'ltm',           
               'lsr',      
               'lsmeans',        
               'nnet',          
               'mlogit'
)

#====================================================================================
#                             PRE-PROCESSING
#====================================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Set working directory to ../data/
setwd("../data/")

data <- do.call(rbind, 
                lapply(list.files(pattern=('*txt')), 
                function(x) as.data.frame(fromJSON(fil =x)))) 

## Attention Checks
data |>
  filter(trialStruct.attention == 0 & trialStruct.comp_number_copies == 2) -> data

## Number of Participants
nrow(data)

## Comprehension Checks
data |>
  filter(
      (trialStruct.cond_num == 1 & trialStruct.comp_original_you == 1) |
      (trialStruct.cond_num == 2 & trialStruct.comp_original_you == 1) |
      (trialStruct.cond_num == 3 & trialStruct.comp_original_you == 2) |
      (trialStruct.cond_num == 4 & trialStruct.comp_original_you == 2)
  ) -> data

## Final Sample
nrow(data)

## Clean column names
colnames(data) <- gsub("trialStruct.", "", colnames(data))

data |>
  select(original_cond, perspective_cond, identity, age, sex) -> data

colnames(data) <- c("living", "persp", "identity", "age", "sex")

data |>
  mutate(
    copy = ifelse(identity == 2, 1, 0),
    original = ifelse(identity == 1, 1, 0),
    both = ifelse(identity == 3, 1, 0),
    neither = ifelse(identity == 4, 1, 0)
  ) -> data

## Demographics
mean(data$age)
table(data$sex)

#====================================================================================
#                               ANALYSIS
#====================================================================================

data |>
  select(copy, original, both, neither, persp, living) -> d

table(d$persp, d$living) 

# For third and living 
## Identify as ORIGINAL
mean(d[d$persp == "third" & d$living == "alive",]$original)
## Identify as COPY
mean(d[d$persp == "third" & d$living == "alive",]$copy)
## Identify as BOTH
mean(d[d$persp == "third" & d$living == "alive",]$both)
## Identify as NEITHER
mean(d[d$persp == "third" & d$living == "alive",]$neither)


d |>
  gather(key = "identity", value = "flag", original, copy, both, neither) |>
  filter(flag == 1) |>
  mutate(
    identity = gsub("", "", identity),
    identity_dummy = relevel(as.factor(identity), ref = "original"),
    persp = relevel(as.factor(persp), ref = "third"),
    living = relevel(as.factor(living), ref = "alive")
  ) -> d_reg

## Regressions
reg1 <- multinom(identity_dummy ~ persp + living, data = d_reg)
s <- summary(reg1)
z <- s$coefficients/s$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

## (1) Identifying as COPY based on IV (Dead v. Alive)
## Identify with copy when original is dead
mean(d[d$living == "dead",]$copy) * 100
## vs Alive
mean(d[d$living == "alive",]$copy) * 100

### Beta Coefficient
s$coefficients[ "copy" , "livingdead" ]
### Standard Error
s$standard.errors[ "copy" , "livingdead" ]
### p-value
p[ "copy" , "livingdead" ]

## (2) Identifying as COPY based on IV (First v. Third)
## Identify with copy when perspective is first
mean(d[d$persp == "first",]$copy) * 100
## vs third
mean(d[d$persp == "third",]$copy) * 100

### Beta Coefficient
s$coefficients[ "copy" , "perspfirst" ]
### Standard Error
s$standard.errors[ "copy" , "perspfirst" ]
### p-value
p[ "copy" , "perspfirst" ]

## (3) Identifying as BOTH based on IV (First v. Third)
## Identify with both when perspective is first
mean(d[d$persp == "first",]$both) * 100
## vs third
mean(d[d$persp == "third",]$both) * 100

### Beta Coefficient
s$coefficients[ "both" , "perspfirst" ]
### Standard Error
s$standard.errors[ "both" , "perspfirst" ]
### p-value
p[ "both" , "perspfirst" ]

## OTHER COEFFICIENTS not significant
p > 0.09

#====================================================================================
#                               VISUALIZATION
#====================================================================================

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