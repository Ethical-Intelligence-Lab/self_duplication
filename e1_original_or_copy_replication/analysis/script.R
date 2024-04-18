#====================================================================================
# Julian De Freitas, 2019
# Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
# Experiment S1
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

nrow(data)

## Attention Checks
data |>
  filter(trialStruct.attention == 0) -> data

## Number of Participants
nrow(data)

## Comprehension Checks
data |>
  filter(
      trialStruct.comp_number_copies == 2,
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
    both = ifelse(identity == 4, 1, 0),
    neither = ifelse(identity == 3, 1, 0)
  ) -> data

## Demographics
mean(data$age)
table(data$sex)

#====================================================================================
#                               ANALYSIS
#====================================================================================

data |>
  select(copy, original, both, neither, persp, living) -> d

# Sample size of each condition
table(d$persp, d$living) 

d |>
  gather(key = "identity", value = "flag", copy, original, both, neither) |>
  filter(flag == 1) |>
  mutate(
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
## Identify with copy when perspective is first
mean(d[d$persp == "first",]$both) * 100
## vs third
mean(d[d$persp == "third",]$both) * 100

### Beta Coefficient
s$coefficients[ "both" , "perspfirst" ]
### Standard Error
s$standard.errors[ "both" , "perspfirst" ]
### p-value
p[ "both" , "perspfirst" ]

## (4) Identifying as BOTH based on IV (Dead v. Alive)
## Identify with both when original is dead
mean(d[d$living == "dead",]$both) * 100
## vs Alive
mean(d[d$living == "alive",]$both) * 100

### Beta Coefficient
s$coefficients[ "both" , "livingdead" ]
### Standard Error
s$standard.errors[ "both" , "livingdead" ]
### p-value
p[ "both" , "livingdead" ]

## (5) Identifying as NEITHER based on IV (First v. Third)
## Identify with both when perspective is first
mean(d[d$persp == "first",]$neither) * 100
## vs third
mean(d[d$persp == "third",]$neither) * 100

### Beta Coefficient
s$coefficients[ "neither" , "perspfirst" ]
### Standard Error
s$standard.errors[ "neither" , "perspfirst" ]
### p-value
p[ "neither" , "perspfirst" ]

## [Not Significant]
## (5) Identifying as NEITHER based on IV (Dead v. Alive) 
## Identify with neither when original is dead
mean(d[d$living == "dead",]$neither) * 100
## vs Alive
mean(d[d$living == "alive",]$neither) * 100

### Beta Coefficient
s$coefficients[ "neither" , "livingdead" ]
### Standard Error
s$standard.errors[ "neither" , "livingdead" ]
### p-value
p[ "neither" , "livingdead" ]

## Notable Difference
### identifies as both
mean(d[d$living == "alive" & d$persp == "first",]$both) * 100
### identifies as original
mean(d[d$living == "alive" & d$persp == "first",]$original) * 100


#====================================================================================
#                               VISUALIZATION
#====================================================================================

data |>
  ungroup() |>
  mutate(cond = paste(living, persp, sep = "-"),
         cond = as.factor(case_when(
           cond == "alive-third" ~ 1,
           cond == "alive-first" ~ 2,
           cond == "dead-third" ~ 3,
           cond == "dead-first" ~ 4,
         ))) |>
  select(cond, original, copy, neither, both) |>
  group_by(cond) |>
  summarize_all(
    mean
  ) |>
  gather(key = "answer", value = "mean", original, copy, neither, both) |>
  mutate(
    answer = as.factor(case_when(
      answer == "original" ~ 1,
      answer == "copy" ~ 2,
      answer == "neither" ~ 3,
      answer == "both" ~ 4,
    )))-> d_plot

theme_update(plot.title = element_text(hjust = 0.5))

x_scale_labels <- c("Alive-Third P", "Alive-First P", "Dead-Third P", "Dead-First P")

ggplot(d_plot, aes(x = cond ,y = mean, fill = factor(answer)))+
  stat_summary(fun = mean, position = position_dodge(), geom = "bar", width = 0.5) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(axis.title.x = element_blank()) + 
  theme_classic() +
  scale_x_discrete(breaks = 1:4, labels = x_scale_labels) +
  theme(legend.key = element_blank(), legend.box = "horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1,vjust = 0.8))+
  ylab("Proportion of Identifications")+
  xlab("") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_fill_manual(values = c("aquamarine3", "tomato3","darkcyan", "tomato4"), name = "", 
                    labels = c('Original', 'Copy', 'Neither', 'Both'))
