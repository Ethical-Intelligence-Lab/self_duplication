#====================================================================================
# Julian De Freitas, 2019
# Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
# Experiment 2
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
## Num recruited
nrow(data)

data |>
  filter(trialStruct.attention == 0 & 
         trialStruct.comp_original_you == 2 & 
         trialStruct.comp_number_copies == 2
    ) -> data

## Final Sample
nrow(data)

## Age
mean(data$trialStruct.age)
## Sex
table(data$trialStruct.sex)

data |>
  select(trialStruct.identity, trialStruct.perspective_cond ) -> data

colnames(data) <- c("identity", "persp")

#====================================================================================
#                         IMPORTING CONTROL FROM E1
#====================================================================================
# Set wd to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set working directory to ../e1_original_or_copy/data/
setwd("../../e1_original_or_copy/data/")

e1 <- do.call(rbind, 
                lapply(list.files(pattern=('*txt')), 
                       function(x) as.data.frame(fromJSON(fil =x)))) 

e1 |>
  filter(trialStruct.cond_num == 3, 
         trialStruct.comp_original_you == 2,
         trialStruct.attention == 0,
         trialStruct.comp_number_copies == 2) |>
  select( trialStruct.identity ) |>
  mutate( persp = "third" ) -> e1

colnames(e1) <- colnames(data)

data <- rbind(data, e1)

#====================================================================================
#                               ANALYSIS
#====================================================================================

data |>
  mutate(
    persp = case_when(
      persp == "third" ~ 1,
      persp == "empathy" ~ 2,
      persp == "self" ~ 3,
      persp == "full" ~ 4
    ),
    identity = as.factor(identity),
    copy = ifelse(identity == 2, 1, 0),
    both = ifelse(identity == 4, 1, 0),
    original = ifelse(identity == 1, 1, 0),
    neither = ifelse(identity == 3, 1, 0),
  ) -> d

## Regressions
reg1 <- multinom(identity ~ persp, data = d)
s <- summary(reg1)
z <- s$coefficients/s$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

## Percentage that chose COPY per condition
prop.table(table(d$persp, d$copy), margin = 1)[,2] * 100

### Beta Coefficient
s$coefficients[ "2" , "persp" ]
### Standard Error
s$standard.errors[ "2" , "persp" ]
### p-value
p[ "2" , "persp" ]

## Percentage that chose BOTH per condition
prop.table(table(d$persp, d$both), margin = 1)[,2] * 100

### Beta Coefficient
s$coefficients[ "4" , "persp" ]
### Standard Error
s$standard.errors[ "4" , "persp" ]
### p-value
p[ "4" , "persp" ]

#====================================================================================
#                               VISUALIZATION
#====================================================================================

d |>
  ungroup() |>
  mutate(cond = as.factor(persp)) |>
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
    ))) -> d_plot

theme_update(plot.title = element_text(hjust = 0.5))

x_scale_labels <- c("Third Person", "Perspective Manip", "Feels the same", "Feels continuous")

ggplot(d_plot, aes(x = cond ,y = mean, fill = factor(answer)))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5)+
  theme_bw()+coord_cartesian(ylim=c(0, 1))+
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



