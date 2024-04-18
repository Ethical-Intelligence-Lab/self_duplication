#====================================================================================
# Julian De Freitas, 2019
# Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
# Experiment 3
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

## Attention Checks
data |>
  filter(
    trialStruct.attention == 0 & 
    trialStruct.comp_original_you == 3 & 
    trialStruct.comp_number_copies == 2
   ) -> data

## Final Sample
nrow(data)

## Age
mean(data$trialStruct.age)
## Sex
table(data$trialStruct.sex)

#====================================================================================
#                             ANALYSIS
#====================================================================================

data |>
  select(trialStruct.identity_before, trialStruct.identity_after) |>
  mutate( initial_copy = ifelse(trialStruct.identity_before == 2, 1, 0) ) -> d

colnames(d) <- c("dead", "revived", "initial_copy")

d |>
  gather(key = "condition", value = "identity", dead, revived) |>
  mutate(
    copy = ifelse(identity == 2, 1, 0),
    original = ifelse(identity == 1, 1, 0),
    both = ifelse(identity == 3, 1, 0),
    neither = ifelse(identity == 4, 1, 0),
    condition = relevel(as.factor(condition), ref = "dead")
  ) -> d

reg1 <- multinom(identity ~ condition, data = d)
s <- summary(reg1)
z <- s$coefficients/s$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

## Proportion that selected COPY by Condition
prop.table(table(d$identity, d$condition), margin = 2)["2",] * 100

### Beta Coefficient
s$coefficients[ "2" , "conditionrevived" ]
### Standard Error
s$standard.errors[ "2" , "conditionrevived" ]
### p-value
p[ "2" , "conditionrevived" ]

## p-value for NEITHER and BOTH
p[ "3" , "conditionrevived" ]
p[ "4" , "conditionrevived" ]

## Focus on INITIALLY identifying as COPY
d |>
  filter(initial_copy == 1) -> d_copy

## Proportion that selected
prop.table(table(d_copy[d_copy$condition == "revived",]$identity)) * 100

#====================================================================================
#                             VISUALIZATION
#====================================================================================

#TODO
d |>
  ungroup() |>
  mutate( cond = ifelse(condition == "dead", 1, 2)) |>
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
      answer == "neither" ~ 4,
      answer == "both" ~ 3,
    )))-> d_plot

theme_update(plot.title = element_text(hjust = 0.5))

x_scale_labels <- c("Dead", "Revived")

ggplot(d_plot, aes(x = as.factor(cond) ,y = mean, fill = factor(answer))) +
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar",width = 0.5) +
  theme_classic() +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_x_discrete(breaks = 1:2, labels = x_scale_labels) +
  theme(legend.key = element_blank(), legend.box = "horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1,vjust = 0.8)) +
  ylab("Proportion of Identifications") +
  xlab("State of the Original") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_fill_manual(values = c("aquamarine3", "tomato3","darkcyan", "tomato4"), name = "", 
                    labels = c('Original', 'Copy', 'Neither', 'Both'))

