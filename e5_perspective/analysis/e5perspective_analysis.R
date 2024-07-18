#====================================================================================
#                               Self Clone
#====================================================================================

## clear workspace
rm(list = ls())  

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('rjson',       
               'ltm', 
               'heplots',            
               'lmtest',        
               'compute.es',        
               'ltm',           
               'lsr',      
               'lsmeans',        
               'nnet',          
               'mlogit',
               'dplyr', 
               'effsize', 
               'ggplot'
)

#====================================================================================
#                             PRE-PROCESSING
#====================================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("data.csv")
data <- data[-c(1,2),]

## Attention Checks
data |>
  filter(attn_1 == "Paul" & attn_2 == "Purple") -> data

## Number of Participants
initial_sample <- nrow(data); initial_sample

## Comprehension Checks
data |>
  filter(
    comp_q == "Teletransportation"
  ) -> data

## Final Sample
final_sample <- nrow(data); final_sample

## Organize
data <- data %>%
  mutate(identity = as.numeric(ifelse(basic_identity_1 != "", basic_identity_1, first_identity_1)))

data$cond_name <- ifelse(data$cond==1, 'basic', 'perspective')

## Demographics
mean(as.numeric(data$age))
table(data$gender)
table(data$cond)

#====================================================================================
#                               ANALYSIS
#====================================================================================

p_data <- subset(data, data$cond_name=='perspective')

t.test(data$identity ~ data$cond_name)
aggregate(data$identity ~ data$cond_name, data = data, FUN = sd)
cohen.d(data$identity, data$cond_name)

table(p_data$simultaneous_or_flip)
prop.table(table(p_data$simultaneous_or_flip))

# end ====================================================================================

