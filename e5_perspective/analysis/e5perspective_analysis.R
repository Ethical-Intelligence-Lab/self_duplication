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

table(p_data$simultaneous_or_flip)
prop.table(table(p_data$simultaneous_or_flip))

t.test(data$identity ~ data$cond_name)
aggregate(data$identity ~ data$cond_name, data = data, FUN = sd)
cohen.d(data$identity, data$cond_name)

mean(data$identity[data$simultaneous_or_flip == "Two consciousnesses"], na.rm=TRUE)
mean(data$identity[data$simultaneous_or_flip == "Single consciousness, flipping"], na.rm=TRUE)
mean(data$identity[data$simultaneous_or_flip == "Single consciousness, simultaneous"], na.rm=TRUE)
mean(data$identity[data$simultaneous_or_flip == "Single consciousness, split screen"], na.rm=TRUE)

#====================================================================================
#                               VISUALIZATION
#====================================================================================

x_scale_labels <- c('basic', 'perspective')
theme_update(plot.title = element_text(hjust = 0.5))

ggplot(d_plot1, aes(x = cond ,y = simultaneous_or_flip))+
  stat_summary(fun = mean, position = position_dodge(), geom = "bar", width = 0.5) +
  coord_cartesian(ylim = c(0, 0.70)) +
  theme(axis.title.x = element_blank()) + 
  theme_classic() +
  scale_x_discrete(breaks = 1:2, labels = x_scale_labels) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1,vjust = 0.8))+
  ylab("Proportion of Identifications")+
  xlab("") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) 
