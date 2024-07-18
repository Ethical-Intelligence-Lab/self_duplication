#====================================================================================
#                               Self Clone
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

data <- read_csv("data.csv")
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

#reorganize data

data |>
  select(dv_identity, dv_entitlement, cond, age, gender) |>
  mutate(
    identity = case_when(
      cond == 1 ~ dv_identity,
      cond == 2 ~ dv_entitlement,
    )
  ) |>
  select(-dv_entitlement, -dv_identity) |>
  mutate(
    identity = relevel(as.factor(case_when(
      identity == "Only the person at his relative's house." ~ "copy_relative",
      identity == "Only the person from his relative's house." ~ "copy_relative",
      identity == "Both persons." ~ "both",
      identity == "I am not sure." ~ "unsure",
      identity == "Only the person at his friend's house." ~ "copy_friend",
      identity == "Only the person from his friend's house." ~ "copy_friend",
    )), ref = "unsure"),
    cond = relevel(as.factor(cond), ref = "1")
  ) -> data

data$cond_name <- ifelse(data$cond == '1', 'Identity', 'Wife')

## Demographics
data$age <- as.numeric(data$age)
data$age[144]<- 2024 - data$age[144]
data$age[145] <- 2024 - data$age[145]
data$age[167] <- 2024 - data$age[167]
data$age[179] <- 2024 - data$age[179]
data$age[182] <- 2024 - data$age[182]
data$age[190] <- 2024 - data$age[190]
data$age[220] <- 2024 - data$age[220]
data$age[256] <- 2024 - data$age[256]

mean(as.numeric(data$age))
table(data$gender)

table(data$cond)

#====================================================================================
#                               ANALYSIS
#====================================================================================

## Condition and Identity
table(data$cond_name, data$identity)
prop.table(table(data$cond_name, data$identity), margin = 1) * 100

## Regression on Identity
reg1 <- multinom(identity ~ cond, data = data)
s <- summary(reg1)
z <- s$coefficients/s$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

s
z
p

#====================================================================================
#                               VISUALIZATION
#====================================================================================

## Identity
data |>
  ungroup() |>
  mutate(
    relative = ifelse(identity == "copy_relative", 1, 0),
    friend = ifelse(identity == "copy_friend", 1, 0),
    both = ifelse(identity == "both", 1, 0),
    unsure = ifelse(identity == "unsure", 1, 0)
    ) |>
  select(cond, relative, friend, unsure, both) |>
  group_by(cond) |>
  summarize_all(
    mean
  ) |>
  gather(key = "answer", value = "mean", relative, friend, unsure, both) |>
  mutate(
    answer = as.factor(case_when(
      answer == "relative" ~ 1,
      answer == "friend" ~ 2,
      answer == "both" ~ 3,
      answer == "unsure" ~ 4,
    )))-> d_plot1

theme_update(plot.title = element_text(hjust = 0.5))

x_scale_labels <- c("Identity", "Practical Matters")

ggplot(d_plot1, aes(x = cond ,y = mean, fill = factor(answer)))+
  stat_summary(fun = mean, position = position_dodge(), geom = "bar", width = 0.5) +
  coord_cartesian(ylim = c(0, 0.70)) +
  theme(axis.title.x = element_blank()) + 
  theme_classic() +
  scale_x_discrete(breaks = 1:2, labels = x_scale_labels) +
  theme(legend.key = element_blank(), legend.box = "horizontal", legend.position = c(0.5, 0.95), legend.direction="horizontal")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1,vjust = 0.8))+
  ylab("Proportion of Choices")+
  xlab("") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_fill_manual(values = c("aquamarine3", "tomato3","darkcyan", "tomato4"), name = "", 
                    labels = c('Relative', 'Friend', 'Both', 'Unsure'))

