library(tidyverse)
library(dplyr)

MechaCar_mpg_data <- read.csv('MechaCar_mpg.csv') #import dataset
head(MechaCar_mpg_data)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg_data) #generate multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg_data)) #generate summary statistics

#Deliverable 2
Suspension_table <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
suspension_summary <- Suspension_table %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep') #create summary table

