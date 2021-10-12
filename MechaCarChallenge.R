library(tidyverse)
library(dplyr)

MechaCar_mpg_data <- read.csv('MechaCar_mpg.csv') #import dataset
head(MechaCar_mpg_data)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg_data) #generate multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg_data)) #generate summary statistics

#Deliverable 2
Suspension_table <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
total_summary <- Suspension_table %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep') #create summary table
lot_summary <- Suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

#Deliverable 3

#?t.test()

t.test(Suspension_table$PSI, mu=1500)


population1 <- subset(Suspension_table, Manufacturing_Lot=='Lot1') 
t.test(population1$PSI, mu=1500)

population2 <- subset(Suspension_table, Manufacturing_Lot=='Lot2') 
t.test(x=population2$PSI, mu=1500) 

population3 <- subset(Suspension_table, Manufacturing_Lot=='Lot3') 
t.test(x=population3$PSI, mu=1500)
