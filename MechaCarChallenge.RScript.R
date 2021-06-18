### Challenge 15

### Deliverable 1

#3. load dplyr library package
library(dplyr)

#4. import csv as a df
library(tidyverse)
mecha_mpg <- read.csv("MechaCar_mpg.csv")

#5. perform linear regression using lm()
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mecha_mpg)

#6. using summary() determine p-value and r-squared value for the linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mecha_mpg))

# bonus: lets remove independent variables that have little imppact on mpg
lm(mpg ~ vehicle_length + ground_clearance, data = mecha_mpg)
summary(lm(mpg ~ vehicle_length + ground_clearance, data = mecha_mpg))

### Deliverable 2

#2. import suspension_coil txt file
mecha_coil <- read.csv("Suspension_Coil.csv")

#3. create a total_summary df using summarize() to get mean, median,variance, stdev of coil
tota_summary <- mecha_coil %>% summarise(mean_PSI=mean(PSI),
                                         Median_PSI=median(PSI),
                                         Var_PSI=var(PSI),
                                         Std_Dev_PSI=sd(PSI),
                                         num_coil=n(), .groups = 'keep')

#4. create lot_summary df using group_by() and summarise() to group manufacturing lot
lot_summary <- mecha_coil %>% group_by(Manufacturing_Lot) %>% summarise(Mean_PSI=mean(PSI),
                                                                        Median_PSI=median(PSI),
                                                                        Var_PSI=var(PSI),
                                                                        Std_Dev_PSI=sd(PSI),
                                                                        num_coil=n(), .groups = 'keep')

#Box plot PSI whole lot
plt1 <- ggplot(mecha_coil,aes(x=Manufacturing_Lot, y=PSI))
plt1 + geom_boxplot()

#Box plot each individual lot
plt2 <- ggplot(mecha_coil,aes(x=Manufacturing_Lot, y=PSI))
plt2 + geom_boxplot()

### Deliverable 3

#1. using t-test to determine if PSI across all lotss is statisctically differnt from population mean of 1500 PSI
t.test(mecha_coil$PSI,mu=1500)

#2. use t.test 3 more times with subset() to determine is PSI for each manufacturing lot is statistically different from pop. of 1500 PSI
lot1 <- subset(mecha_coil, Manufacturing_Lot=="lot1")
lot2 <- subset(mecha_coil, Manufacturing_Lot=="Lot2")
lot3 <- subset(mecha_coil, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)
                                                                        