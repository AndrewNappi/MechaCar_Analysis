# Import dplyr package (tidyverse)
library(tidyverse)

# Import first dataset
mechacar_mpg <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)

# Perform MLR
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance +
     AWD, data=mechacar_mpg)

# Determine the p-value and the r-squared value
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
             ground_clearance + AWD, data=mechacar_mpg))

# Import second dataset
suspension_coil <- read.csv('Suspension_Coil.csv',check.names = F,
                            stringsAsFactors = F)

# The suspension coil’s PSI
total_summary <- suspension_coil %>% summarize(Mean=mean(PSI),Median=median(PSI),
                                               Variance=var(PSI), SD=sd(PSI))

# find the Mean, Median, Variance, and Standard deviation of the second data set
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% 
  summarize(Mean=mean(PSI),Median=median(PSI),
            Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

# Visualize the distribution of PSI
ggplot(suspension_coil,aes(x=PSI)) + geom_density()

# Run one sample t-test for coil PSI where mu= 1500
t.test(suspension_coil$PSI, mu=1500)

# LOT 1: Run one sample t-test for coil PSI where mu= 1500
lot1_psi <- subset(suspension_coil, Manufacturing_Lot == "Lot1")
t.test(lot1_psi$PSI, mu=1500)

# LOT 2: Run one sample t-test for coil PSI where mu= 1500
lot2_psi <- subset(suspension_coil, Manufacturing_Lot == "Lot2")
t.test(lot2_psi$PSI, mu=1500)

# LOT 3: Run one sample t-test for coil PSI where mu= 1500
lot3_psi <- subset(suspension_coil, Manufacturing_Lot == "Lot3")
t.test(lot3_psi$PSI, mu=1500)