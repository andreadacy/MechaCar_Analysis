# Deliverable 1: Linear Regression to Predict MPG

# 3. Load dplyr package
library(dplyr)

# 4. Import MechaCar_mpg.csv as a dataframe
mechacar_mpg_df <- read.csv(file='/Users/andrea/Desktop/Class/MechaCar_Analysis/Resources/MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

head(mechacar_mpg_df)

# 5. Perform linear regression using lm()
mecha_lm <- lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data=mechacar_mpg_df)

# Generate summary statistics
summary(mecha_lm)

# Deliverable 2: Create Visualizations for the Trip Analysis

# 2. Import and read in Suspension_Coil.csv as a table
suspension_coil_df <- read.csv(file='/Users/andrea/Desktop/Class/MechaCar_Analysis/Resources/MechaCar_mpg.csv', check.names=F, stringsAsFactors=F)

head(suspension_coil_df)

# 3. Write an RScript that creates a total_summary dataframe using the 
# summarize() function to get the mean, median, variance, and standard deviation 
# of the suspension coil’s PSI column.
total_summary <- suspension_coil_df %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

# 4. Write an RScript that creates a lot_summary dataframe using the group_by() 
# and the summarize() functions to group each manufacturing lot by the mean, 
# median, variance, and standard deviation of the suspension coil’s PSI column.
lot_summary <- suspension_coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')

# Deliverable 3: T-Tests on Suspension Coils

# 1. Use the t.test() function to determine if the PSI across all manufacturing lots 
# is statistically different from the population mean of 1,500 pounds per square inch
t.test(suspension_coil_df$PSI, mu=1500)
t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot1")$PSI, mu=1500)
t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot2")$PSI, mu=1500)
t.test(subset(suspension_coil_df,Manufacturing_Lot=="Lot3")$PSI, mu=1500)
