# Load data from CSV into R. 
Project0_dental_data <- read.csv(
  "~/Library/Mobile Documents/com~apple~CloudDocs/BIOS 6623/Datasets/Project0_dental_data.csv",
  row.names=NULL)
#Convert intergers for categorical variables to factors for easier analysis.
Project0_dental_data$sex <- factor(Project0_dental_data$sex)
levels(Project0_dental_data$sex) <- c("male","female")
Project0_dental_data$race <- factor(Project0_dental_data$race)
levels(Project0_dental_data$race) <- c("Native American", "African American",
                                       "Asian", "White")
Project0_dental_data$smoker <- factor(Project0_dental_data$smoker)
levels(Project0_dental_data$smoker) <- c("No", "Yes")
#add varaibles for difference between baseline and 1 year measurements
Project0_dental_data$attachdiff <- Project0_dental_data$attach1year -
  Project0_dental_data$attachbase
Project0_dental_data$pddiff <- Project0_dental_data$pd1year - 
  Project0_dental_data$pdbase
# save dataframe as r data set
save(Project0_dental_data, 
     file = '/Users/CarolineL/Repositories/Data/Project0Data/Project0cleandata.rda')
