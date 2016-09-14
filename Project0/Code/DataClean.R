# Load data from CSV into R. 
Project0_dental_data <- read.csv(
  "~/Library/Mobile Documents/com~apple~CloudDocs/BIOS 6623/Datasets/Project0_dental_data.csv",
  row.names=NULL)

#Convert intergers for categorical variables to factors for easier analysis.
Project0_dental_data$sex <- factor(Project0_dental_data$sex)
levels(Project0_dental_data$sex) <- c("Male","Female")
Project0_dental_data$race <- factor(Project0_dental_data$race)
levels(Project0_dental_data$race) <- c("Native American", "African American",
                                       "Asian", "White")
Project0_dental_data$smoker <- factor(Project0_dental_data$smoker)
levels(Project0_dental_data$smoker) <- c("No", "Yes")

#change trtgroup so that no gel is lowest level (0) and gel with no active ingredient is next
Project0_dental_data$trtgroupFactored <- Project0_dental_data$trtgroup
Project0_dental_data$trtgroupFactored[Project0_dental_data$trtgroup == 2] <- 0

#confirm
length(Project0_dental_data$trtgroupFactored[Project0_dental_data$trtgroupFactored == 0])

#factor and add labels
Project0_dental_data$trtgroupFactored <- factor(Project0_dental_data$trtgroupFactored)
levels(Project0_dental_data$trtgroupFactored) <- c("No Treatment", "Placebo", "Low", "Medium", "High")

#confirm new treatment groups
table(Project0_dental_data$trtgroup,Project0_dental_data$trtgroupFactored)

#add varaibles for difference between baseline and 1 year measurements
Project0_dental_data$attachdiff <- Project0_dental_data$attach1year -
  Project0_dental_data$attachbase
Project0_dental_data$pddiff <- Project0_dental_data$pd1year - 
  Project0_dental_data$pdbase

#collapse non-white race groups
Project0_dental_data$RaceColl <- Project0_dental_data$race
levels(Project0_dental_data$RaceColl) <- list("Non-White" = c("Native American", "African American",
                                                                "Asian"),
                                              "White" = "White")
#confirm
table(Project0_dental_data$RaceColl)

# save dataframe as r data set
save(Project0_dental_data, 
     file = '/Users/Caroline/Repositories/Data/Project0Data/Project0cleandata.rda')

#Create a data set with no missing data 
Project0_no_missing <- Project0_dental_data[!is.na(Project0_dental_data$attach1year),]
Project0_no_missing <- Project0_no_missing[!is.na(Project0_no_missing$age),]
Project0_no_missing <- Project0_no_missing[!is.na(Project0_no_missing$smoke),]
save(Project0_no_missing, 
     file = '/Users/Caroline/Repositories/Data/Project0Data/Project0cleandatanomissing.rda')
