#load data set
load(file = '/Users/Caroline/Repositories/Data/Project1Data/camp_teach.rda')

#attach data set so variables can be accessed directly
attach(camp_teach)

#get demographics by id
ids <- unique(id) 

#create empty data frame with structure of first 6 columns
#only treatment, age at enrollment, gender, and ethnicity are constant for all 
#observations of a given id.
desc <- camp_teach[0,1:6]
invisible(lapply(ids, function(i) {
  desc <<- rbind.data.frame(desc,unique(camp_teach[camp_teach$id == i, 1:6])
)
}))#obs in desc should match length of ids- it does!


#create a dataset with the number of observations for each id
ids_obs <- data.frame("ids" = ids, "obs" = unlist(lapply(ids, function(i) {length(camp_teach[camp_teach$id == i,3])})))
#get range and mean # of observation
range(ids_obs$obs); mean(ids_obs$obs)

#change names for demographics for clarity/prettiness and expand factors
names(desc) <- c(names(desc)[1:3],"Age at Enrollment", "Gender", "Ethnicity")
levels(desc$Gender) <- c("Female", "Male")
levels(desc$Ethnicity) <- c("Black", "Hispanic", "Other", "Non-Hispanic White")
#look at demographics
source("/Users/Caroline/Repositories/Table1/Table1.r")
Table_1 <- Table1(c(4:6),1, data = desc)

#save workspace for use in .rmd
save.image("~/Repositories/bios6623-ledbettc/Project1/Descriptives.RData")

#look at how manys obs at each outcome
hist(visitc, freq = TRUE, plot = TRUE, xlab= 
       "Months from Initial Measurement", col = "blue",
     main='Number of Id with observations
at each time period', cex.main = 1, cex.lab = 0.75)

#look at age of homes data;
hist(agehome, freq = TRUE, plot = TRUE, xlab= 
       "Age of Home in Years", col = "blue", cex.main = 1, cex.lab = 0.75)

######add descriptives for analysis dataset#######
load(file = '/Users/Caroline/Repositories/Data/Project1Data/analysis_ds.rda')
#change names for demographics for clarity/prettiness and expand factors
names(analysis)[c(4:6,10:15)] <- c("Age at Enrollment", "Gender", "Ethnicity", "House >= 50", "House >= 100", "Pets", "Woodstove", 
                                   "Parents Smoke", "Anyone Smokes")
levels(analysis$Gender) <- c("Female", "Male")
levels(analysis$Ethnicity) <- c("Black", "Hispanic", "Other", "Non-Hispanic White")
source("/Users/Caroline/Repositories/Table1/Table1.r")
Table_1_analysis <- Table1(c(4:6,8,10:15),1, data = analysis) #treatment groups are well balanced, only siginificant difference is gender


boxplot(analysis$PREFEV ~ analysis$TX)
boxplot(analysis$PREFEV ~ analysis$TG)
plot(analysis$PREFEV, analysis$baselineFEV)
boxplot(analysis$PREFEV ~ analysis$`Age at Enrollment`)
boxplot(analysis$PREFEV ~ analysis$Gender)

#save analysis descriptives workspace for use with .rmd
save.image("~/Repositories/bios6623-ledbettc/Project1/AnalysisDescriptives.RData")
