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

