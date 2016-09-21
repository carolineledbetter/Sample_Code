#attach data set so variables can be accessed directly
attach(camp_teach)

#get demographics by id
ids <- unique(id) 

#create empty data frame with structure of first 6 columns
#only treatment, age at enrollment, gender, and ethnicity are constant for all 
#observations of a given id.
desc <- camp_teach[0,1:6]
invisible(lapply(ids, function(i) {
  desc <<- rbind.data.frame(desc,camp_teach[camp_teach$id == i,1:6][1,])
}))

#look at demographics
source("/Users/Caroline/Repositories/Table1/Table1.r")
Table1(c("age_rz","GENDER", "ETHNIC"),1, data = desc)
