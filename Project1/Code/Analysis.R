###########analysis of Project 1###################
load(file = '/Users/Caroline/Repositories/Data/Project1Data/camp_teach.rda')


#########create dataset for analysis#######

#create dataset from only observation in the first 60 mos. 
analysis <- camp_teach[camp_teach$visitc <= 60,c(1:6,8,27)]

#select last observations of each id
library(plyr)
analysis <- ddply(analysis, .(id), function(x) x[which.max(x$visitc),])

#add baseline measurements 
analysis <- cbind.data.frame(analysis, baselineFEV = camp_teach[camp_teach$visitc == 0, 8])

#check
table(analysis$visitc) #there are numerous observations with only a few visits
#remove ids with less than 4 years of follow up
analysis <- analysis[analysis$visitc >= 48, ] #36 observations removed

###add additional variables###
#setup variables for have they ever lived in a house older than 50 and 100 years
analysis[,10:11] <- t(sapply(analysis$id, function(i) {
  if (length(which(!is.na(camp_teach[camp_teach$id == i, 21]))) == 0) return(c(NaN,NaN))
  else {
    if (length(which(camp_teach[camp_teach$id == i, 21] > 49)) == 0) {
      return(c("No", "No"))}
    else {
      if (length(which(camp_teach[camp_teach$id == i, 21] > 99)) == 0) {
        return(c("Yes", "No"))}
     else return(c("Yes", "Yes"))}
  }
}))
names(analysis)[10:11] <- c("Ever50","Ever100")

#add variables for everpet, everwoodstove, everparent, everanysmokes.
analysis[,c(12:15)] <- sapply(c(22,23,25,26),function(x){sapply(analysis$id, function(i) {
  if (length(which(!is.na(camp_teach[camp_teach$id == i, x]))) == 0) return(NaN)
  else {
    if (length(which(camp_teach[camp_teach$id == i, x] == 'Yes')) == 0) return("No")
    else return("Yes")
  }
})})
names(analysis)[12:15] <- c("everpet", "everwoodstove", "everparent", "everanysmokes")

#factor new variables
invisible(lapply(10:15, function(i){
  analysis[,i] <<- factor(analysis[,i], exclude = NaN)
}))

############save analysis dataset##############
save(analysis, file = '/Users/Caroline/Repositories/Data/Project1Data/analysis_ds.rda')
