# Load data from csv into R. 
camp_teach <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/BIOS 6623/Datasets/camp_teach.csv")


#factor anypet, woodstove, dehumid, parent_smokes, and anysmokes for clarity
camp_teach$dehumid <-factor(camp_teach$dehumid)
levels(camp_teach$dehumid) <- c("Yes", "No", "DK")
lapply(c("anypet", "woodstove", "parent_smokes", "any_smokes"), function(i){
  camp_teach[,i] <<- factor(camp_teach[,i])
  levels(camp_teach[,i]) <<- c("Yes","No")
})

save(camp_teach, file = '/Users/Caroline/Repositories/Data/Project1Data/camp_teach.rda')

#########create dataset for analysis#######

#create dataset from only observations in the first 72 mos. Only select constant demographic variables, PREFEV and visitc.
analysis <- camp_teach[camp_teach$visitc <= 72,c(1:6,8,27)]

#select last observations of each id
library(plyr)
analysis <- ddply(analysis, .(id), function(x) x[which.max(x$visitc),])

#add baseline measurements 
analysis <- cbind.data.frame(analysis, baselineFEV = camp_teach[camp_teach$visitc == 0, 8])

#check
table(analysis$visitc) #there are numerous observations with only a few visits
#remove ids with less than 4 years of follow up
analysis <- analysis[analysis$visitc >= 48, ] #35 observations removed

###add additional variables###
#restrict dataset to only first 72 months.
camp <- camp_teach[camp_teach$visitc <=72,]
#setup variables for have they ever lived in a house older than 50 and 100 years
analysis[,10:11] <- t(sapply(analysis$id, function(i) {
  if (length(which(!is.na(camp[camp$id == i, 21]))) == 0) return(c(NaN,NaN))
  else {
    if (length(which(camp[camp$id == i, 21] > 49)) == 0) {
      return(c("No", "No"))}
    else {
      if (length(which(camp[camp$id == i, 21] > 99)) == 0) {
        return(c("Yes", "No"))}
      else return(c("Yes", "Yes"))}
  }
}))
names(analysis)[10:11] <- c("Ever50","Ever100")

#add variables for everpet, everwoodstove, everparent, everanysmokes.
analysis[,c(12:15)] <- sapply(c(22,23,25,26),function(x){sapply(analysis$id, function(i) {
  if (length(which(!is.na(camp[camp$id == i, x]))) == 0) return(NaN)
  else {
    if (length(which(camp[camp$id == i, x] == 'Yes')) == 0) return("No")
    else return("Yes")
  }
})})
names(analysis)[12:15] <- c("everpet", "everwoodstove", "everparent", "everanysmokes")

#factor new variables
invisible(lapply(10:15, function(i){
  analysis[,i] <<- factor(analysis[,i], exclude = NaN)
}))

#confirm treatment groups and treatment are aligned (correct)
table(analysis$TX,analysis$TG)#confirmed

############save analysis dataset##############
save(analysis, file = '/Users/Caroline/Repositories/Data/Project1Data/analysis_ds.rda')

