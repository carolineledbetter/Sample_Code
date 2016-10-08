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
load('/Users/Caroline/Repositories/Data/Project1Data/camp_teach.rda')

#create dataset from only observations in the first 72 mos and where there is an PREFEV measurement.
#Only select constant demographic variables, PREFEV and visitc.
analysis <- camp_teach[which(!is.na(camp_teach$PREFEV)),c(1:6,8,27)]
analysis <- analysis[analysis$visitc <= 72,]

#select last observations of each id
library(plyr)
analysis <- ddply(analysis, .(id), function(x) x[which.max(x$visitc),])

#add baseline measurements 
analysis <- merge(analysis, camp_teach[camp_teach$visitc == 0, c('id','PREFEV')], by = "id")
names(analysis)[c(7,9)] <- c("PREFEV", "baselineFEV")

#check for month of follow up
table(analysis$visitc) #there are numerous observations with less than 4 years

#####tasha said to leave in observations with less than 4 years - 10/8/16##### 
# #remove ids with less than 4 years of follow up
# analysis <- analysis[analysis$visitc >= 48, ] #35 observations removed
# #check
# table(analysis$visitc)#all good
####still need to remove those with only baseline
analysis <- analysis[analysis$visitc > 0, ] #3 observations removed
#check
table(analysis$visitc)#all good


#check for missing baseline measurements
which(is.na(analysis$baselineFEV))#id = 228 is missing.
#remove
analysis <- analysis[which(!is.na(analysis$baselineFEV)),]


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

#recode Don't Know for Dehumidifier as missing information
camp$dehumid[camp$dehumid == 'DK'] <- NaN
#remove DK level
camp$dehumid <- (droplevels(camp$dehumid))

#add variables for everpet, everwoodstove, everdehumid, everparent, everanysmokes.
analysis[,c(12:16)] <- sapply(c(22:26),function(x){sapply(analysis$id, function(i) {
  if (length(which(!is.na(camp[camp$id == i, x]))) == 0) return(NaN)
  else {
    if (length(which(camp[camp$id == i, x] == 'Yes')) == 0) return("No")
    else return("Yes")
  }
})})
names(analysis)[12:16] <- c("everpet", "everwoodstove", "everdehumid", "everparent", "everanysmokes")

#factor new variables
invisible(lapply(10:16, function(i){
  analysis[,i] <<- factor(analysis[,i], exclude = NaN)
}))

#confirm treatment groups and treatment are aligned (correct)
table(analysis$TX,analysis$TG)#confirmed

#reorder TG so placebo group is reference
analysis$TG_ref <- relevel(analysis$TG, 'C')
#confirm 
table(analysis$TG_ref, analysis$TG)#allgoof
#relabel for clarity
levels(analysis$TG_ref) <- c("Placebo", "Budesonide", "Nedocromil")

############save analysis dataset##############
save(analysis, file = '/Users/Caroline/Repositories/Data/Project1Data/analysis_ds.rda')

