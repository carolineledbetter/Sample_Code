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
