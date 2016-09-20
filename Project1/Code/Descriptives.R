#attach data set so variables can be accessed directly
attach(camp_teach)

table(TX)#look at treatment groups
table(TG)


#look at demographics
source("/Users/Caroline/Repositories/Table1/Table1.r")
Table1(c("age_rz","GENDER", "ETHNIC","wbc", "agehome", "anypet", "woodstove", "dehumid",
         "parent_smokes", "any_smokes"),2, data = camp_teach)