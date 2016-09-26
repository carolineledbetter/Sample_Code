#analysis of Project 1
#########create dataset for analysis#######


#create dataset from only observation in the first 60 mos. 
analysis <- camp_teach[camp_teach$visitc <= 60,c(1:6,8,27)]
#add obs 

#additional variables to add
# baselineFEV = double(), ever50 = factor(), ever100 = factor(), everpet = factor(),
#everwoodstove = factor(), everdehumid = factor(), everparent = factor(), everanysmokes = factor())
#apply yes/no levels to vars 8-14
#lapply(8:14, function(i){
#  levels(analysis[,i]) <<- c("Yes","No")
})

