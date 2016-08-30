#load dataset
load(file = '/Users/CarolineL/Repositories/Data/Project0Data/Project0cleandata.rda')
#look at missing data
require(Amelia)
missmap(Project0_dental_data, main="Dental Data - Missings Map", 
        col=c("red", "white"), legend=FALSE)
#missing results by treatment group
require(doBy)
summaryBy(attach1year + pd1year ~ trtgroup, data = Project0_dental_data,
          FUN=function(x)sum(is.na(x)),keep.names = TRUE)
#look at demographics
table(Project0_dental_data$sex)
table(Project0_dental_data$race)
table(Project0_dental_data$smoker)
#confirm equal numbers in treatment groups
table(Project0_dental_data$trtgroup)
#look at race, sex and age by treatment group
barplot(table( Project0_dental_data$race,Project0_dental_data$trtgroup),
        col = c("blue", "green","red", "light blue"), 
        main = "Race by Treatment Group", legend.text = T,
        args.legend = list(x = 7, y=32,bty = 'n', cex = .75))
barplot(table( Project0_dental_data$sex,Project0_dental_data$trtgroup),
        col = c("blue", "red"), 
        main = "Race by Treatment Group", legend.text = T,
        args.legend = list(x = 6.5, y=32,bty = 'n', cex = .75))
boxplot(Project0_dental_data$age~Project0_dental_data$trtgroup,
        main = "Age by Treatment Group",
        xlab = 'Treatment Group', ylab = 'Age')
#look at results
#overall improvement
summary(Project0_dental_data$attachdiff)
summary(Project0_dental_data$pddiff)
# attachment improvement by treatment group
boxplot(Project0_dental_data$attachdiff ~ Project0_dental_data$trtgroup,
        main = 'Difference in attachment measurements
        at 1 year by Treatment Group',
        ylab = 'Attachment Difference',
        xlab = 'Treatment Group')
summaryBy(attachdiff + pddiff ~ trtgroup, data = Project0_dental_data,
          FUN=c(mean,sd), na.rm = T)
#for only nonsmokers
boxplot(Project0_dental_data$attachdiff[Project0_dental_data$smoker == 'No']
        ~ Project0_dental_data$trtgroup[Project0_dental_data$smoker == 'No'],
        main = 'Difference in attachment measurements
        at 1 year by Treatment Group
        for Non Smokers',
        ylab = 'Attachment Difference',
        xlab = 'Treatment Group')
summaryBy(attachdiff + pddiff ~ trtgroup, 
          data = Project0_dental_data[Project0_dental_data$smoker == "No" & 
                                        !is.na(Project0_dental_data$smoker),],
          FUN=c(mean,sd), na.rm = T)
#for only smokers
boxplot(Project0_dental_data$attachdiff[Project0_dental_data$smoker == 'Yes']
        ~ Project0_dental_data$trtgroup[Project0_dental_data$smoker == 'Yes'],
        main = 'Difference in attachment measurements
        at 1 year by Treatment Group
        for Smokers',
        ylab = 'Attachment Difference',
        xlab = 'Treatment Group')
summaryBy(attachdiff + pddiff ~ trtgroup, 
          data = Project0_dental_data[Project0_dental_data$smoker == "Yes" & 
                                        !is.na(Project0_dental_data$smoker),],
          FUN=c(mean,sd), na.rm = T)
