install.packages("")
library(readxl)
lab<- read.csv("C:/Users/india/Desktop/data science/assignments/hypothesis testing/LabTAT.csv")
lab
Stacked_Data <- stack(lab)
View(Stacked_Data)
attach(Stacked_Data)
#Normality test
library(nortest)
ad.test(Stacked_Data$values) 
#Variance test 
install.packages("car")
library(car)
leveneTest(Stacked_Data$values~Stacked_Data$ind, data = Stacked_Data)   #Test for equal Variance
#One-way Anova 
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
boxplot((lab$Laboratory.1),(lab$Laboratory.2),(lab$Laboratory.3),(lab$Laboratory.4))
#There is no difference in average TAT among laboratory 1 and laboratory 2.