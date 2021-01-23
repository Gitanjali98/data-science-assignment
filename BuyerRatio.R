install.packages("")
library(readxl)
buyer<-read.csv("C:/Users/india/Desktop/data science/assignments/hypothesis testing/BuyerRatio.csv")  
View(buyer) 
attach(buyer)
colnames(buyer)
buyer1<-stack(buyer,select = c(East=East,West=West,North=North,South=South))
buyer1
chisq.test(buyer1$ind,buyer1$values)
#p-value:0.2931>0.05    Therefore ACCEPT null hypothesis