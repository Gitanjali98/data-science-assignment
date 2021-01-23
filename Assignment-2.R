df=read.csv("C:/Users/india/Desktop/data science/assignments/ass2.csv")
df
mean(df$Measure.X)
var(df$Measure.X)
sd(df$Measure.X)
boxplot(df$Measure.X)


p=1/200 #p is probability call is miscarried
p
p1=1-p  #p1 is probability call is not miscarried
p1
prob=1-(p1)^5  #prob is probability that at least one in five attempted telephone calls reaches the wrong number
prob


a=c(-2,000,-1,000,0,1000,2000,3000)
sd(a)
