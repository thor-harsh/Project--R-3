getwd()

fin<-read.csv("P3-Future-500-The-Dataset.csv",na.strings = c(""))
fin
head(fin)
tail(fin,10)
str(fin)
summary(fin)


#Changing from non-factor to factor
fin$ID<-factor(fin$ID)
summary(fin)


fin$Inception<-factor(fin$Inception)
str(fin)


#Factor Variable Trap (FVT)
a<-c("1",'2','3','4','5')
a
typeof(a)

b<-as.numeric(a)
b
typeof(b)


z<-factor(c("12",'13','14','12','13'))
z
typeof(z)
y<-as.numeric(z)
y
typeof(y)


x<-as.numeric(as.character(z))
x
typeof(x)


str(fin)
#fin$Profit<-factor(fin$Profit)
summary(fin)


#fin$Profits<-as.numeric(fin$Profit)
#fin$Profits<-NULL
str(fin)

?sub()

#Using gsub and sub function
#First converting non factor variable to factor variable for Expenses column
fin$Expenses<-gsub(" Dollars","",fin$Expenses)
fin$Expenses<-gsub(",",'',fin$Expenses)
head(fin)
str(fin) 

#Secondly converting non factor variable to factor variable for Revenue Column
fin$Revenue<-gsub("\\$","",fin$Revenue)
fin$Revenue<-gsub(",","",fin$Revenue)
head(fin)

str(fin)

#Thirdly converting non factor variable to factor variable for Growth Column
fin$Growth<-gsub("%",'',fin$Growth)
head(fin)
str(fin)

fin$Expenses<-as.numeric(fin$Expenses)
fin$Revenue<-as.numeric(fin$Revenue)
fin$Growth<-as.numeric(fin$Growth)
head(fin)
str(fin)
summary(fin)







#Filtering: using which() for non-missing data

head(fin,24)
fin[!complete.cases(fin),]
fin[fin$Revenue==9746272,]

fin[which(fin$Revenue==9746272),]

head(fin)
fin[which(fin$Employees==45),]


is.na(fin$Expenses)
fin[is.na(fin$Expenses),]
fin[is.na(fin$State),]

#Removing records with missing data

fin_backup<-fin

fin[!complete.cases(fin),]
fin<-fin[!is.na(fin$Industry),]
fin


#Resetting the dataframe index
rownames(fin)<-1:nrow(fin)
fin

rownames(fin)<-NULL
fin


fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City=='New York',]
fin[is.na(fin$State) & fin$City=="New York","State"]<-'NY'


fin[!complete.cases(fin),]


fin[c(11,379),]

fin[is.na(fin$State) & fin$City=='San Francisco',"State"]<-'CA'
fin[c(84,267),]

#Median Computation method for industry

med_empl_detail<-median(fin[fin$Industry=='Retail',"Employees"],na.rm = TRUE)
med_empl_detail

fin[is.na(fin$Employees) & fin$Industry=="Retail","Employees"]<-med_empl_detail
fin[3,]

med_fin_detail<-median(fin[fin$Industry=="Financial Services","Employees"],na.rm=TRUE)
fin[is.na(fin$Employees) & fin$Industry=='Financial Services',"Employees"]<-med_fin_detail
fin[332,]


#Median Computation method for Growth
fin[!complete.cases(fin),]


mean(fin[,'Growth'],na.rm=TRUE)
median(fin[,'Growth'],na.rm = TRUE)

mean(fin[fin$Industry=='Construction','Growth'],na.rm = TRUE)
med_growth_constr<-median(fin[fin$Industry=='Construction','Growth'],na.rm = TRUE)

fin[is.na(fin$Growth) & fin$Industry=="Construction","Growth"]<-med_growth_constr
fin[!complete.cases(fin),]


#Median Imputation for Revenue
med_rev_constr<-median(fin[fin$Industry=='Construction',"Revenue"],na.rm = TRUE)
med_rev_constr

fin[is.na(fin$Revenue) & fin$Industry=='Construction','Revenue']<-med_rev_constr

fin[!complete.cases(fin),]


#Median imputation for Expenses
med_exp_constr<-median(fin[fin$Industry=="Construction","Expenses"],na.rm = TRUE)
med_exp_constr

fin[is.na(fin$Expenses) & fin$Industry=='Construction' & is.na(fin$Profit),'Expenses']<-med_exp_constr
fin[8,]


fin[!complete.cases(fin),]



#Replace Missing data: deriving values
#Revenue-Expense=Profit
#Expenses=Revenue-Profit
fin[is.na(fin$Profit),'Profit']<-fin[is.na(fin$Profit),"Revenue"]-fin[is.na(fin$Profit),"Expenses"]
fin[!complete.cases(fin),]


fin[is.na(fin$Expenses),"Expenses"]<-fin[is.na(fin$Expenses),"Revenue"]-fin[is.na(fin$Expenses),'Profit']
fin[!complete.cases(fin),]
fin[15,]



#They have requested the following charts:
#• A scatterplot classified by industry showing revenue, expenses, profit


library(ggplot2)

p<-ggplot(data=fin)
p

p+geom_point(aes(x=Revenue,y=Expenses,
                 colour=Industry,size=Profit))


#• A scatterplot that includes industry trends for the expenses~revenue relationship
d<-ggplot(data=fin,aes(x=Revenue,y=Expenses,colour=Industry))
d+geom_point() +
  geom_smooth(fill=NA,size=1.2)



#• BoxPlots showing growth by industry

f<-ggplot(data=fin,aes(x=Industry,y=Growth,
                       colour=Industry))
f + geom_boxplot(size=1)

f+geom_jitter()+geom_boxplot(size=1,alpha=0.5,
                             outlier.color=NA)



#Factor Variable Trap