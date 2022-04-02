library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ppcor)

lifexp <- read.csv("D:\\Drivers\\academics\\sem 5  3rd year 1st sem\\stats\\extrass\\stats lab\\Life Expectancy Data.csv")
View(lifexp)       

Fullifexp<-lifexp
lifexp <- lifexp %>% drop_na()

dim(lifexp)
range(lifexp$Year)

#Adult.Mortality - Adult Mortality Rates on both sexes (probability of dying between 15-60 years/1000 population).
#infant.deaths - Number of Infant Deaths per 1000 population.
#Alcohol - Alcohol recorded per capita (15+) consumption (in litres of pure alcohol).
#percentage.expenditure - Expenditure on health as a percentage of Gross Domestic Product per capita(%).
#Hepatitis.B - Hepatitis B (HepB) immunization coverage among 1-year-olds (%) #how many below one year of age received 3 doses (%)
#BMI - Average Body Mass Index of entire population.
#under.five.deaths - Number of under-five deaths per 1000 population.
#Polio - Polio (Pol3) immunization coverage among 1-year-olds (%).
#Total expenditure - General government expenditure on health as a percentage of total government expenditure (%).
#Diphtheria - Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds (%).
#HIV_AIDS - Deaths per 1 000 live births HIV/AIDS (0-4 years).
#GDP - Gross Domestic Product per capita (in USD).
#Population - Population of the country.
#thinness.10.19 years - Prevalence of thinness among children and adolescents for Age 10 to 19 (%).
#thinness 5-9 years - Prevalence of thinness among children for Age 5 to 9(%).
#Income.composition.of.resources -  in terms of income composition of resources (index ranging from 0 to 1).
#Schooling - Number of years of Schooling(years) .

str(lifexp)
summary(lifexp)
range(lifexp$Life.expectancy)



#Life expectancy distribution in our dataset 

hist(lifexp$Life.expectancy, breaks = seq(0,100,10),ylim=c(0,800), xlab = "Life Expectancy",
     main="Life expectancy over years for developed and developing countries", col = "gray50")
axis(1, at = seq(0, 100, by = 10))
summary(lifexp$Life.expectancy)


#Does Status of country have an effect on life expectancy?

Dev<- lifexp%>% filter(Status=="Developed") %>% group_by(Year) %>% summarise(total=mean(Life.expectancy))

Devloping<-lifexp%>% filter(Status=="Developing") %>% group_by(Year) %>% summarise(total=mean(Life.expectancy))

barplot(Dev$total,names.arg = Dev$Year,
        cex.axis = 0.65,ylim=c(0,100),space=c(0),xlab = "Years",
        col=rgb(1, 0, 0, .5),border = "Red",ylab="Life Expectancy (Years)",
        main="Life expectancy over years for developed and developing countries",)

barplot(Devloping$total,names.arg = Devloping$Year,
        cex.axis = 0.65,ylim=c(0,100),space=c(0),xlab = "Years",
        col=rgb(0, 1, 0, .05),border = "Green",ylab="Adult mortality rates",
        main="Life expectancy over years for developed and developing countries",
        add = TRUE)
legend("topright",
       c("Developed","Developing"),
       fill = c("orange","green"),cex=0.65,bty="n"
)

print("Over the years there isn't much difference in life expectancy though it differs significantly for developed and developing countries");


# Life expectancy for developed countries in 2014 

data_developed<- lifexp%>% filter(Status=="Developed" & Year==2014) 
dim(data_developed)

Full_data_developed<- Fullifexp%>% filter(Status=="Developed" & Year==2014) 
dim(Full_data_developed)

mean(data_developed$Life.expectancy)
plot((seq(1:19)), data_developed$Life.expectancy, type = "o", pch = 19, ylim=c(0,100),
     col = "red", ylab = "Life Expectancy (Age) ",
     xlab = "Sample observations",main="Life Expectancy Distribution for Developed countries in 2014")
axis(2, at = seq(0, 100, by = 10))
axis(1, at = seq(1, 19, by = 1))


# Can average life expectancy for developed countries in 2014 be said as 80 ?

#Null hypothesis life expectancy for developed countries in 2014 = 80
isAccept<- function(tcal,dF,alpha,isTwotailed){
  ttable<-0;
  if(isTwotailed){
    ttable<-abs(qt(alpha/2,df=dF))
  }else{
    ttable<-abs(qt(alpha,df=dF))
  }
  tcal<-abs(tcal)
  if(tcal<ttable){
    cat("The calculated t value is ", tcal," is less than the table t value ",ttable,"\n")
    cat("Hence the null hypothesis is accepted")
  }else{
    cat("The calculated t value is ", tcal," is greater than the table t value ",ttable,"\n")
    cat("Hence the null hypothesis is rejected")
  }
}

res<-t.test(data_developed$Life.expectancy,alt="two.sided",mu=80,conf.level=0.95)
res
isAccept(res$statistic,res$parameter,0.05,TRUE)


#Developing countries in 2014

data_developing<- lifexp%>% filter( Status=="Developing" & Year==2014)
dim(data_developing)

Full_data_developing<- Fullifexp%>% filter(Status=="Developing" & Year==2014) 
dim(Full_data_developing)



hist(data_developing$Life.expectancy, breaks = seq(20,100,5), xlab = "Age",
     main="Histogram of Life Expectancy Distribution for developing countries", col = "gray50")
axis(1, at = seq(0, 100, by = 5))


#Null hypothesis : the life expectancy of population is 70


#is distribution normal ?
qqnorm(data_developing$Life.expectancy)
qqline(data_developing$Life.expectancy)


#Z test 
isAccept<- function(zcal,alpha,isTwotailed){
  ztable<-0;
  if(isTwotailed){
    ztable<-abs(qnorm(alpha/2))       
  }else{
    ztable<-abs(qnorm(alpha))
  }
  zcal<-abs(zcal)
  if(zcal<ztable){
    cat("The calculated z value is ", zcal," is less than the table z value ",ztable,"\n")
    cat("Hence the null hypothesis is accepted")
  }else{
    cat("The calculated z value is ", zcal," is greater than the table z value ",ztable,"\n")
    cat("Hence the null hypothesis is rejected")
  }
}

sd<-sd(data_developing$Life.expectancy)
z<- (mean(data_developing$Life.expectancy)-70)/sd

isAccept(z,0.05,TRUE)  # for 95% confidence interval


#Top 10 countries which have highest improvement of life expectancy in 2014 as compared to 2000?

high_lifexp<-lifexp%>% filter((Year==2014 | Year == 2000) &Life.expectancy!=0 ) %>%group_by(Country,Year) %>% summarise(Life.expectancy) %>% arrange(desc(Life.expectancy))
View(high_lifexp)
high_lifexp$Year<- as.factor(high_lifexp$Year)

high_lifexp_s<-high_lifexp %>% spread(Year,Life.expectancy) 
View(high_lifexp_s)

high_lifexp_s<- high_lifexp_s%>% mutate(diff= (`2014`- `2000`))
high_lifexp_s <- high_lifexp_s %>% drop_na()
View(high_lifexp_s)

#did any country had a fall in life expectancy in 2014 compared to 2000
high_lifexp_s%>% filter(diff<=0)  
#Brazil and Romania had a fall in thier life expectancy

top_improved<-high_lifexp_s%>% arrange(desc(diff))
top_improved<-top_improved[1:10,]
View(top_improved) #top improved 

barplot(top_improved$`2000`,names.arg = top_improved$Country,
        cex.axis = 0.65,ylim=c(0,100),space=c(0),xlab = "Countries",
        col=rgb(1, 0, 0, .5),border = "Red",ylab="Life Expectancy (Years)")

barplot(top_improved$`2014`,names.arg = top_improved$Country,
        cex.axis = 0.65,ylim=c(0,100),space=c(0),xlab = "Countries",
        col=rgb(0, 1, 0, .05),border = "Green",ylab="Adult mortality rates",
        main="Life expectancy for different countries for year 2000 and 2014",
        add = TRUE)

legend("topleft",
       c("2000","2014"),
       fill = c("orange","green"),cex=0.75,bty="n"
)



#adult mortality over years in developed and developing countries

morDev<- lifexp%>% filter(Status=="Developed") %>% group_by(Year) %>% summarise(total=mean(Adult.Mortality))
View(morDev)
morDevloping<-lifexp%>% filter(Status=="Developing") %>% group_by(Year) %>% summarise(total=mean(Adult.Mortality))

barplot(morDev$total,names.arg = morDev$Year,
        cex.axis = 0.65,ylim=c(0,250),space=c(0),xlab = "Years",
        col=rgb(1, 0, 0, .5),border = "Red",ylab="Adult mortality rates",main="Adult mortality rates over years")

barplot(morDevloping$total,names.arg = morDevloping$Year,
        cex.axis = 0.65,ylim=c(0,250),space=c(0),xlab = "Years",
        col=rgb(0, 1, 0, .05),border = "Green",ylab="Adult mortality rates",
        main="Avg adult mortality rates over years for developed and developing countries",
        add = TRUE)
legend("topright",
       c("Developed","Developing"),
       fill = c("orange","green"),cex=0.75,bty="n"
)
#Average adult mortality rates over the years have not changed significantly but 
#its quite low in developed countries as compared to developing countries


#life exp in dev and devloping countries

boxplot(lifexp$Life.expectancy~lifexp$Status,xlab="Country Status",
        ylab = "Life Expectancy (Age)",main="Life Expectancy For Developed And Developing Countries",
        col = c("green","red"),
        names = c("Developed","Developing"))

#vaccination status

lifexp1 <- lifexp %>% 
  mutate(Hepatitis.B = ifelse(Hepatitis.B < 90, "lt80", "mt80"),
         Polio = ifelse(Polio < 80, "lt80", "mt80"),
         Diphtheria = ifelse(Diphtheria < 80, "lt80", "mt80"),
         Hepatitis.B = as.factor(Hepatitis.B),
         Polio = as.factor(Polio),
         Diphtheria = as.factor(Diphtheria))


# corr between Hepatitis B Coverage and life expectancy

coeff<-cor(lifexp$Hepatitis.B,lifexp$Life.expectancy, method = "pearson")
coeff  #v low 

boxplot(lifexp1$Life.expectancy~lifexp1$Hepatitis.B,xlab="Hepatitis.B vaccine coverage",
        ylab = "Life Expectancy (Age)",main="Different countries Hepatitis.B vaccine coverage vs Life Expectancy ",
        col = c("red","green"),
        names = c("Less than 80% covered","More than 80% covered"))

# Polio Coverage and life expectancy

coeff<-cor(lifexp$Polio,lifexp$Life.expectancy, method = "pearson")
coeff  #

boxplot(lifexp1$Life.expectancy~lifexp1$Hepatitis.B,xlab="Polio vaccine coverage",
        ylab = "Life Expectancy (Age)",main="Different countries Polio vaccine coverage vs Life Expectancy ",
        col = c("red","green"),
        names = c("Less than 80% covered","More than 80% covered"))



coeff<-cor(lifexp$Diphtheria,lifexp$Life.expectancy, method = "pearson")
coeff 

boxplot(lifexp1$Life.expectancy~lifexp1$Diphtheria,xlab="Diphtheria vaccine coverage",
        ylab = "Life Expectancy (Age)",main="Different countries Diphtheria vaccine coverage vs Life Expectancy ",
        col = c("red","green"),
        names = c("Less than 80% covered","More than 80% covered"))



y <- lifexp1 %>% group_by(Status, Hepatitis.B) %>%  count()%>% spread(Hepatitis.B, n, fill = 0L) 
y
y<- y%>%  mutate( lt80p=(lt80/(lt80+mt80))*100,mt80p=(mt80/(lt80+mt80))*100  )

y
barplot(height = t(y[c("lt80p","mt80p" )]),
        names.arg = y$Status,col = c("red","green"),space=c(0),cex.axis = 0.65,xlab = "Country Status",ylab="Percentage ",main="Less than and greater than 80 % Hepatitis.B coverage in developed and developing countries")

legend("topleft",
       c("<80% Hepatitis.B coverage",">80% Hepatitis.B coverage"),
       fill = c("red","green"),cex=0.5
)


# Does the staus of country affect Hepatitis.B coverage significantly ?

#chi square test
#function to compare x value calculated and table x value
acceptChi<-function(Xcal,alpha,df){
  XTable<-qchisq(p=alpha,df,lower.tail=FALSE)
  if(abs(Xcal)<=abs(XTable)){
    cat("The calculated X value is ", abs(Xcal)," is less than the table X value ",abs(XTable),"\n")
    print("There isn't much difference in the groups of data ")
  }else{
    cat("The calculated X value is ", abs(Xcal)," is greater than the table X value ",abs(XTable),"\n")
    print("There is a significant difference in the groups of data")
  }
}


df<-y[c(4,5)]
df<-data.frame(df)
rownames(df) <- c("Developed","Developing")
#class(df)
#dim(df)
#str(df)
df
res<-chisq.test(df,correct=FALSE)
res
acceptChi(res$statistic,0.05,res$parameter)
# Hence development status affect the Hepatitis.B coverage


#Does developed or devloping status of country affect polio immunization?

y <- lifexp1 %>% group_by(Status, Polio) %>%  count()%>% spread(Polio, n, fill = 0L) 
y<- y%>%  mutate( lt80p=(lt80/(lt80+mt80))*100,mt80p=(mt80/(lt80+mt80))*100  )

#View(y)
barplot(height = t(y[c("lt80p","mt80p" )]),
        names.arg = y$Status,col = c("red","green"),space=c(0),cex.axis = 0.65,xlab = "Country Status",
        ylab="Percentage ",
        main="Less than and greater than 80 % Polio coverage in developed and developing countries")

legend("topleft",
       c("<80% Polio coverage",">80% Polio coverage"),
       fill =  c("red","green"),cex=0.5
)

#chi square test Does the staus of country affect Hepatitis.B coverage significantly ?

#function to compare x value calculated and table x value
acceptChi<-function(Xcal,alpha,df){
  XTable<-qchisq(p=alpha,df,lower.tail=FALSE)
  if(abs(Xcal)<=abs(XTable)){
    cat("The calculated X value is ", abs(Xcal)," is less than the table X value ",abs(XTable),"\n")
    print("There isn't much difference in the groups of data ")
  }else{
    cat("The calculated X value is ", abs(Xcal)," is greater than the table X value ",abs(XTable),"\n")
    print("There is a significant difference in the groups of data")
  }
}


df<-y[c(4,5)]
df<-data.frame(df)
rownames(df) <- c("Developed","Developing")
#class(df)
#dim(df)
#str(df)
res<-chisq.test(df,correct=FALSE)
res
acceptChi(res$statistic,0.05,res$parameter)
# Hence development status affect the Polio coverage


#Does developed or devloping status of country affect Diphteria immunization?

y <- lifexp1 %>% group_by(Status, Diphtheria) %>%  count()%>% spread(Diphtheria, n, fill = 0L) 
y<- y%>%  mutate( lt80p=(lt80/(lt80+mt80))*100,mt80p=(mt80/(lt80+mt80))*100  )

barplot(height = t(y[c("lt80p","mt80p" )]),
        names.arg = y$Status,col = c("red","green"),space=c(0),cex.axis = 0.65,xlab = "Country Status",
        ylab="Percentage ",
        main="Less than and greater than 80 % Diphtheria coverage in developed and developing countries")

legend("topleft",
       c("<80% Diphtheria coverage",">80% Diphtheria coverage"),
       fill =  c("red","green"),cex=0.5
)

#chi square test Does the staus of country affect Diphtheria coverage significantly ?

#function to compare x value calculated and table x value
acceptChi<-function(Xcal,alpha,df){
  XTable<-qchisq(p=alpha,df,lower.tail=FALSE)
  if(abs(Xcal)<=abs(XTable)){
    cat("The calculated X value is ", abs(Xcal)," is less than the table X value ",abs(XTable),"\n")
    print("There isn't much difference in the groups of data ")
  }else{
    cat("The calculated X value is ", abs(Xcal)," is greater than the table X value ",abs(XTable),"\n")
    print("There is a significant difference in the groups of data")
  }
}


df<-y[c(4,5)]
df<-data.frame(df)
rownames(df) <- c("Developed","Developing")
#class(df)
#dim(df)
#str(df)
res<-chisq.test(df,correct=FALSE)
res
acceptChi(res$statistic,0.05,res$parameter)
# Hence development status affect the Diphtheria coverage



#correlation
data_corr <- lifexp %>% select_if(is.numeric) 
coef<-cor(data_corr,method = "pearson")
View(coef)


#correlation btw life expectancy and schooling is v high 
coef<-cor(data_corr$Life.expectancy,data_corr$Schooling, method = "pearson")
cat("Pearson correlation between Life Expectancy and Schooling: ",coef)
plot(data_corr$Life.expectancy,data_corr$Schooling,xlab="No of years of schooling",ylab="Life expectancy");

pcor.test(data_corr$Life.expectancy,data_corr$Schooling,data_corr$Income.composition.of.resources,method = c("pearson"))$estimate
print("large decrease in correlation and hence life expectancy and schooling have spurious relationship")

coef<-cor(data_corr$Life.expectancy,data_corr$Income.composition.of.resources, method = "pearson")
coef
pcor.test(data_corr$Life.expectancy,data_corr$Income.composition.of.resources,data_corr$GDP,method = c("pearson"))$estimate
print("not a major difference hance life expectancy and income composition share a direct realtionship")

model<-lm(Life.expectancy ~ Income.composition.of.resources,data=lifexp)
plot(Life.expectancy ~ Income.composition.of.resources,data=lifexp,
     xlab="Income composition of resources",ylab="Life expectancy",col="gray50");
abline(reg=lm(Life.expectancy~Income.composition.of.resources,data=lifexp),col="red")
coeffs<-coefficients(model)
coeffs


#divide the datset into 2 for prediction 

set.seed(42)
rows <- sample(nrow(lifexp))

lifexp_100 <- lifexp[rows, ]
split <- round(nrow(lifexp_100) * 0.80)
lifexp_80<-lifexp_100[1:split, ]
lifexp_20<-lifexp_100[(split + 1):nrow(lifexp_100), ]
View(lifexp_80)
View(lifexp_20)


#linear regression

model<-lm(Life.expectancy ~ Income.composition.of.resources,data=lifexp_80)
plot(Life.expectancy ~ Income.composition.of.resources,data=lifexp_80,
     xlab="Income composition of resources",ylab="Life expectancy",col="gray50");
abline(reg=model,col="red")
coeffs<-coefficients(model)
coeffs
predicted<-predict(model,lifexp_20)

View(predicted)

error_info = data.frame(real =lifexp_20$Life.expectancy,prediction = predicted)

error_info<- error_info %>% mutate(error=error_info[,1] - error_info[,2] );
View(error_info)
mean_abs_err = mean(abs(error_info$error))
mean_abs_err
x<- as.factor(seq(1:330))
plot(x,error_info$error,type = "l",pch=1,ylim=c(-40,40),xlab="Test observations",ylab = "Error",main="Plot for error in predicted and real values ")
lines(x,rep(mean_abs_err,330),col="Blue")
legend("topleft",
       c("Each test observation error","Mean absolute error line"),
       fill = c("Black","blue"),cex=0.65,bty="n"
)
rmse = sqrt(mean(error_info$error ^ 2))
rmse


#multiple regression 

my_model<-lm(Life.expectancy ~ Income.composition.of.resources+Adult.Mortality+Alcohol+GDP+thinness..1.19.years+thinness.5.9.years,data=lifexp_80)
#plot(Life.expectancy ~ Income.composition.of.resources,data=lifexp_80,
 #    xlab="Income composition of resources",ylab="Life expectancy",col="gray50");
#abline(reg=my_model,col="red")
coeffs<-coefficients(my_model)
coeffs
predicted<-predict(my_model,lifexp_20)

#View(predicted)

error_info = data.frame(real =lifexp_20$Life.expectancy, prediction = predicted)


error_info<- error_info %>% mutate(error=error_info[,1] - error_info[,2] );
View(error_info)
mean_abs_err = mean(abs(error_info$error))
mean_abs_err
x<- as.factor(seq(1:330))
plot(x,error_info$error,type = "l",pch=1,ylim=c(-40,40),xlab="Test observations",ylab = "Error",main="Plot for error in predicted and real values ")
lines(x,rep(mean_abs_err,330),col="Blue")
legend("topleft",
       c("Each test observation error","Mean absolute error line"),
       fill = c("Black","blue"),cex=0.65,bty="n"
)

rmse = sqrt(mean(error_info$error ^ 2))
rmse
