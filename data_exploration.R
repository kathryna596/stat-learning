#Project stuff
setwd("d:/UTSA/Fall2020/DataMining/Project/stat-learning") # set working directory
library(sqldf) #needed to run sql queries on dataframes
install.packages("tidyverse") #needed to deal with times
library(lubridate) #^
library(readxl)
library(amelia)
df = read.csv('rawraw.csv',na.strings = c("","NA")) # read in raw csv data
temps = read_excel("Leaderboard - Yards 75 updated.xlsx",sheet = "temps") # read in additional temp data


#function to convert time to seconds
time_to_Sec <- function(x){
  per = ms(substr(x,1,5))
  sec = period_to_seconds(per)
  return(sec)
}

#Missing Age
missmap(df)
Completed<-amelia(df,m=1, idvars = c("Team", "Runner", "Run.Time", "Min", "Max", "Avg", "Nationality", "Gen", "Yard.1", "Yard.2", "Yard.3", "Yard.4", 
                                     "Yard.5", "Yard.6", "Yard.7", "Yard.8", "Yard.9", "Yard.10", "Yard.11", "Yard.12", "Yard.13", "Yard.14", "Yard.15",
                                     "Yard.16", "Yard.17", "Yard.18", "Yard.19", "Yard.20", "Yard.21", "Yard.22", "Yard.23", "Yard.24", "Yard.25",
                                     "Yard.26", "Yard.27", "Yard.28", "Yard.29", "Yard.30", "Yard.31", "Yard.32", "Yard.33", "Yard.34", "Yard.35", 
                                     "Yard.36", "Yard.37", "Yard.38", "Yard.39", "Yard.40", "Yard.41", "Yard.42", "Yard.43", "Yard.44", "Yard.45",
                                     "Yard.46", "Yard.47", "Yard.48", "Yard.49", "Yard.50", "Yard.51", "Yard.52", "Yard.53", "Yard.54", "Yard.55", 
                                     "Yard.56", "Yard.57", "Yard.58", "Yard.59", "Yard.60", "Yard.61", "Yard.62", "Yard.63", "Yard.64", "Yard.65",
                                     "Yard.66", "Yard.67", "Yard.68", "Yard.69", "Yard.70", "Yard.71", "Yard.72", "Yard.73", "Yard.74", "Yard.75")) 
write.amelia(obj=Completed, file.stem="outdata")
View(Completed$imputations$imp1)


#apply function to convert hours to seconds for easier math
time_cols = c(5,6,7,8,12:86)
df.seconds =data.frame(apply(df[time_cols],MARGIN = 2,time_to_Sec))

#add back original data
df.seconds$Runner = df$Runner
df.seconds$Team = df$Team
df.seconds$Yards = df$Yards
df.seconds$Nationality = df$Nationality

teamdf = split(df.seconds, df.seconds$Team) # team specific df

#avg analysis
# look into when 


#Rate analysis
# we want to see rate changes from lap to lap. 
# each runner has a unique number of total yards. 
# x - yard transition i.e.: 1 -> difference from yard 1 to yard 2
# y - the actual difference in seconds

rate_builder <- function(dFrame){
  #number of yards passed in with the dataframe
  rate <- dFrame
  for(i in 2:ncol(dFrame)){
    rate[,i] <- (dFrame[i] - dFrame[i-1])
  }
  return(rate)
}

#df[,grepl("Yard.",names(df))]  this will give you all the columns that are "Yard..." Warning it does include Total Yards

rate = rate_builder(teamdf$Australia[1,1:max(teamdf$Australia$Yards)+4])
rate = rate[-1] # get rid of first lap since there is nothing to compare it too
rate=data.matrix(rate)
dim(rate)

rate_length = length(rate)
rate_length/4

q1 = round(rate_length/4,digits = 0)
q2 = q1*2+1
q3 = q1 *3 +2
q4 = rate_length

#q = quantile(rate) don't use quantile because it sorts the data. BAD!

sd(rate[1,2:10])
sd(rate[q1:q2])
sd(rate[q2:q3])
sd(rate[q3:q4])


#looking at this graph we can start to see a lot of spread right before they end. How do we quantify this and look for that pattern. 

a =df[,grepl("Yard.",names(df))] #condense to just yard columns for time analysis
a = a[,colSums(is.na(a))<nrow(a)] #drop rows that are all na
a=subset(a,select = -c(Yards)) #drop count of total yards
a.sec =data.frame(apply(a,MARGIN = 2,time_to_Sec)) #convert time to seconds
#a.sec$Yards = df$Yards
b = rate_builder(a.sec) #build rate dataframe 
b= b[-1] #get rid of yard 1
b$Yards = df$Yards
b[1,]


plot.new()
plot(1:74,b[1,1:74], xlab = "lap #", ylab = "rate diff in seconds", type = "l", col=randomColor(10))
points(74,b[1,74])
for(i in 2:7){
  points(1:74,b[i,1:74],type = "l", col=randomColor(10, luminosity = "random"))
  points(b$Yards[i]-1,b[i,b$Yards[i]-1])
  }

#Need a way to find deviations for rates for percentages. 
c= rate_builder(a.sec)
c$Yards = df$Yards
#[rows,columns]
c.matrix = as.matrix(c[1:280,])
#need to complete more than 12 yards min.

for(i in 1:nrow(c)){
  i_length = c$Yards[i]
  q1 = round(i_length/4,digits = 0)
  q2 = q1*2+1
  q3 = q1 *3 +2
  q4 = i_length
  
  c$rate_sd1[i] = sd(c.matrix[1,2:q1])
  c$rate_sd2[i] = sd(c.matrix[1,q1:q2])
  c$rate_sd3[i] = sd(c.matrix[1,q2:q3])
  c$rate_sd4[i] = sd(c.matrix[1,q3:q4])
  
  c$avg_rate_q1[i] = mean(c.matrix[1,2:q1])
  c$avg_rate_q2[i] = mean(c.matrix[1,q1:q2])
  c$avg_rate_q3[i] = mean(c.matrix[1,q2:q3])
  c$avg_rate_q4[i] = mean(c.matrix[1,q3:q4])
  
  
  c$median_rate_q1[i] = median(c.matrix[1,2:q1])
  c$median_rate_q2[i] = median(c.matrix[1,q1:q2])
  c$median_rate_q3[i] = median(c.matrix[1,q2:q3])
  c$median_rate_q4[i] = median(c.matrix[1,q3:q4])
}


plot.new()
plot(c(25,50,75,100),c[17,grepl("rate_sd",names(c))],type="l",xlab = "%laps completd",ylab = "Std dev rate change",col=randomColor(10))
points(c(25,50,75,100),c[25,grepl("rate_sd",names(c))],type="l",col=randomColor(10))
points(c(25,50,75,100),c[1,grepl("rate_sd",names(c))],type="l",col=randomColor(10),lwd = 7)
points(c(25,50,75,100),c[50,grepl("rate_sd",names(c))],type="l",col=randomColor(10))
points(c(25,50,75,100),c[90,grepl("rate_sd",names(c))],type="l",col=randomColor(10))
points(d$rate_sd1[1],d$Yards[1],pch=0:25)
points(c(25,50,75,100),c[150,grepl("rate_sd",names(c))],type="l",col=randomColor(10))
points(c(25,50,75,100),c[280,grepl("rate_sd",names(c))],type="l",col=randomColor(10),lwd=2)

d =c[1:280,]
d =d[-1]

lm.1 = lm(Yards~rate_sd1,data = d)

plot.new()
plot(Yards~rate_sd1, data = d,col=randomColor(10))
points(d$rate_sd1[1],d$Yards[1],pch=0:25)
plot.new()
plot(Yards~rate_sd2, data = d,col=randomColor(10))
points(d$rate_sd2[1],d$Yards[1],pch=0:25)
plot.new()
plot(Yards~rate_sd3, data = d,col=randomColor(10))
points(d$rate_sd3[1],d$Yards[1],pch=0:25)
plot.new()
plot(Yards~rate_sd4, data = d,col=randomColor(10))
points(d$rate_sd4[1],d$Yards[1],pch=0:25)

d$rate_sd_diff_4_1 = d$rate_sd4-d$rate_sd1

plot.new()
plot(Yards~rate_sd_diff_4_1, data = d,col=randomColor(10))
points(d$rate_sd_diff_4_1[1],d$Yards[1],pch=0:25)
points(d$rate_sd_diff_4_1[280],d$Yards[280],pch=0:25)

d$Runner = df$Runner[1:280]
d$Team = df$Team[1:280]
d$Nationality = df$Nationality[1:280]

#work on getting temps into dataframe

temps$Team = temps$Country
e<- dplyr::left_join(d,temps,by="Team")


# creating dummy variables for teams

df$Australia =ifelse(df$Team == "Australia",1,0)
df$Belarus = ifelse(df$Team =="Belarus",1,0)
df$Belgium = ifelse(df$Team == "Belgium", 1,0)
df$Canada = ifelse(df$Team == "Canada", 1,0)
df$Denmark = ifelse(df$Team =="Denmark",1,0)
df$Finland = ifelse(df$Team == "Finland", 1,0)
df$France = ifelse(df$Team == "France", 1,0)
df$Germany = ifelse(df$Team =="Germany",1,0)
df$India = ifelse(df$Team == "India", 1,0)
df$Ireland = ifelse(df$Team == "Ireland", 1,0)
df$Japan = ifelse(df$Team =="Japan",1,0)
df$Mexico = ifelse(df$Team == "Mexico", 1,0)
df$NZ = ifelse(df$Team == "New Zealand", 1,0)
df$Russia = ifelse(df$Team =="Russia",1,0)
df$Singapore = ifelse(df$Team =="Singapore",1,0)
df$Spain = ifelse(df$Team == "Spain", 1,0)
df$Sweden = ifelse(df$Team == "Swede", 1,0)
df$Switzerland = ifelse(df$Team == "Switzerland", 1,0)
df$Ukraine = ifelse(df$Team == "Ukraine", 1,0)
df$UK= ifelse(df$Team == "United Kingdom", 1,0)
df$US= ifelse(df$Team == "United States", 1,0)

 
# Creating dummy varaibles for age groups

df$Age14_19 = ifelse(df$Age >=15 & df$Age<=19 ,1,0)
df$Age20_24 = ifelse(df$Age >=20 & df$Age<=24,1,0)
df$Age25_29 = ifelse(df$Age >=25 & df$Age<=29, 1,0)
df$Age30_34 = ifelse(df$Age >=30 & df$Age<=34, 1,0)
df$Age35_39 = ifelse(df$Age >=35 & df$Age<=39,1,0)
df$Age40_44 = ifelse(df$Age >=40 & df$Age<=44, 1,0)
df$Age45_49 = ifelse(df$Age >=45 & df$Age<=49, 1,0)
df$Age50_54 = ifelse(df$Age >=50 & df$Age<=54,1,0)
df$Age55_59 = ifelse(df$Age >=55 & df$Age<=59, 1,0)
df$Age60_64 = ifelse(df$Age >=60 & df$Age<=64, 1,0)

#View(df)
