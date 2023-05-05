library(tidyverse)
library(jtools)
library(kableExtra)
library(ggplot2)

##################### Reading and cleaning the raw data #########################

Player<-read.csv("data/2018-19_nba_player-statistics.csv")

payroll<-read.csv("data/2019-20_nba_team-payroll.csv")
payroll[]<-lapply(payroll,gsub,pattern="$",fixed=TRUE,replacement="")
payroll$salary<- as.numeric(gsub(",","",payroll$salary))

stat_1<-read.csv("data/2018-19_nba_team-statistics_1.csv")
stat_2 <-read.csv("data/2018-19_nba_team-statistics_2.csv")

playersalaries<-read.csv("data/2018-19_nba_player-salaries.csv")


#Merging Player and playersalaries together
player<-merge(Player,playersalaries,all = T)
player<-player%>%select(player_name,Pos,Age,Tm,PTS,salary)
head(player,7)%>%
  kbl(caption = "Portion of the data") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(latex_options = "hold_position")



######################## Exploratory analysis ##################################

## Checking for errors and missing values within the datasets 
missing=sapply(player, function(x) sum(is.na(x)))%>%data.frame()
names(missing)<-"NA"
missing%>%
  kbl(caption = "Missing") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(latex_options = "hold_position")

# Replacing missing values with mean
player$Age[is.na(player$Age)] <- mean(player$Age,na.rm = TRUE)
player$Tm[is.na(player$Tm)] <- mean(player$Tm,na.rm = TRUE)
player$PTS[is.na(player$PTS)] <- mean(player$PTS,na.rm = TRUE)
player$salary[is.na(player$salary)] <- mean(player$salary,na.rm = TRUE)
player<-na.omit(player)


##### Checking the distribution of variables

# Distribution of players' age
attach(player)
par(mfrow=c(1,2))
hist(Age)
boxplot(Age)
par(mfrow=c(1,1))

# Distribution of players' points
par(mfrow=c(1,2))
hist(PTS,main ="Histogram of points",xlab = "Player points" )
boxplot(PTS)
par(mfrow=c(1,1))

# Distribution of players' salary
attach(player)
par(mfrow=c(1,2))
hist(salary)
boxplot(salary)
par(mfrow=c(1,1))


##### Checking for relationships between variables

# Relationship between players' points and salary
ggplot(player, aes(x=salary, y=PTS)) + 
  geom_point(color="#E69F00")+
  geom_smooth(method=lm, se=FALSE)+ 
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Perfomance in points")+xlab("Salary") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="none")+ 
  labs(title = "Relationship between players' points and salary")

# Relationship between players' points and Age
ggplot(player, aes(x=Age, y=PTS)) + 
  geom_point(color="#E69F00")+
  geom_smooth(method=lm, se=FALSE)+ theme(plot.title = element_text(hjust = 0.5))+
  ylab("Perfomance in points")+
  xlab("Age") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="none")+ 
  labs(title = "Relationship between players' points and age")

# Relationship between age and positions
pp=aggregate(cbind(Age)~Pos,FUN=mean,player)
ggplot(pp, aes(x = reorder(Pos, -Age), y = Age))+ geom_bar(stat="identity")+ 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Age")+xlab("Field position") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="none")+ 
  labs(title = "Age vs field position")

# Relationship between salary and positions
pp=aggregate(cbind(salary)~Pos,FUN=mean,player)
ggplot(pp, aes(x = reorder(Pos, -salary), y = salary))+ 
  geom_bar(stat="identity")+ 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Salary")+xlab("Field position") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="none")+
  labs(title = "Salary vs field position")

# Relationship between perfomance(points) and positions
pp=aggregate(cbind(PTS)~Pos,FUN=mean,player)
ggplot(pp, aes(x = reorder(Pos, -PTS), y = PTS))+ geom_bar(stat="identity")+ 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Perfomance in points")+xlab("Field position") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="none")+ 
  labs(title = "Perfomance in points vs field position")


########################### Data modelling and results ################################

##### Data modelling 
model<-lm(PTS~salary+Age+Pos,data=player)
summary(model)

# Assumption checking 
plot(model)
par(mfrow=c(1,1))

#Model output
summary(model)
plot_summs(model)


#####  Player recommendations 

## Player for PF
PF=subset(player,player$Pos=="PF")
PF=head(PF%>%arrange(-PTS),5)
PF%>%
  kbl(caption = "Top 5 PF players") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(latex_options = "hold_position")

## Player for C
C=subset(player,player$Pos=="C")
C=head(C%>%arrange(-PTS),5)
C%>%
  kbl(caption = "Top 5 C players") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(latex_options = "hold_position")

## Player for PG
PG=subset(player,player$Pos=="PG")
PG=head(PG%>%arrange(-PTS),5)
PG%>%
  kbl(caption = "Top 5 PG players") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(latex_options = "hold_position")

## Player for SF
SF=subset(player,player$Pos=="SF")
SF=head(SF%>%arrange(-PTS),5)
SF%>%
  kbl(caption = "Top 5 SF players") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(latex_options = "hold_position")

## Player for SG
SG=subset(player,player$Pos=="SG")
SG=head(SG%>%arrange(-PTS),5)
SG%>%
  kbl(caption = "Top 5 SG players") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  kable_styling(latex_options = "hold_position")






























