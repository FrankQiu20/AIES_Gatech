data<-read.csv("deaths-in-custody.csv",header = T)
data$race<-factor(data$race)
levels(data$race)<-c("other","other","Black",rep("other",14),"white")
table(data$race,data$manner_of_death)

library(ggplot2)
ggplot(data) + aes(race, fill=manner_of_death)+
  geom_bar(position = "dodge")+theme_bw()
table(data$gender,data$manner_of_death)
ggplot(data) + aes(gender, fill=manner_of_death)+
  geom_bar(position = "dodge")+theme_bw()

data$age<-as.numeric(as.character(data$age))
summary(data$age)

data$age_group<-ifelse(data$age<=49,"<=49",">49")
table(data$age_grp,data$manner_of_death)
ggplot(na.omit(data)) + aes(age_group, fill=manner_of_death)+
  geom_bar(position = "dodge")+theme_bw()


table(data$race,data$custody_status)

ggplot(na.omit(data)) + aes(race, fill=custody_status)+
  geom_bar(position = "dodge")+theme_bw()

table(data$gender,data$custody_status)

ggplot(na.omit(data)) + aes(gender, fill=custody_status)+
  geom_bar(position = "dodge")+theme_bw()

table(data$age_grp,data$custody_status)
ggplot(na.omit(data)) + aes(age_group, fill=custody_status)+
  geom_bar(position = "dodge")+theme_bw()

ggplot(data[which(data$custody_status=="Awaiting Booking"),]) + 
  aes(custody_status, fill=gender)+ ylim(0,50000)+
  geom_bar(position = "dodge")+theme_bw()+ylab("")

ggplot(data[which(data$custody_status=="Booked - No Charges Filed"),]) + 
  aes(custody_status, fill=gender)+ ylim(0,50000)+
  geom_bar(position = "dodge")+theme_bw()+ylab("")

ggplot(data[which(data$custody_status=="Awaiting Booking"),]) + 
  aes(custody_status, fill=gender)+
  geom_bar()+theme_bw()


ggplot(data[which(data$custody_status=="Booked - No Charges Filed"),]) + 
  aes(custody_status, fill=gender)+
  geom_bar()+theme_bw()

####
data$gen_num<-ifelse(data$gender=="Male",1,0)
summary(data$gen_num)

data1<-data[sample(8373,size=4187),]
summary(data1$gen_num)


table(data$gender)
table(data1$gender)

table(data1$gender,data1$custody_status)
ggplot(na.omit(data1)) + aes(gender, fill=custody_status)+
  geom_bar(position = "dodge")+theme_bw()
