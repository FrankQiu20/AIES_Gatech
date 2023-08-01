data<-read.csv("toxity_per_attribute.csv",header = T)
colnames(data)
head(data)
sum(data[1,3:52])
remove<-rep(nrow(data),0)

for (i in 1:nrow(data)) {
  if(sum(data[i,3:52],na.rm = T)==0){
    remove[i] = 1
  } else{remove[i]=0}
}
sum(remove)

remove<-ifelse(remove==1,FALSE,TRUE)

dat_sub<-data[remove,]
dat_sub<-rbind(dat_sub)
head(dat_sub)

###sex
dat_sub2<-dat_sub
dat_sub2$male<-ifelse(dat_sub2$male==FALSE,0,1)
dat_sub2$female<-ifelse(dat_sub2$female==FALSE,0,2)

dat_sub2$lesbian<-ifelse(dat_sub2$lesbian==FALSE,0,3)
dat_sub2$gay<-ifelse(dat_sub2$gay==FALSE,0,3)
dat_sub2$bisexual<-ifelse(dat_sub2$bisexual==FALSE,0,3)
dat_sub2$transgender<-ifelse(dat_sub2$transgender==FALSE,0,3)
dat_sub2$trans<-ifelse(dat_sub2$trans==FALSE,0,3)
dat_sub2$queer<-ifelse(dat_sub2$queer==FALSE,0,3)
dat_sub2$lgbt<-ifelse(dat_sub2$lgbt==FALSE,0,3)
dat_sub2$lgbtq<-ifelse(dat_sub2$lgbtq==FALSE,0,3)
dat_sub2$homosexual<-ifelse(dat_sub2$homosexual==FALSE,0,3)
dat_sub2$straight<-ifelse(dat_sub2$straight==FALSE,0,3)
dat_sub2$heterosexual<-ifelse(dat_sub2$heterosexual==FALSE,0,3)
dat_sub2$nonbinary<-ifelse(dat_sub2$nonbinary==FALSE,0,3)

sex<-cbind(dat_sub2$male,dat_sub2$female,dat_sub2$lesbian,dat_sub2$gay,dat_sub2$bisexual,
           dat_sub2$transgender,dat_sub2$trans,dat_sub2$queer,dat_sub2$lgbt,
           dat_sub2$lgbtq,dat_sub2$homosexual,dat_sub2$straight,dat_sub2$heterosexual,
           dat_sub2$nonbinary)

table(apply(sex, 1, sum))

dat_sub2$Sex2<-apply(sex, 1, sum)
table(dat_sub2$Sex2)

##Race
dat_sub2$black<-ifelse(dat_sub2$black==FALSE,0,1)
dat_sub2$white<-ifelse(dat_sub2$white==FALSE,0,2)
race<-cbind(dat_sub2$black,dat_sub2$white)
table(apply(race,1,sum))
dat_sub2$Race2<-apply(race,1,sum)
table(dat_sub2$Race2)

##Age
dat_sub2$young<-ifelse(dat_sub2$young==FALSE,0,1)
dat_sub2$younger<-ifelse(dat_sub2$younger==FALSE,0,1)
dat_sub2$teenage<-ifelse(dat_sub2$teenage==FALSE,0,1)
dat_sub2$millenial<-ifelse(dat_sub2$millenial==FALSE,0,1)

dat_sub2$middle.aged<-ifelse(dat_sub2$middle.aged==FALSE,0,2)

dat_sub2$old<-ifelse(dat_sub2$old==FALSE,0,3)
dat_sub2$older<-ifelse(dat_sub2$older==FALSE,0,3)
dat_sub2$elderly<-ifelse(dat_sub2$elderly==FALSE,0,3)

age<- cbind(dat_sub2$young, dat_sub2$younger,dat_sub2$teenage,dat_sub2$millenial,
            dat_sub2$middle.aged,dat_sub2$old,dat_sub2$older,dat_sub2$elderly)
table(apply(age,1,sum))
dat_sub2$Age2<-apply(age,1,sum)
table(dat_sub2$Age2)

###religion
dat_sub2$christian<-ifelse(dat_sub2$christian==FALSE,0,1)
dat_sub2$catholic<-ifelse(dat_sub2$catholic==FALSE,0,1)
dat_sub2$protestant<-ifelse(dat_sub2$protestant==FALSE,0,1)

dat_sub2$muslim<-ifelse(dat_sub2$muslim==FALSE,0,2)
dat_sub2$jewish<-ifelse(dat_sub2$jewish==FALSE,0,2)
dat_sub2$sikh<-ifelse(dat_sub2$sikh==FALSE,0,2)

dat_sub2$buddhist<-ifelse(dat_sub2$buddhist==FALSE,0,3)
dat_sub2$taoist<-ifelse(dat_sub2$taoist==FALSE,0,3)

religion<-cbind(dat_sub2$christian,dat_sub2$catholic,dat_sub2$protestant,
                dat_sub2$muslim,dat_sub2$jewish,dat_sub2$sikh,
                dat_sub2$buddhist,dat_sub2$taoist)
table(apply(religion, 1, sum))

dat_sub2$Religion2<-apply(religion, 1, sum)

####National origin
dat_sub2$african.american <- ifelse(dat_sub2$african.american==FALSE,0,1)
dat_sub2$hispanic<- ifelse(dat_sub2$hispanic==FALSE,0,1)
dat_sub2$latino <- ifelse(dat_sub2$latino==FALSE,0,1)
dat_sub2$latina <- ifelse(dat_sub2$latina==FALSE,0,1)
dat_sub2$latinx <- ifelse(dat_sub2$latinx==FALSE,0,1)
dat_sub2$american <- ifelse(dat_sub2$american==FALSE,0,1)
dat_sub2$mexican<- ifelse(dat_sub2$mexican==FALSE,0,1)

dat_sub2$canadian<- ifelse(dat_sub2$canadian==FALSE,0,1)

dat_sub2$european <- ifelse(dat_sub2$european==FALSE,0,2)
dat_sub2$middle.eastern <- ifelse(dat_sub2$middle.eastern==FALSE,0,2)
dat_sub2$african<- ifelse(dat_sub2$african==FALSE,0,2)

dat_sub2$chinese <- ifelse(dat_sub2$chinese==FALSE,0,3)
dat_sub2$asian <- ifelse(dat_sub2$asian==FALSE,0,3)
dat_sub2$indian <- ifelse(dat_sub2$indian==FALSE,0,3)
dat_sub2$japanese <- ifelse(dat_sub2$japanese==FALSE,0,3)

Nat_ori<-cbind(dat_sub2$african.american,dat_sub2$hispanic,dat_sub2$latino,
               dat_sub2$latina,dat_sub2$latinx, dat_sub2$american,dat_sub2$mexican,
               dat_sub2$canadian,dat_sub2$european,dat_sub2$middle.eastern,
               dat_sub2$african,dat_sub2$chinese,dat_sub2$asian,dat_sub2$indian,
               dat_sub2$japanese)



table(apply(Nat_ori, 1, sum))
##1514 4
which(apply(Nat_ori, 1, sum)==4)
Nat_ori[which(apply(Nat_ori, 1, sum)==4),]
colSums(Nat_ori[which(apply(Nat_ori, 1, sum)==4),])


dat_sub2$Nat_ori2<-apply(Nat_ori, 1, sum)
table(dat_sub2$Nat_ori2)
dat_sub2$Nat_ori2[dat_sub2$Nat_ori2==4] <-1
table(dat_sub2$Nat_ori2)

####Disability
dat_sub2$blind<-ifelse(dat_sub2$blind==FALSE,0,1)
dat_sub2$deaf<-ifelse(dat_sub2$deaf==FALSE,0,1)
dat_sub2$paralyzed<-ifelse(dat_sub2$paralyzed==FALSE,0,1)

dis<-cbind(dat_sub2$blind,dat_sub2$deaf,dat_sub2$paralyzed)
table(apply(dis, 1, sum))
dat_sub2$Dis2<-apply(dis, 1, sum)
table(dat_sub2$Dis2)
####
cor(dat_sub2$Sex2,dat_sub2$TOXICITY)
cor(dat_sub2$Race2,dat_sub2$TOXICITY)
cor(dat_sub2$Age2,dat_sub2$TOXICITY)

cor(dat_sub2$Nat_ori2,dat_sub2$TOXICITY)
cor(dat_sub2$Religion2,dat_sub2$TOXICITY)
cor(dat_sub2$Dis2,dat_sub2$TOXICITY)

####
library(dplyr)
library(ggplot2)
d_sex<-dat_sub2 %>% group_by(Sex2) %>% summarise(mean_tox = mean(TOXICITY))
ggplot(d_sex)+aes(x = Sex2, y = mean_tox) +geom_point()+xlab("Sex")+ylim(0,1)+
  theme_bw()

d_nat<-dat_sub2 %>% group_by(Nat_ori2) %>% summarise(mean_tox = mean(TOXICITY))
ggplot(d_nat)+aes(x = Nat_ori2, y = mean_tox) +geom_point()+xlab("National Origin")+ylim(0,1)+
  theme_bw()

d_age<-dat_sub2 %>% group_by(Age2) %>% summarise(mean_tox = mean(TOXICITY))
ggplot(d_age)+aes(x = Age2, y = mean_tox) +geom_point()+xlab("Age")+ylim(0,1)+
  theme_bw()


####
mean(dat_sub2$TOXICITY)
n = length(dat_sub2$TOXICITY)
sqrt((n-1)/n)*sd(dat_sub2$TOXICITY)
(1.96*0.362)/sqrt(75700)
0.55+c(-1,1)*(1.96*0.362)/sqrt(75700)
0.55+c(-1,1)*(2*0.362)


mean(dat_sub2[sample(nrow(dat_sub2),0.1*nrow(dat_sub2)),]$TOXICITY)
sd(dat_sub2[sample(nrow(dat_sub2),0.1*nrow(dat_sub2)),]$TOXICITY)
0.3620451*sqrt((7570-1)/7570)
(1.96*0.362)/sqrt(7570)



mean(dat_sub2[sample(nrow(dat_sub2),0.6*nrow(dat_sub2)),]$TOXICITY)
sd(dat_sub2[sample(nrow(dat_sub2),0.6*nrow(dat_sub2)),]$TOXICITY)
0.362*sqrt((45420-1)/45420)
(1.96*0.362)/sqrt(45420)

####
mean(dat_sub2[which(dat_sub2$Sex2!=0),]$TOXICITY)
length(dat_sub2[which(dat_sub2$Sex2!=0),]$TOXICITY)
sd(dat_sub2[which(dat_sub2$Sex2!=0),]$TOXICITY)*sqrt(21195/21196)
(0.324*1.96)/sqrt(21196)

dat10<-dat_sub2[sample(nrow(dat_sub2),0.1*nrow(dat_sub2)),]
length(dat10[which(dat10$Sex2!=0),]$TOXICITY)
mean(dat10[which(dat10$Sex2!=0),]$TOXICITY)
sd(dat10[which(dat10$Sex2!=0),]$TOXICITY)
sd(dat10[which(dat10$Sex2!=0),]$TOXICITY)*sqrt(2098/2099)
(0.332*1.96)/sqrt(2099)
0.645+c(-1,1)*0.014

dat60<-dat_sub2[sample(nrow(dat_sub2),0.6*nrow(dat_sub2)),]
mean(dat60[which(dat60$Sex2!=0),]$TOXICITY)
length(dat60[which(dat60$Sex2!=0),]$TOXICITY)
sd(dat60[which(dat60$Sex2!=0),]$TOXICITY)
sd(dat60[which(dat60$Sex2!=0),]$TOXICITY)*sqrt(12815/12816)
(0.323*1.96)/sqrt(12816)
0.652+c(-1,1)*0.006
######
mean(dat_sub2[which(dat_sub2$Sex2==1),]$TOXICITY)
length(dat_sub2[which(dat_sub2$Sex2==1),]$TOXICITY)
sd(dat_sub2[which(dat_sub2$Sex2==1),]$TOXICITY)
sd(dat_sub2[which(dat_sub2$Sex2==1),]$TOXICITY)*sqrt(1513/1514)
(0.338*1.96)/sqrt(1514)


mean(dat10[which(dat10$Sex==1),]$TOXICITY)
sd(dat10[which(dat10$Sex2==1),]$TOXICITY)
length(dat10[which(dat10$Sex2==1),]$TOXICITY)
sd(dat10[which(dat10$Sex2==1),]$TOXICITY)*sqrt(188/189)
(0.335*1.96)/sqrt(189)
0.557+c(-1,1)*0.048


mean(dat60[which(dat60$Sex2==1),]$TOXICITY)
sd(dat60[which(dat60$Sex2==1),]$TOXICITY)
length(dat60[which(dat60$Sex2==1),]$TOXICITY)
sd(dat60[which(dat60$Sex2==1),]$TOXICITY)*sqrt(936/937)
(0.337*1.96)/sqrt(937)
0.591+c(-1,1)*0.022

####
mean(dat_sub2[which(dat_sub2$Sex2==2),]$TOXICITY)
length(dat_sub2[which(dat_sub2$Sex2==2),]$TOXICITY)
sd(dat_sub2[which(dat_sub2$Sex2==2),]$TOXICITY)
sd(dat_sub2[which(dat_sub2$Sex2==2),]$TOXICITY)*sqrt(1513/1514)
(0.337*1.96)/sqrt(1514)


mean(dat10[which(dat10$Sex==2),]$TOXICITY)
sd(dat10[which(dat10$Sex2==2),]$TOXICITY)
length(dat10[which(dat10$Sex2==2),]$TOXICITY)
sd(dat10[which(dat10$Sex2==2),]$TOXICITY)*sqrt(135/136)
(0.36*1.96)/sqrt(136)
0.602+c(-1,1)*0.061


mean(dat60[which(dat60$Sex2==2),]$TOXICITY)
sd(dat60[which(dat60$Sex2==2),]$TOXICITY)
length(dat60[which(dat60$Sex2==2),]$TOXICITY)
sd(dat60[which(dat60$Sex2==2),]$TOXICITY)*sqrt(913/914)
(0.339*1.96)/sqrt(914)
0.594+c(-1,1)*0.022

####
mean(dat_sub2[which(dat_sub2$Sex2==3),]$TOXICITY)
n<-length(dat_sub2[which(dat_sub2$Sex2==3),]$TOXICITY)
sd(dat_sub2[which(dat_sub2$Sex2==3),]$TOXICITY)
sd(dat_sub2[which(dat_sub2$Sex2==3),]$TOXICITY)*sqrt((n-1)/n)
(0.321*1.96)/sqrt(n)


mean(dat10[which(dat10$Sex==3),]$TOXICITY)
sd(dat10[which(dat10$Sex2==3),]$TOXICITY)
n<-length(dat10[which(dat10$Sex2==3),]$TOXICITY)
sd(dat10[which(dat10$Sex2==3),]$TOXICITY)*sqrt((n-1)/n)
(0.327*1.96)/sqrt(n)
0.658+c(-1,1)*0.015


mean(dat60[which(dat60$Sex2==3),]$TOXICITY)
sd(dat60[which(dat60$Sex2==3),]$TOXICITY)
n<-length(dat60[which(dat60$Sex2==3),]$TOXICITY)
sd(dat60[which(dat60$Sex2==3),]$TOXICITY)*sqrt((n-1)/n)
(0.32*1.96)/sqrt(n)
0.662+c(-1,1)*0.006

#####
plot_dat<-data.frame(x=c(rep(c("population","sex","male","female","Others"),each=2)),
                     y=c(0.55,0.362,0.650,0.324,0.585,0.338,0.603,0.337,0.660,0.321),
                     cat=c(rep(c("Mean","Standard Deviation"),5)))

ggplot(plot_dat)+aes(x=x,y=y)+geom_point(aes(color=factor(cat)))+ylim(0,1)+
  geom_line(aes(group=factor(cat)))+
  theme_bw()
