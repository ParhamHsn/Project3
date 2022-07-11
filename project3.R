library(tidyverse)
data <- read_csv("C:\\Users\\Parham\\Desktop\\projectData.csv")
data <- data%>%
  rename(SubstanceRelease = `Substance release`)
glimpse(data)
t<-data%>%
  group_by(Year,SubstanceRelease)%>%
  summarize(Cnt = n())
t
t%>%
  ggplot(aes(x=Year, y=Cnt,fill=SubstanceRelease)) +
  geom_bar(stat="identity")
t<-pivot_wider(
  t,
  names_from = SubstanceRelease,
  values_from = `Cnt`,
)
t

t2<-data%>%
  group_by(Province,SubstanceRelease)%>%
  summarize(Cnt = n())%>%
  arrange(desc(Cnt))
t2
t2%>%
  ggplot(aes(x=Province, y=Cnt,fill=SubstanceRelease)) +
  geom_bar(stat="identity")

t3<-data%>%
  group_by(Company,SubstanceRelease)%>%
  summarize(Cnt = n())
t3
t3[1:15,]%>%
  ggplot(aes(x=Company, y=Cnt,fill=SubstanceRelease)) +
  geom_bar(stat="identity")

t4<-data%>%
  group_by(Status,SubstanceRelease)%>%
  summarize(Cnt = n())%>%
  arrange(desc(Cnt))
t4
t4%>%
  ggplot(aes(x=Status, y=Cnt,fill=SubstanceRelease)) +
  geom_bar(stat="identity")

t5<-data%>%
  group_by(Significant,SubstanceRelease)%>%
  summarize(Cnt = n())%>%
  arrange(desc(Cnt))
t5
t5%>%
  ggplot(aes(x=Significant, y=Cnt,fill=SubstanceRelease)) +
  geom_bar(stat="identity")

t6<-data%>%
  group_by(Release.Type,SubstanceRelease)%>%
  summarize(Cnt = n())%>%
  arrange(desc(Cnt))
t6
t6%>%
  ggplot(aes(x=Release.Type, y=Cnt,fill=SubstanceRelease)) +
  geom_bar(stat="identity")

t7<-data%>%
  group_by(Substance,SubstanceRelease)%>%
  summarize(Cnt = n())%>%
  arrange(desc(Cnt))
t7
t7%>%
  ggplot(aes(x=Substance, y=Cnt,fill=SubstanceRelease)) +
  geom_bar(stat="identity")

data

table(data$SubstanceRelease,data$Release.Type)
table(data$SubstanceRelease,data$Significant)
chisq.test(table(data$SubstanceRelease,data$Significant))

data<-data%>%
  mutate(SubstanceRelease = ifelse(SubstanceRelease == "Yes",1,0),
         Significant = ifelse(Significant == "Yes",1,0))
fit<-glm(SubstanceRelease ~ Significant,family = binomial(link="logit"),data=data)
fit
summary(fit)

tb<-as.data.frame(table(data$SubstanceRelease,data$Substance))
tb<-as.tibble(tb)
tb
colnames(tb) =c("Substance Release" , "Substance","Freq")
tb<-tb%>%
  arrange(Freq)

fit1<-glm(SubstanceRelease ~ Latitude + Longitude ,family = binomial(link="logit"),data=data)
fit1
summary(fit1)


fit2<-glm(SubstanceRelease ~ Latitude + Longitude + Province,family = binomial(link="logit"),data=data)
fit2
summary(fit2)

n<-nrow(data)
n.train = trunc(0.7*n)
n.test = n - n.train
train = sample(1:n,n.train)
train.x = data[train,-16] 
train.y = data[train,16] 
test.x = data[-train,-16]
test.y = data[-train,16]

fit1<-glm(SubstanceRelease ~ Latitude + Longitude ,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit1
summary(fit1)
yhat<-round(predict.glm(fit1,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)

fit2<-glm(SubstanceRelease ~ Latitude + Longitude + Province,family = binomial(link="logit"),data=data)
fit2
summary(fit2)
yhat<-round(predict.glm(fit2,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)

fit3<-glm(SubstanceRelease ~ Latitude + Longitude + Province + Significant,family = binomial(link="logit"),data=data)
fit3
summary(fit3)
yhat<-round(predict.glm(fit3,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)


fit4<-glm(SubstanceRelease ~ Release.Type,family = binomial(link="logit"),data=data)
fit4
summary(fit4)
yhat<-round(predict.glm(fit4,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)


fit5<-glm(SubstanceRelease ~ Substance,family = binomial(link="logit"),data=data)
fit5
summary(fit5)
yhat<-round(predict.glm(fit5,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)
