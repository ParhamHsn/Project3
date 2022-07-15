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

n.why.It.Happend<-c()
n.What.Happened<-c()
for(i in 1:1624)
{
  n.why.It.Happend <- c(n.why.It.Happend,length(strsplit(data$Why.It.Happened[i],",")[[1]]))
  n.What.Happened <- c(n.What.Happened,length(strsplit(data$What.Happened[i],",")[[1]]))
}
data<-data%>%
  mutate(n.why.It.Happend = n.why.It.Happend,
         n.What.Happened = n.What.Happened)


r = c()
for(i in 2:20)
{
  k = kmeans(cbind(data$Latitude,data$Longitude) , centers = i)
  r = c(r,k$tot.withinss / k$betweenss)
  
}
r
plot(2:20,r)


k = kmeans(cbind(data$Latitude,data$Longitude) , centers = 10)
k_m = as.factor(k$cluster)
data<-data%>%
  mutate(k = k_m)

fit<-glm(SubstanceRelease ~ k,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit
summary(fit)
yhat<-round(predict.glm(fit,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)

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

fit2<-glm(SubstanceRelease ~ Province,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit2
summary(fit2)
yhat<-round(predict.glm(fit2,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)

fit3<-glm(SubstanceRelease ~ Latitude + Longitude + Province + Significant,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit3
summary(fit3)
yhat<-round(predict.glm(fit3,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)


fit4<-glm(SubstanceRelease ~ Release.Type,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit4
summary(fit4)
yhat<-round(predict.glm(fit4,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)


fit5<-glm(SubstanceRelease ~ Substance,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit5
summary(fit5)
yhat<-round(predict.glm(fit5,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)

fit6<-glm(SubstanceRelease ~ Nearest.Populated.Centre,family = binomial(link="logit"),data=cbind(train.x,train.y))
#fit6
#summary(fit6)
yhat<-round(fitted.values(fit6))
tb<-table(yhat,as.data.frame(train.y)[,1])
sum(diag(tb))/sum(tb)

yhat<-round(predict.glm(fit6,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)



fit<-glm(SubstanceRelease ~ Year,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit
summary(fit)
yhat<-fitted.values(fit)
yhat<-round(predict.glm(fit,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)


fit<-glm(SubstanceRelease ~ Year + n.why.It.Happend,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit
summary(fit)
yhat<-fitted.values(fit)
yhat<-round(predict.glm(fit,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)


fit<-glm(SubstanceRelease ~ Year + Province + Status + Significant,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit
summary(fit)
yhat<-round(predict.glm(fit,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)




fit<-glm(SubstanceRelease ~ + n.What.Happened,family = binomial(link="logit"),data=cbind(train.x,train.y))
fit
summary(fit)
yhat<-round(predict.glm(fit,newdata = test.x,type = "response"))
tb<-table(yhat,as.data.frame(test.y)[,1])
sum(diag(tb))/sum(tb)
