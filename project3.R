library(tidyverse)
data <- read_csv("C:\\Users\\Parham\\Desktop\\projectData.csv")
t<-data%>%
  group_by(Year,`Substance release`)%>%
  summarize(Cnt = n())
t
t%>%
  ggplot(aes(x=Year, y=Cnt,fill=`Substance release`)) +
  geom_bar(stat="identity")
t<-pivot_wider(
  t,
  names_from = `Substance release`,
  values_from = `Cnt`,
)
t

t2<-data%>%
  group_by(Province,`Substance release`)%>%
  summarize(Cnt = n())%>%
  arrange(desc(Cnt))
t2
t2%>%
  ggplot(aes(x=Province, y=Cnt,fill=`Substance release`)) +
  geom_bar(stat="identity")

t3<-data%>%
  group_by(Company,`Substance release`)%>%
  summarize(Cnt = n())
t3
t3[1:15,]%>%
  ggplot(aes(x=Company, y=Cnt,fill=`Substance release`)) +
  geom_bar(stat="identity")
