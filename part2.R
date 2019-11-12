# Tan Kah Wang (29442826)
# read data into R
coral_data<-read.csv("data.csv")
str(coral_data)
# load library
library (ggplot2)
# rename the headers in data
names(coral_data)<-c("year","type","site","bleaching","longtitude","latitude")
# change the column "bleaching" to numeric form instead of % form
coral_data$bleaching<-as.numeric(sub("%","",coral_data$bleaching))
# plot the graph
p<-ggplot(coral_data,aes(year,bleaching)) +
geom_point(aes(shape=type,color=type)) +
facet_grid(type~latitude+site,labeller=label_context)

# adding smoother of polynomial type
q<-p+
geom_smooth(method="lm",color="black",size=0.2,formula=y~poly(x,2),se=FALSE)

# adding theme to polynomial smoother graph
r<-q+
  theme_bw()+
  ylab('% of bleaching')+
  xlab('Year')+
  theme(legend.position = "bottom", legend.direction = "horizontal")

r