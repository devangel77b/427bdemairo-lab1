library(ggplot2)
library(dplyr)

raw <- read.csv('demairo.csv',header=TRUE)
data <- tibble(raw)
data <- mutate(data,tsquared=t_s^2)

# make fig 2.1
fig21 <- ggplot(data,aes(x=x_in,y=t_s,color=type))+
      geom_point()+
      geom_smooth(method="lm",formula=y~I(x^0.5)+0,se=FALSE)+
      xlab('$x$, in')+
      ylab('$t$, \\unit{\\second}')+
      theme_bw(base_size=8)+
      theme(legend.position="inside",
	legend.position.inside=c(0.95,0.05),
	legend.justification.inside=c("right","bottom"),
	legend.key.size=unit(4,"pt"),
	legend.title=element_blank())
ggsave('fig21.svg',plot=fig21,width=3.4167,height=2,units="in")

# make fig 2.2
fig22 <- ggplot(data,aes(x=x_in,y=tsquared,color=type))+
      geom_point()+
      geom_smooth(method="lm",formula=y~x+0,se=FALSE)+
      xlab('$x$, in')+
      ylab('$t^2$, \\unit{\\second\\squared}')+
      theme_bw(base_size=8)+
      theme(legend.position="inside",
	legend.position.inside=c(0.95,0.05),
	legend.justification.inside=c("right","bottom"),
	legend.key.size=unit(4,"pt"),
	legend.title=element_blank())
ggsave('fig22.svg',plot=fig22,width=3.4167,height=2,units="in")


# get stats
model1 <- lm(tsquared~x_in+0,data)
model2 <- lm(tsquared~x_in:type+0,data)
print(anova(model1,model2))

# baseball 35.15611 +- 0.3
# pong 32.0038 +- 0.3