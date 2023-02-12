#codes for figures in the main text
require(tidyverse)
require(randomForest)
require(Hmisc)
require(easynls)
require(rio)
require(dplyr)

## figure 1 ==========
### Fig.1b
ggplot(data, (aes(val, fill=sets)))+
  geom_histogram(binwidth=.5, alpha=.5, position="identity")+
  geom_vline(aes(xintercept= m1), color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept= m2), color="blue", linetype="dashed", size=1)+
  labs(x=quote(AOPD~("×"~10^4~plants~ha^-1)~or~Yield~(Mg~ha^-1)), y="Count")+
  theme_bw()+
  theme(panel.grid = element_blank())

### Fig.1c
ggplot()+
  geom_point(data, aes(x=obs, y=pred), alpha=0.4, size= 2, color= "#1E90FF")+
  geom_abline(intercept=0,slope=1,colour="DimGrey",size=0.6,linetype="solid")+
  geom_abline(intercept=er,slope=1,colour="brown",size=0.6,linetype="dashed")+
  geom_abline(intercept=-er,slope=1,colour="brown",size=0.6,linetype="dashed")+
  scale_x_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(5,15,5))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(5,15,5))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(size=16,vjust=-0.2),
        axis.text = element_text(size=16,color = 'black'),
        legend.position =c(2, 0.89))

### Fig.1d
ggplot(data, aes(x=optd, y=opty))+
  geom_point(size=2, color='#BEBEBE', shape=1, alpha=1)+
  geom_smooth(method = 'lm', formula = y ~ x, level=0.95,size=1,alpha=0.8, color='#00A277', fill='#DDF1EB')+
  scale_y_continuous(limits = c(0,22), breaks = seq(0,22,5))+
  theme_bw()+
  labs(x=quote(AOPD~("×"~10^4~plants~ha^-1)), y=quote(Yield~(Mg~ha^-1)))+
  theme(panel.grid = element_blank())



## figure 2 ==========
### Fig.2a 
#data <- df
df <- mutate(df, rr=ifelse(r>=0, '+', '-'))
df <- mutate(df, pp=ifelse(p < 0.001, '***', ifelse(p<0.01, '**', ifelse(p<0.05,'*', ''))))
#figure
ggplot(df)+
  geom_point(aes(y=region, x=fact, size=relat, fill=region, color = region),alpha=0.8, shape=21)+
  scale_size(range = c(5, 25), limits = c(0, 0.5),breaks = c(0.03, 0.15, 0.3), name=NULL,guide = "legend")+
  scale_fill_manual(values =c("#7CAE00","#F8766D","#00BFC4","#C77CFF"), breaks =c('NE','NCP','NW','SW'),guide ="none")+
  scale_color_manual(values =c("#7CAE00","#F8766D","#00BFC4","#C77CFF"), breaks =c('NE','NCP','NW','SW'),guide ="none")+
  geom_text(aes(y=region,x=fact), label = paste(df$rr), size=6)+
  geom_text(aes(y=region,x=fact), label = paste(df$pp), size=6, vjust = 1.5)+
  theme_light()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color="black", size=1, linetype=1),
        legend.position = 'right')

## relationships between indicators and OPDs
#def linear-plateau curve 
f1 <- function(x){a + b * (x - c) * (x <= c)}
#def quadratic curve
f2 <- function(x){c*x*x + b*x + a}
#def linear curve
f3 <- function(x){b*x + a}

### Fig.2b (Tmin)
ggplot(df,aes(x=Tmin, y=OPD))+
  geom_point(aes(shape=region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_smooth(aes(color=region),fill='#FFFFFF', method = 'lm', formula = y~x,level = 0,size=1.25,alpha=0)+
  scale_x_continuous(limits = c(10, 25), breaks = seq(12, 25, 4))+
  scale_y_continuous(expand=c(0,0), limits = c(0, 16), breaks = seq(0,15,5))+
  scale_shape_manual(values = c(3,4,2,1), breaks =c('NE','NCP','NW','SW'))+
  scale_color_manual(values = c( "#7CAE00","#F8766D","#00BFC4","#C77CFF"), breaks =c('NE','NCP','NW','SW'))+
  theme_bw()

### Fig.2c (Tmax)
#NCP
data <- select(filter(df, region=='NCP'), Tmax, OPD)
nlsplot(data)
fit <- nlsfit(data, model = 2)
fit$Parameters$OPD
a <- fit$Parameters$OPD[1]
b <- fit$Parameters$OPD[2]
c <- fit$Parameters$OPD[3]
xx <- seq(25, 32, 0.1)
yy <- f2(xx)
df.p1 <- data.frame(Tmax=xx, OPD=yy,region='NCP')
#SW
data <- select(filter(df, region=='SW'), Tmax, OPD)
fit <- nlsfit(data, model = 2)
fit$Parameters$OPD
a <- fit$Parameters$OPD[1]
b <- fit$Parameters$OPD[2]
c <- fit$Parameters$OPD[3]
xx <- seq(25, 32, 0.1)
yy <- f2(xx)
df.p2 <- data.frame(Tmax=xx, optd=yy, region='SW')
fig <- rbind(df.p1, df.p2)
ggplot()+
  geom_point(data=df, aes(x=Tmax, y=OPD,shape=region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_line(data=fig, aes(x=Tmax, y=OPD, color= region), size=1.25)

### Fig.2d (Radn)
ggplot(df1, aes(x=Radn, y=OPD))+
  geom_point(aes(shape=region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_smooth(aes(color=region, fill=region), method = 'lm', formula = y~x,alpha=0.2,
              size=1.25,level=0)+
  theme_bw()

### Fig.2e (Prec)

#NE
ne <- filter(df, region=='NE')
data <- select(ne, Prec, OPD)
fit <- nlsfit(data, model = 2)
fit$Parameters$OPD
a <- fit$Parameters$OPD[1]
b <- fit$Parameters$OPD[2]
c <- fit$Parameters$OPD[3]
xx <- seq(200, 1200, 1)
yy <- f2(xx)
df.p1 <- data.frame(Prec=xx, OPD=yy,region='NE')

#NW
nw <- filter(df, region=='NW')
data <- select(nw, Prec, OPD)
fit <- nlsfit(data, model = 2)
nlsplot(data, model=2)
fit$Parameters$OPD
a <- fit$Parameters$OPD[1]
b <- fit$Parameters$OPD[2]
c <- fit$Parameters$OPD[3]
xx <- seq(200, 700, 1)
yy <- f2(xx)
df.p2 <- data.frame(Prec=xx, OPD=yy,region='NW')

fig <- rbind(df.p1, df.p2)
ggplot()+
  geom_point(data=df, aes(x=Prec, y=OPD,shape=region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_line(data=fig, aes(x=Prec, y=OPD, color= region), size=1.25)


### Fig.2f (GDD)
ggplot(data=df,aes(x=GDD, y=optd))+
  geom_point(color = '#BEBEBE',alpha=0.8, shape =4)+
  geom_smooth(method = 'lm',formula = y~x, size=1.25, color = '#F8766D', fill = '#F8766D',level = 0,alpha=0.2)

### Fig.2g (SOM) 

#NE
df1 <- filter(df, region == 'NE')
data <- select(df1, SOM, OPD)
fit <- nlsfit(data, model = 3, start = c(8, 1,15))
fit$Parameters$OPD
a <- fit$Parameters$OPD[1]
b <- fit$Parameters$OPD[2]
c <- fit$Parameters$OPD[3]
xx <- seq(0, 70, 0.5)
yy <- f1(xx)
df.p1 <- data.frame(SOM=xx, OPD=yy,region='NE')

#NCP
df1 <- filter(df, region == 'NCP')
data <- select(df1, SOM, OPD)
fit <- nlsfit(data, model = 2)
fit$Parameters$OPD
a <- fit$Parameters$OPD[1]
b <- fit$Parameters$OPD[2]
c <- fit$Parameters$OPD[3]
xx <- seq(0, 30, 0.5)
yy <- f2(xx)
df.p2 <- data.frame(SOM=xx, OPD=yy,region='NCP')

#NW
df1 <- filter(df, region == 'NW')
data <- select(df1, SOM, OPD)
fit <- nlsfit(data, model = 1)
fit$Parameters$OPD
a <- fit$Parameters$OPD[1]
b <- fit$Parameters$OPD[2]
xx <- seq(0, 20, 0.5)
yy <- f3(xx)
df.p3 <- data.frame(SOM=xx, OPD=yy,region='NW')

df.p <- rbind(df.p1, df.p2, df.p3)

ggplot()+
  geom_point(data=df, aes(x=SOM, y=optd, shape = region), size=1.5, color= '#BEBEBE',alpha=0.8)+
  geom_line(data = df.p, aes(SOM, optd, color=region), size=1.25)+
  geom_vline(xintercept = 20, linetype=2, color='#8B8682', size = 1)



## figure 3 ===========
### Fig.3g and Fig.3h

ggplot(data, mapping = aes(x=groups, y=Density, fill=groups, group=factor(1)))+ #or y = Yield
  geom_bar(stat= "identity", width = 0.5)



## figure 4 ===========
### Fig.4b and Fig.4c
pd <- filter(df, name == 'Density')
gy <- filter(df, name == 'Yield')

ggplot(pd, aes(x=region, y=values))+
  geom_bar(aes(fill=treat),stat= "identity", position = 'dodge', width = 0.5)



## figure 5 ==========

### Fig.5a and b
### example for OPD
s0 <- data.frame(name = df$Name, region=df$region, OPD = df$`2010s.opd`, set='Baseline')
s1 <- data.frame(name = df$Name, region=df$region, OPD =df$`2010s.opdsom`, set='S1')
s2 <- data.frame(name = df$Name, region=df$region, OPD =df$`2030s.opd`, set='S2')
s3 <- data.frame(name = df$Name, region=df$region, OPD =df$`2030s.opdsom`, set='S3')

fig <- rbind(s0, s1, s2, s3)
fig$region <- factor(fig$region, levels = c('NE','NCP','NW','SW'), ordered = TRUE)

ggplot(fig, aes(x=region, y=OPD))+
  geom_boxplot(aes(fill= set), alpha=0.7, varwidth = FALSE, 
               outlier.colour="#D3D3D3", outlier.size=1.5, position=position_dodge(.9))+
  stat_summary(fun ='mean', geom = 'point', aes(group=set), color='black', position=position_dodge(0.9), pch=10, size=2)+
  scale_fill_manual(values =c('#008000','#00FF00','#FFBF66','#FF8000'), breaks =c('Baseline','S1','S2','S3'),
                    labels =c('2010s','2010s (soil improvement)','2030s','2030s (soil improvement)'))+
  theme_bw()

### Fig.5c and d
df1 <- filter(df, name=='historical')
df2 <- filter(df, (name=='trend')|(set=='AOPD')|(set=='SOM'))

# density
d1 <- filter(df2, name == 'trend')$density
d2 <- filter(df2, name == 'OPD')$density
d3 <- filter(df2, name == 'SOM')$density
ggplot()+
  geom_segment(aes(x = 2030, y = 8, xend = -Inf, yend = 8), size=0.5, linetype = 2, color='#D0D0D0') +
  geom_ribbon(aes(x = x1, ymin = d2, ymax = d3), alpha = 1, fill = "#FFDDCE")+  #"#C4E0F8"
  geom_ribbon(aes(x = x1, ymin = d1, ymax = d2), alpha = 1, fill = "#C4E0F8")+  #"#FFDDCE"
  geom_line(data=df1, aes(x=year, y=density, color = name), size=1, linetype = 1)+
  geom_line(data=df2, aes(x=year, y=density, color = name), size = 0.75, linetype = 1)

#yield
y1 <- filter(df2, set == 'trend')$yield
y2 <- filter(df2, set == 'OPD')$yield
y3 <- filter(df2, set == 'SOM')$yield
ggplot()+
  geom_segment(aes(x = 2030, y = 10.14, xend = -Inf, yend = 10.14), size=0.5, linetype = 2, color='#D0D0D0') +
  geom_ribbon(aes(x = x1, ymin = y2, ymax = y3), alpha = 1, fill = "#FFDDCE")+  #"#C4E0F8"
  geom_ribbon(aes(x = x1, ymin = y1, ymax = y2), alpha = 1, fill = "#C4E0F8")+  #"#FFDDCE"
  geom_line(data=df1, aes(x=year, y=yield, color = set), size=1, linetype = 1)+
  geom_line(data=df2, aes(x=year, y=yield, color = set), size = 0.75, linetype = 1)







