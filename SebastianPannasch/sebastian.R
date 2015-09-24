setwd('/home/rene/Arbeitsfläche/LMM/sebastian')
library(R.matlab)

data=data.frame(readMat('dM.mat'))

names(data)[c(1,2,3,4,8,16,18,19)]=c('subject','begin','end','duration','blink','amplitude','onset', 'pic')

# Col1: Vp
# Col4: fixation duration
# col8: Blink (wenn 1, dann Blink, also rausfiltern)
# col16: saccade amplitude
# col18: time trial onset
# col19: bildinfo (wenn 8 am Anfang = 8 Objekte, wenn 16 am Anfang = 16 Objekte) 

library(zoo)

data[ which(data == -99999, arr.ind = TRUE) ] = NA


data <- within(data, {
  pic = na.locf(pic)
  onset = na.locf(onset)
  time = begin - onset
  objects  = factor(as.numeric(substr(pic,start=1, stop=1)), labels=c('16', '8'))
  subject=factor(subject)
  }
)

head(data[,c('subject','begin','end','duration','blink','amplitude','onset', 'pic', 'time', 'objects')])

with(data, ftable(subject,  onset))

library(plyr)
data = ddply(data, .(subject), mutate, trial    = as.factor(as.numeric(as.factor(onset))) )   


d = data[,c('subject','begin','end','duration','blink','amplitude','onset', 'pic', 'time', 'objects','trial')]

d=subset(d, time>0 & blink == 0 & time<10000 & duration < 1000)


f <- function(time,a,b) {b * exp(a / time)}

fit <- nls(duration ~ b*exp(a/time), start = c(a=1, b=252), data=d) 
# plot(duration~time, data=d, pch='.')
# curve(f(x, a=coef(fit)['a'], b=coef(fit)['b']), add = TRUE, col="green", lwd=2) 


library(ggplot2)
ggplot(data = d, aes(x=time, y=duration, colour = trial, group=trial )) +
  geom_line(size=0.2) +
  #(size=1, shape=21, fill="white") +
 # geom_line(size=0.8,  aes(x=time, y=eta, group=exertype), colour = 'black', size=.5) +
  facet_wrap(~subject) +
  #geom_smooth(aes(group=1), method="loess",  color = "black", se=FALSE) +
  stat_smooth(aes(group=1), method = 'nls', formula = y ~ b*exp(a/x), se = FALSE, start = list(a=coef(fit)['a'],b=coef(fit)['b']), colour='black')+
  theme_bw()+
  theme(legend.position = "none")
  
# y = φ1 + (φ1 − φ2 )e−φ3 x
# The “asymptotic regression” model, SSasymp

