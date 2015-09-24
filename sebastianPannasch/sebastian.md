---
title: "Sebastian"
author: "René"
date: "04.09.2014"
output: html_document
---

Explorative Datenanalyse. Hauptfrage: sollten die Daten anders gefiltert werden?

```{r}
setwd('/home/rene/Arbeitsfläche/LMM/sebastian')
library(R.matlab)

data=data.frame(readMat('dM.mat'))

names(data)[c(1,2,3,4,8,16,18,19)]=
  c('subject','begin','end','duration','blink','amplitude','onset', 'pic')

# Col1: Vp
# Col4: fixation duration
# col8: Blink (wenn 1, dann Blink, also rausfiltern)
# col16: saccade amplitude
# col18: time trial onset
# col19: bildinfo (wenn 8 am Anfang = 8 Objekte, wenn 16 am Anfang = 16 Objekte) 

# recode Missings 
library(zoo)
data[ which(data == -99999, arr.ind = TRUE) ] = NA

# recode data
data <- within(data, {
  pic      = na.locf(pic)   # write picture under sucessive trials
  onset    = na.locf(onset) # ...
  time     = begin - onset
  objects  = factor(as.numeric(substr(pic,start=1, stop=1)), labels=c('16', '8'))
  subject  = factor(subject)
  }
)

```

wie sehen die daten aus (erste sieben Zeilen)?
```{r}
head(data[,c('subject','begin','end','duration','blink','amplitude','onset', 'pic', 'time', 'objects')])

# with(data, ftable(subject,  onset))
```

Kodiere trials innerhalb einer VP
```{r}
library(plyr)
data = ddply(data, .(subject), mutate, trial    = as.factor(as.numeric(as.factor(onset))) )   
```
stecke alles in einem data frame zum weiterarbeiten
```{r}

d = data[,c('subject','begin','end','duration','blink','amplitude','onset', 'pic', 'time', 'objects','trial')]
```

das Filtern: keine Fixationen vor dem Onset, keine Blinks, weniger als 1000 ms, nur fixationen unter 1000 ms.

```{r}
d=subset(d, time>0 & blink == 0 & time<10000 & duration < 1000)
```

nonleinear least squares fit, wir ignorieren die hierarchische Struktur Person/Trial/Fixation.

```{r}
f <- function(time,a,b) {b * exp(a / time)}

fit <- nls(duration ~ b*exp(a/time), start = c(a=1, b=252), data=d) 
# plot(duration~time, data=d, pch='.')
# curve(f(x, a=coef(fit)['a'], b=coef(fit)['b']), add = TRUE, col="green", lwd=2) 
```

Pro VP einen plot. Jeder Trial hat eine Farbe. Über den Daten soll der exponentielle fit geplottet werde.

```{r}
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
  
```

was sagt eine glättungsline über eine lokales polynom?

```{r}
library(ggplot2)
ggplot(data = d, aes(x=time, y=duration, colour = trial, group=trial )) +
  geom_line(size=0.2) +
  #(size=1, shape=21, fill="white") +
 # geom_line(size=0.8,  aes(x=time, y=eta, group=exertype), colour = 'black', size=.5) +
  facet_wrap(~subject) +
  geom_smooth(aes(group=1), method="loess",  color = "black", se=FALSE) +
  theme_bw()+
  theme(legend.position = "none")
  
```
