setwd('/home/rene/Arbeitsfläche/LMM/helmert/daten')

d=read.delim('distractor_effect_filtered.dat', header=TRUE)

d<-within(d, {
  duration=CURRENT_FIX_DURATION
  distractor=factor(distractor, levels=c(0,1), labels=c('Baseline','present'))
  instruction=factor(instruction, levels=c(0,1), labels=c('spatial','object'))
  object=factor(on_obj, levels=c(0,1), labels=c('background', 'object'))
  subject=factor(RECORDING_SESSION_LABEL)
})

library(lattice)

# ballanced? - no
with(d, ftable(table(distractor,instruction, object,subject )))



# A: marginal fixed effects 
#pdf('distractor_effect.pdf')
xyplot(duration~distractor|instruction, group=object, type='a',data=d, ylim=c(200,500), auto.key=T)
#dev.off()

# A: random effects?
rt <- aggregate(duration~subject*distractor*instruction*object, data = d, FUN = mean)
n <- aggregate(duration~subject*distractor*instruction*object, data = d, FUN = length)
rt<-cbind(rt,n=n$duration)

library(latticeExtra)

# 'id = ...' is unused in this call but exported to the next
with(rt, 
     xyplot( duration ~ distractor|instruction , group = object,  id = subject, type = 'p', pch = 21 , cex=n/30, alpha=.2, 
             auto.key=list(space='right', points=FALSE, lines=TRUE)) ) + 
# 'group = ...' ist not changed in terms of color but in terms of belongingness of points
glayer( panel.xyplot(x, y, group = id, type='l', alpha=.2,...) ) + 
# average horizontally
glayer( panel.average(x, y, type='l', horizontal=F, lwd=3, alpha=.8,...) )


library(lme4)

summary(m1 <- lmer(duration ~ distractor*instruction*object + (1|subject), data=d) )

# konvergiert nicht, object-varianz nicht schätzbar 
summary(m1a <- lmer(duration ~ distractor*instruction*object + (distractor+instruction+object|subject), data=d) )

# model convergiert nicht - vermutlich zu reich parametrisiert
summary(m2 <- lmer(duration ~ distractor*instruction*object + (distractor+instruction|subject), data=d) )

summary(m2a <- lmer(duration ~ distractor*instruction*object  +(1+distractor|subject), data=d) )

summary(m2aa <- lmer(duration ~ distractor*instruction*object  +(distractor||subject), data=d) )

# run a model without any random effects correlation parameters

# head(model.matrix(duration ~ distractor*instruction*object, data=d))
d$c.d=model.matrix(duration ~ distractor*instruction*object, data=d)[,'distractorpresent']
d$c.i=model.matrix(duration ~ distractor*instruction*object, data=d)[,'instructionobject']
d$c.o=model.matrix(duration ~ distractor*instruction*object, data=d)[,'objectobject']

summary(m2aaa <- lmer(duration ~ c.d*c.i*c.o  +(c.d+c.i||subject), data=d) )

anova(m2, m2aaa, refit=FALSE) # correlation parameters n.s.


# compound symmetrie für distraktor
summary(m2b<- lmer(duration ~ distractor*instruction*object  +(1|subject/distractor), data=d) )

anova(m1,m2,m2a,m2b, refit = FALSE) # random intercept model ausreichend


summary(m2i <- lmer(duration ~ distractor+instruction+object + (distractor+instruction|subject), data=d) )
anova(m2,m2i) # interactions significant 
summary(m2ia <- lmer(duration ~ (distractor+instruction+object)^2 + (distractor+instruction|subject), data=d) )
anova(m2a,m2ia) # dreifachinteraktion n.s.

# wirklich normalverteilt?
library(MASS)
boxcox( duration ~ distractor*instruction*object*subject, data=d)   #  lambda ~ 0, also log
with(d, densityplot(~duration) )
par(mfrow=c(2,2))
plot(fit <- lm( duration ~ distractor*instruction*object*subject, data=d)) # spread-location-plot zeigt einen positiven trend
plot(fit <- lm( log(duration) ~ distractor*instruction*object*subject, data=d)) # log-y ist OK


# alles nochmal mit log-transformierten Daten
summary(m1a <- lmer(log(duration) ~ distractor*instruction*object + (distractor+instruction+object|subject), data=d) )
summary(m1b <- lmer(log(duration) ~ distractor*instruction*object + (1|subject), data=d) )
anova(m1a, m1b, refit=FALSE) # trotzdem scheint das random intercept model hier gut zu passen

# crossed random effects

bwplot(duration~picname,data=subset(d, duration < 2000))
bwplot(duration~question,data=subset(d, duration < 2000))

summary(m1a <- lmer(duration ~ distractor*instruction*object + (1|subject)+(1|picname)+(1|question), data=d) )

# A: level 1 varianz bleibt der dominante Varianzanteil

summary(m1a <- lmer(log(duration) ~ distractor*instruction*object+I(as.numeric(rt)/1000) + (1|subject)+(1|picname)+(1|question), data=d) )





# ----------------------------------------

setwd('/home/rene/Arbeitsfläche/LMM/helmert/daten')

d=read.delim('fov_locations_vs_attributes.csv', header=TRUE)


library(lattice)

# ballanced? - no
with(d, ftable(table(subject, block, fixation_target)))

xyplot(fix_duration~fixation_target, group=block, type='a',data=d, ylim=c(200,400), auto.key=T)

xyplot(sac_amplitude~fixation_target, group=block, type='a',data=na.omit(d),ylim=c(4,5), auto.key=T)

# with(na.omit(d), interaction.plot(response=sac_amplitude, x.factor=fixation_target, trace.factor=block)
 
library(ggplot2)
#histogram with density line overlaid
ggplot(d, aes(x=log(sac_amplitude, fill=fixation_target)) + 
  geom_histogram(aes(y = ..density..), color="black", fill=NA) +
  geom_density(color="blue",  alpha = 0.5) +
  facet_grid(.~block)


# A: random effects?
rt <- aggregate(fix_duration~subject*fixation_target*block, data = d, FUN = mean)
n <- aggregate(fix_duration~subject*fixation_target*block, data = d, FUN = length)
rt<-cbind(rt,n=n$fix_duration)

library(latticeExtra)

# 'id = ...' is unused in this call but exported to the next
with(rt, 
     xyplot( fix_duration~fixation_target, group = block,  id = subject, type = 'p', pch = 21 , cex=n/100, alpha=.2, 
             auto.key=list(space='right', points=FALSE, lines=TRUE)) ) + 
# 'group = ...' ist not changed in terms of color but in terms of belongingness of points
glayer( panel.xyplot(x, y, group = id, type='l', alpha=.2,...) ) + 
# average horizontally
glayer( panel.average(x, y, type='l', horizontal=F, lwd=3, alpha=.8,...) )

library(lme4)
summary(m1a <- lmer(fix_duration ~ fixation_target*block + (1|subject), data=d) )
m2 <- update(m1a, .~.-(1|subject)+(fixation_target+block|subject))
summary(m2)


anova(m1a, m2, refit=F)

head(model.matrix(m2))

d$c1 = model.matrix(m2)[,'fixation_targetObject']
d$c2 = model.matrix(m2)[,'blocklocations']
d$c3 = model.matrix(m2)[,'blockmixed']


summary(m1c <- lmer(fix_duration ~ c1*c2*c3 + (0+c1+c2+c3||subject), data=d) )

# um die Fehlermeldung zu verstehen, siehe 
# head(model.matrix(fix_duration ~ c1*c2*c3, data=d ))
# tatsächlich, die letzten beiden Spalten brauchen wir nicht

anova(m2, m1c, refit=F)


# correlation der zufälligen Effekte
dotplot(ranef(m2), layout=c(4,1))
library(latticeExtra)

splom(~ ranef(m2)$subject, 
  panel=function(x,y,...) {
    panel.ellipse(x,y,...)
    panel.xyplot(x,y,...)
  }
)


d$lsa=log(d$sac_amplitude+1)


summary(m1a <- lmer(lsa ~ fixation_target*block + (fixation_target+block|subject), data=na.omit(d) ))
summary(m1b <- lmer(lsa ~ 1 + (fixation_target+block|subject), data=na.omit(d) ))

anova(m1a, m1b, refit=T) # fixed effects n.s.

## to try: different error distributions for sac_amplitude, e.g. weibul, gamma
##	   multivariate mixed model with specific distributions like
## 	   mcmcglmm(c(sac_amplitude, fix_duration ) ...family=c('gamma', 'normal'))


# ---------------------------------------------------------------------------------------------

setwd('/home/rene/Arbeitsfläche/LMM/helmert/daten')

d=read.csv2('testlmm_RT_fixdistance.csv', header=TRUE)

d<-within(d, {
  relation=factor(cue_target_relation, levels=c(1,2,3), labels=c('valid', 's.invalid','d.invalid'))
  head=factor(head_position, levels=c(1,2,3), labels=c('back', 'between', 'front'))
  c.fd=scale(fixdistance, scale=F)
})

m=melt(d[,c('relation','RT', 'vp')], measure='RT')
ad=cast(m,vp~relation, mean)

idata <- with(d, expand.grid (
   relation     = levels(relation) )
)

fit <-  lm( cbind(valid,s.invalid,d.invalid) ~ 1, data=ad)
library(car)

av <- Anova(fit, idata=idata, idesign= ~relation) 

summary(av, multivariate=F)

ad1=cast(m,vp+relation~., mean)
names(ad1)[3]='RT'

summary(aov2 <- aov(RT~relation+factor(vp), data=ad1))


# computing ICC using ANOVA table
ms <- anova(aov2)[[3]]
vs <- (ms[2] - ms[3])/nlevels(d$relation)   # subject
vr <- ms[3]                                 # residuals
vs / (vs+vr)

library(lme4)
summary(lmm <- lmer(RT~relation+(1|vp), data=ad1))

vcov(lmm)




names(m.wide)=c('id','time._1','time.0','time.1','time.2','time.4','time.6','time.8')

mm=cast(m[,c('id','value','time')], id + time ~ .)

summary(aov(value ~ variable + Error(id), data=m))



xyplot(RT~relation, group=head, type='a',data=d, ylim=c(350,420), auto.key=T)

xyplot(RT~fixdistance|relation, group=head, type='smooth',data=d, ylim=c(200,500), auto.key=T)

xyplot(RT~fixdistance|relation, group=head, type='spline',data=d[d$fixdistance<800,], ylim=c(200,500), auto.key=T)
xyplot(RT~fixdistance|relation, group=head, type='smooth',data=d[d$fixdistance<800,], ylim=c(200,500), auto.key=T)


library(ggplot2)
ggplot(d[d$fixdistance<800 & d$RT<800,], aes(x=fixdistance, y=RT, group=relation, colour=relation)) +
  geom_point(alpha=2/10, shape=19, size=.5) +
  stat_smooth(method="lm", se=TRUE, 
                formula=y ~ poly(x, 3, raw=TRUE)) +
  facet_grid(.~head)
  
ggplot(d[d$fixdistance<800 & d$RT<800,], aes(x=fixdistance, y=RT, group=head, colour=head)) +
  geom_point(alpha=2/10, shape=19, size=.5) +
  stat_smooth(method="loess", se=TRUE) +
  facet_grid(.~relation)              
  
library(lme4)

summary(m1a <- lmer(RT ~ c.fd+relation*head + (relation+head|vp), data=na.omit(d) ))

