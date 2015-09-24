

library(car)
library(lme4)
#library(lattice)
#options(contrasts=c(" contr.treatment","contr.poly"))


setwd('/home/rene/Dokumente/romy/lmms gaze transfer')
mig <- read.csv("TrialData.csv", sep=';')

# since the file is not large, copying is ok..., select relevant cols
m1 <- mig[ , c('RT', 'Block_type', 'Trial_type', 'reply3', 'Vpn')]
# rename labels in accordance to paper 
names(m1) <- c("rt", "cursor", "triad", "response", "subject")

# rename levlesl in accordance to paper 
m1 <-
within(m1, {
            levels(cursor) = c('gaze','dot')
            levels(response) = c('yes+solved', 'yes+unsolved', 'incoherent')
      }
)


rt <- aggregate(rt ~ subject * cursor * triad * response, data = m1, FUN = mean)
n <- aggregate(rt ~ subject * cursor * triad * response, data = m1, FUN = length)

#max=aggregate(rt ~ subject * cursor * triad * response, data = n, FUN = max)

rt<-cbind(rt,n=n$rt)

# 'id = ...' is unused in this call but exported to the next
library(latticeExtra)

pdf('level1_level2.pdf')

p1 <-
with(rt, 
     xyplot( rt ~ cursor|triad  , group = response,  id = subject, type = 'p', pch = 21 , cex=n/10, ylim=c(0,30200),
             auto.key=list(corner=c(0,0.95), points=FALSE, lines=TRUE, size = 2), 
             par.settings = list(strip.background=list(col="lightgrey"))) ) + 
# 'group = ...' ist not changed in terms of color but in terms of belongingness of points
glayer( panel.xyplot(x, y, group = id, type='l', alpha=.3,...) ) + 
# average horizontally
glayer( panel.average(x, y, type='l', horizontal=F, lwd=3, alpha=.8,...) )
dev.off()

library(lme4)

library(MASS)
boxcox( rt ~  cursor * triad * response, data=m1)   #  lambda ~ 0, also log
with(m1, densityplot(~rt) )
par(mfrow=c(2,4))
plot(fit <- lm( rt ~  cursor * triad * response, data=m1[(scale(m1$rt, scale=F) < -2.5) & (scale(m1$rt, scale=F) < 2.5),])) # spread-location-plot zeigt einen positiven trend

pframe=with(m1, expand.grid(cursor = levels(cursor), triad = levels(triad), response=levels(type)  ))
pframe$eta=predict(fit, pframe)
xyplot(eta ~ cursor|triad  , group = response, pframe,  type='l')


plot(fit <- lm( log(rt) ~  cursor * triad * response, data=m1)) # log-y ist OK


# daten sind log-linear
m1=m1[m1$subject != 11, ]

summary( loaded.r <- 
         lmer(rt ~  cursor * triad * response + 
                   ( cursor + triad + response | subject), 
              data = m1, REML = TRUE)
)



# modelling top down with maximal fixed and random effects; i.e., does not converge with additive random terms, so (c+t+r|s) 
summary( loaded <- 
         lmer(rt ~  cursor * triad * response + 
                   ( cursor + triad + response | subject), 
              data = m1, REML = TRUE)
)




summary( red <- 
         lmer(rt ~  cursor * triad * response + 
                   ( 1 | subject), 
              data = m1, REML = TRUE)
)
# random intercepts model
anova(red, loaded)



summary( loaded.r <- 
         lmer(rt ~  cursor *  response + 
                   ( cursor + triad + response | subject), 
              data = m1, REML = TRUE)
)


summary( l2 <- 
         update(loaded, .~.-cursor : triad : response, 
              data = m1, REML = TRUE)
)

anova(loaded, l2) # n.s., d.h. keine dreifachinteraktion

summary( l3 <- 
         update(l2, .~.-(cursor * triad * response)^2, 
              data = m1, REML = TRUE)
)

anova(l3,l2) # 0.0004745 ***, d.h. min eine zweifachinteraktion

# welche
summary( l4 <- 
         update(l2, .~.-cursor:triad, 
              data = m1, REML = TRUE)
)
anova(l4,l2) # cursor:triad - n.s.


l5 <- update(l2, .~.-cursor:response, 
              data = m1, REML = TRUE)

anova(l5,l2) # cursor:response

summary( l6 <- 
         update(l2, .~.-triad:response, 
              data = m1, REML = TRUE)
)

anova(l6,l2) #triad:response - n.s.

summary( update(l2, log(rt)~.) )



pframe=with(m1, expand.grid(cursor = levels(cursor), triad = levels(triad), response=levels(response)  ))
pframe$eta=predict(l2, pframe, re.form=NA)
p2 <-
xyplot(eta ~ cursor|triad  , group = response, pframe,  type='l', ylim=c(0,30200),
auto.key=list(corner=c(0,0.95), points=FALSE, lines=TRUE, size = 2),
scales=list(y=list(at=NULL)), 
             par.settings = list(strip.background=list(col="lightgrey")))

pdf('data_level1.2_model_level1.pdf')

library(gridExtra)
print(arrangeGrob(p1, p2, ncol = 2))
dev.off()

# wald-F-tests
Anova(l2, type=3)

Anova(loaded.r, type=2)

# planned comparisons
library(phia)

# was ist die L-matrix?
summary( testFactors(l2, list(response = c( "yes+unsolved", "incoherent")) ) )

testInteractions(loaded.r, pairwise='cursor' , fixed= 'response') # fixed = alle levels, across = 'triad' triad.x - triad.y

testInteractions(l2, fixed='response', across='cursor') # fixed = alle levels, across = 'triad' triad.x - triad.y

testInteractions(l2, pairwise='cursor', fixed='response') # fixed = alle levels, across = 'triad' triad.x - triad.y

# mache die L-Matrix selber!

testInteractions(l2, pairwise='cursor', across='response')

# #   solved.vs.unsolved <- list(response = c(1, -1, 0))
# #   unsolved.vs.incoherent <- list(response = c(0, 1, -1))
# #   solved.vs.incoherent <- list(response = c(1, 0, -1))

# Einzelvergleiche
testInteractions(l2, pairwise='cursor', custom=solved.vs.incoherent) 


  
## ANOVA


library(plyr)
library(reshape)

m=melt(data=m1, measure='rt')
options(width=190)

# romy lässt triad weg
( dd=cast(m,  subject ~ cursor +  triad + response  , mean) )

library(mice)
dd.imp<-mice(dd)
complete(dd.imp)


idata <- with(m1, expand.grid (
  response = levels(response),
   triad   = levels(triad),
   cursor  = levels(cursor) 
 )
)

# test if it matches
( cbind(idata, names(dd)[-1]) )


names(dd)=gsub(names(dd), pattern='\\+', replace='.')

fit.r  <-  lm( cbind(   gaze_cued_yes.solved,   gaze_cued_yes.unsolved,   gaze_cued_incoherent,    
                      gaze_uncued_yes.solved, gaze_uncued_yes.unsolved, gaze_uncued_incoherent,   
                         dot_cued_yes.solved,    dot_cued_yes.unsolved,    dot_cued_incoherent,
                       dot_uncued_yes.solved,  dot_uncued_yes.unsolved,   dot_uncued_incoherent   ) ~ 1, 
                       data=complete(dd.imp))

# Repeated-Measures MANOVA
library(car)

av.r <- Anova(fit.r, idata=idata, idesign= ~ cursor * triad * response) # , type="III"
options(width=190)

summary(av.r)

library(phia)

ok.means <- interactionMeans(fit.r, c("response","cursor", "triad" ), idata=idata)

pdf('ANOVA.pdf')
plot(ok.means, errorbar="ci95")
dev.off()


testInteractions(fit.r, pairwise=c("cursor"), fixed='response',idata=idata)


#aggregate(data=m[m$subject!=11, ],  rt ~ subject * cursor *  response  , mean)

# a=aggregate(rt ~ subject * cursor * response, data = m[m$subject!=11, ], FUN = mean)

# a=aggregate(rt ~  cursor * triad * response, data = a, FUN = mean)



( e=cast(m[m$subject!=11, ],    cursor ~ response + triad   , mean, na.rm=T) )


apply(e, MARGIN=2,FUN=diff)

# summary( lm(data=rt, rt ~ response * cursor ) )



rt <- aggregate(rt ~ subject * cursor * triad * response, data = m1, FUN = mean)
ftable(with(m1, table(subject, cursor, triad, response)), row.vars = c('subject'))

# viewing NAs
ftable(atab <- xtabs(rt ~ subject + cursor +   triad + response, m1), row.vars = c('subject'))
ftable( with(m1, table(subject, triad, cursor, response)), row.vars = c('subject'))



#(diffs <- as.table(apply(atab, 2:3, diff)))




# general linear hypothesis by hand

names(fixef(l2))

"cursor=gaze response=yes+solved traid=cued" 
"cursor=dot" 
"triad=uncued" 
"respons=eyes+unsolved" 
"response=incoherent"              
"cursor=dot:triad=uncued"
"cursor=dot:respons=eyes+unsolved"
"cursor=dot:response=incoherent"
"triad=uncued:respons=eyes+unsolved"
"triad=uncued:response=incoherent"


# L-Matrix
# http://www.ats.ucla.edu/stat/stata/faq/contrast12.htm
# install.packages('sas7bdat',dep=T )
require(sas7bdat)
require(lme4)

download.file('http://www.ats.ucla.edu/stat/sas/faq/exercise.sas7bdat', destfile='exercise.sas7bdat')
d= read.sas7bdat(file='exercise.sas7bdat')
d=within(d, {diet =factor(diet)
exertype=factor(exertype)
 time=factor(time)
 id = factor(id)}
)
options(contrasts = c("contr.treatment", "contr.poly"))
summary( s <- lmer(pulse ~  1+ exertype * diet     + (1|id), d) )
Anova( s)
library(lattice)

xyplot(pulse ~ exertype , group=diet, type='a', d)
diet=matrix(c(1,-1), ncol=1)
exertype=matrix(c(-.5,-.5,1), ncol=3)

l = diet %*% exertype 
L = c(l[1,], l[2,])

esticon(s, L )

dd=aggregate(pulse ~ exertype * diet,d, mean)
ftable(atab <- xtabs(pulse ~ diet + exertype , dd))

t(   as.matrix(atab) %*% t(exertype)  ) %*% diet



m=with(d,expand.grid( exertype = levels(exertype), diet = levels(diet) ))

model.matrix(~  1+ exertype * diet,m ) %*% matrix( (fixef(s)), ncol=1)
 
linearHypothesis(s, L)
# L %*% b
L %*% matrix( (fixef(s)), ncol=1) 

install.packages('contrast', dep=T)

"(Intercept)" "exertype2" "exertype3" "diet2" "exertype2:diet2" "exertype3:diet2"
L1=-( ( c(1, 0, 0, 0, 0, 0) + c(1, 1, 0, 0, 0, 0) ) /2 ) + c(1,0,1,0,0,0) # 0.0  0.5 -1.0  0.0  0.0  0.0
L2=- ((  c(1, 0, 0, 1, 0, 0) + c(1, 1, 0, 1, 1, 0) ) /2 ) +c(1,0,1,1,0,1) 


#
L=c(0,0,0,-1,1,-1)
# 
# 
# --> keine Zeit dafür!!
library(doBy)

L=c(.25, .25, -.5, .25,  .25, -.5, -.5,  -.5,   1, -.25, -.25,  .5, -.25, -.25,  .5, .5,   .5,  -1)
names(fixef(s))

# http://socserv.mcmaster.ca/racinej/Gallery/crs_files/radial_rgl.R

## --- contextual effects


mm=data.frame(m1, model.matrix(~0+response,data=m1))
library(plyr)
mm = ddply(mm, .(subject), mutate, 
       
       m.unsolved =  mean(responseyes.unsolved) * 10,
       m.incoherent =  mean(responseincoherent) *10
       )  

summary( context <- 
         update(l2, .~. +  m.unsolved +  m.incoherent, 
              data = mm, REML = TRUE)
)
library(spidadev)

options(width=180)
summary( context <- 
         lmer(rt ~ cursor + triad + response + I(cvar(response, subject)*10) + cursor:triad + cursor:response +      triad:response
               + I(cvar(response, subject)*10) : triad +  I(cvar(response, subject)*10) :cursor +
              (  cursor + triad + response | subject), 
              data = m1, REML = TRUE) )

summary( context2 <- 
         lmer(rt ~ cursor * triad * response * I(cvar(response, subject)*10) +
              (  cursor + triad + response | subject), 
              data = m1, REML = TRUE) )
              
              
library(car)

anova(context, l2)


# ... kontraste müssen per Hand gemacht werden!
library(phia)

testInteractions(context, pairwise='cursor' , fixed= 'response') # fixed = alle levels, across = 'triad' triad.x - triad.y



library(lattice)

pdf('ranef.pdf')
dotplot(ranef(l2, postVar=TRUE))
dev.off()

summary( l2.r <- 
         update(l2, .~. , 
              data = mm[mm$subject!=11,], REML = TRUE)
)


library(car)

Anova(l2.r)
testInteractions(ld, pairwise='cursor' , fixed= 'response') # fixed = alle levels, across = 'triad' triad.x - triad.y

library(lattice)


mm$response <- relevel(mm$response, ref = "incoherent")



summary( ld <- 
         lmer(rt ~  cursor * triad * response - cursor : triad : response +
                   ( cursor + triad + response | subject), 
              data = mm[mm$subject!=11,], REML = TRUE)
)

testInteractions(ld, pairwise='cursor' , fixed= 'response') # fixed = alle levels, across = 'triad' triad.x - triad.y

# simple coding for marginal effects

#creating the contrast matrix manually by modifying the dummy coding scheme


cc=contr.treatment(2)-matrix(rep(1/2, 2), ncol=1)
dimnames(cc) = list(c("gaze", "dot"), c('.dot-gaze'))
contrasts(m1$cursor)=cc

ct=contr.treatment(2)-matrix(rep(1/2, 2), ncol=1)
dimnames(ct) = list(c("cued", "uncued"), c(".uncued-cued"))
contrasts(m1$triad)=ct

library(MASS)

cr=contr.sdif(3)#-matrix(rep(1/3, 6), ncol=2)
dimnames(cr) = list(c( "yes+solved", "yes+unsolved", "incoherent" ), 
                    c(".unsolved-solved", '.incoherent-solved'))
contrasts(m1$response)=cr

summary( fit2 <- 
         lmer(rt ~  cursor * response  +
                   ( cursor + response | subject), 
              data = m1, REML = TRUE)
)

anova(fit,fit2)
# http://stats.stackexchange.com/questions/118416/getting-p-value-with-mixed-effect-with-lme4-packages

#install.packages("lmerTest", repos="http://R-Forge.R-project.org")
#?drop1.merMod 
library(lmerTest)

