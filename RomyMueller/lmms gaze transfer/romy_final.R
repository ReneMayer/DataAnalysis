library(lme4)

setwd('/home/rene/Dokumente/romy/lmms gaze transfer')
mig <- read.csv("TrialData.csv", sep=';')

# select variables and rename them
m1 <- mig[ , c('RT', 'Block_type', 'Trial_type', 'reply3', 'Vpn')]
names(m1) <- c("rt", "cursor", "triad", "response", "subject")

# rename levlesl in accordance to paper 
m1 <-
within(m1, {
            levels(cursor) = c('gaze','dot')
            levels(response) = c('yes+solved', 'yes+unsolved', 'incoherent')
      }
)

# delete strange subject 11
m1=m1[m1$subject != 11, ]

options(contrasts = c("contr.sum", "contr.poly"))


dimnames(model.matrix(~ c * t * r ,m1 ))
m1 <-
within(m1, {
            c = cursor
            r = response
            t = triad
      }
)
mm=model.matrix(~ c * t * r ,m1 )
d=data.frame(mm, m1)
library(optimx)

l3 <- 
         lmer(rt ~  c * t * r + 
                   ( c.dot.gaze + t.uncued.cued + r.unsolved.solved + r.incoherent.solved  +
              c.dot.gaze:t.uncued.cued +   c.dot.gaze:r.unsolved.solved   +   c.dot.gaze:r.incoherent.solved    +          
              t.uncued.cued:r.unsolved.solved    + t.uncued.cued:r.incoherent.solved  +         
              c.dot.gaze:t.uncued.cued:r.unsolved.solved  + c.dot.gaze:t.uncued.cued:r.incoherent.solved
                   
                   | subject), 
              data = d, REML = TRUE, control=lmerControl(optCtrl=list(maxfun=20000))
             ) # "bobyqa"
              # control=lmerControl(optCtrl=list(maxfun=20000) )
              # lmerControl(optimizer="optimx", optCtrl=list(method="nlminb")

l2 <- 
         lmer(rt ~  c * t * r + 
                   ( c.dot.gaze + t.uncued.cued + r.unsolved.solved + r.incoherent.solved  +
              c.dot.gaze:t.uncued.cued +   c.dot.gaze:r.unsolved.solved   +   c.dot.gaze:r.incoherent.solved    +          
              t.uncued.cued:r.unsolved.solved    + t.uncued.cued:r.incoherent.solved  
                   
                   | subject), 
              data = d, REML = TRUE, control=lmerControl(optCtrl=list(maxfun=20000))
             ) # "bobyqa"

l1 <- 
         lmer(rt ~  c * t * r + 
                   ( c.dot.gaze + t.uncued.cued + r.unsolved.solved + r.incoherent.solved    
                   
                   | subject), 
              data = d, REML = TRUE, control=lmerControl(optCtrl=list(maxfun=20000))
             ) # "bobyqa"

l0 <- 
         lmer(rt ~  c * t * r + 
                   ( c.dot.gaze + r.unsolved.solved + r.incoherent.solved    
                   
                   | subject), 
              data = d, REML = TRUE, control=lmerControl(optCtrl=list(maxfun=20000))
             ) # "bobyqa"
             
             
anova(l0, l1, l3, refit=FALSE)

             
# top-down modelling, start with the full (loaded) model 
loaded <- 
         lmer(rt ~  cursor * triad * response + 
                   ( cursor + triad + response | subject), 
              data = m1, REML = TRUE,
                   control=lmerControl(optimizer="bobyqa"))
#anova(loaded, ddf="Kenward-Roger")
# library(pbkrtest)
# KRmodcomp(fitF, fitR)

library(car)
l.anova=Anova(loaded, type=3, test.statistic= "F")

reduced <- 
         lmer(rt ~  cursor * triad * response + 
                   ( cursor +  response | subject), 
              data = m1, REML = TRUE)
anova(loaded, reduced, refit = F)


twoFactors <- 
         lmer(rt ~  cursor * response + 
                   ( cursor + triad + response | subject), 
              data = m1, REML = TRUE)
lrt1=anova(loaded, twoFactors)

# setting up usfule contrasts
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

twoFactorsC <- 
         lmer(rt ~  cursor * response + 
                   ( cursor + triad + response | subject), 
              data = m1, REML = TRUE)
              
              
library(phia)
post.hock=round(
  testInteractions(twoFactorsC, pairwise='cursor' , fixed= 'response'), 2 )
  # fixed = alle levels, across = 'triad' triad.x - triad.y
              
s=round(summary(twoFactorsC)$coefficients, 2) 
rownames(s)= gsub(rownames(s), pattern='cursor|response|\\.', replacement='')
save(file='LMMs_romy.RData', post.hock, twoFactorsC, loaded, l.anova, lrt1, s)              

# 08193 72850