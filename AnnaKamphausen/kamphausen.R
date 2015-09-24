# hier den Ordner-Pfad einstellen 
setwd('/home/rene/Dokumente/kamphausen')
library(foreign)
d <- 
read.spss(  file             = 'rating_wissen_mot_12sept._ak.sav',
	    use.value.labels = TRUE,  # SPSS variables with value labels into R factors with levels
            max.value.labels = Inf,   # can be any real number 
            to.data.frame    = TRUE,  # we want it to be an data.frame 
            use.missings     = TRUE  # recode SPSS set missings-code to NA
)

d = within(d, { Vp = factor(Vp) })


s=read.table(file             = 'daten_feat_bez.csv', sep=';', 
             na.strings       = "NA", 
             header           = TRUE )

library(reshape)

# Recodieren 
# Vorw = ordinal
# Motiv = disordinal
# Selbstk = semiordinal
# Vorw_steigg_riR = revision # ohne R baseline

# Restructure Data
m = melt(d, 
 id.vars      = c('Vp', 'Reihenfolge', 'Bedingung'), 
 measure.vars = c("Mot_beziehg_ri", "Mot_steigg_ri", "Mot_kreuzg_ri", "Vorw_beziehg_ri", "Vorw_steigg_ri", "Vorw_kreuzg_ri",
                  "Selbst_beziehg_ri", "Selbst_steigg_ri", "Selbst_kreuzg_ri", "Mot_beziehg_riR", "Mot_steigg_riR", "Mot_kreuzg_riR",
                  "Vorw_beziehg_riR", "Vorw_steigg_riR", "Vorw_kreuzg_riR", "Selbst_beziehg_riR", "Selbst_steigg_riR", "Selbst_kreuzg_riR" ) )
 
# head(within(m, { new=strsplit(as.character(variable), split='_')[[1]]}))

library(plyr)

# Split up labels 'Vorw_beziehg_riR' into 3 variables
info  = sapply(as.character(m$variable),function(x) { l=strsplit(x, split='_')})                            
parts = ldply(info, function(x) {  c(diagram=x[[1]], v=x[[2]], baseline=x[[3]]) })                            
m     = data.frame(m, parts)

# rename and select
names(m) = c('subj', 'order', 'condition', 'variable', 'correct', '.id', 'diagram', 'feature', 'baseline' )
m        = m[, c('subj', 'order', 'condition', 'correct','diagram', 'feature', 'baseline' ) ]


n  <- aggregate(correct ~ subj * order *  diagram * feature * baseline, data = m, FUN = mean, na.rm=TRUE)
n  <- aggregate(correct ~ subj * order * condition * diagram * feature, data = m, FUN = length)
rt <- cbind(rt,n=n$RT)

m[,c('diagram', 'feature', 'baseline')] = lapply(m[,c('diagram', 'feature', 'baseline')], as.factor)

library(latticeExtra)
n  <- aggregate(correct ~ subj *  diagram * feature * baseline, data = m, FUN = mean, na.rm=TRUE)


with(m, ftable(subj, order, baseline, row.vars=c('baseline', 'subj'), col.vars='order'))

with(m, ftable(subj,feature, baseline, condition, row.vars=c('baseline', 'subj'), col.vars=c('feature', 'condition')))

with(m, ftable(subj, diagram, baseline, row.vars= 'subj' ))

## pdf('k1.pdf')
# ... vierfachinteraktion
with(aggregate(correct ~ subj * condition * baseline * feature * diagram , data = m, FUN = mean, na.rm=TRUE),
     useOuterStrips(xyplot( correct ~ baseline |  feature * diagram, group = condition   ,  id = subj, type = ' ', pch = 21 , 
     auto.key=list(space='right', points=FALSE, lines=TRUE)) )) + 
# 'group = ...' ist not changed in terms of color but in terms of belongingness of points
# glayer( panel.xyplot(x, y, group = id, type='b', alpha=.5,...) ) + 
# average horizontally
glayer( panel.average(x, y, type='l', horizontal=F, lwd=3, alpha=.8,...) )

# ... dreifachninteraktion
with(aggregate(correct ~ subj * condition * baseline * feature  , data = m, FUN = mean, na.rm=TRUE),
    xyplot( correct ~ baseline |  feature , group = condition   ,  id = subj, type = ' ', pch = 21 , 
     auto.key=list(space='right', points=FALSE, lines=TRUE), layout=c(3,1) )) + 
# 'group = ...' ist not changed in terms of color but in terms of belongingness of points
# glayer( panel.xyplot(x, y, group = id, type='b', alpha=.5,...) ) + 
# average horizontally
glayer( panel.average(x, y, type='l', horizontal=F, lwd=3, alpha=.8,...) )

# zweifachinteraktion
with(aggregate(correct ~ subj * condition * feature  , data = m, FUN = mean, na.rm=TRUE),
    xyplot( correct ~  feature , group = condition   ,  id = subj, type = ' ', pch = 21 , 
     auto.key=list(space='right', points=FALSE, lines=TRUE, title = "condition"), 
     par.settings=canonical.theme(color=FALSE)) )  + 
# 'group = ...' ist not changed in terms of color but in terms of belongingness of points
# glayer( panel.xyplot(x, y, group = id, type='b', alpha=.5,...) ) + 
# average horizontally
glayer( panel.average(x, y, type='l', horizontal=F, lwd=3, alpha=.8,...) )

## dev.off()

# condition + feature + time + condition:time
library(lme4)
m1 = glmer(correct ~  feature + baseline + condition + diagram + 
           (1|subj) + (1|order), 
           data=m,
           family=binomial)
summary(m1)
m2 = update(m1, formula= . ~ . +  baseline:condition + baseline:feature + baseline:diagram )


m3 = glmer(correct ~ (feature  + condition + baseline  )^2 + C(diagram,sum) + C(diagram,sum):baseline + C(diagram,sum):condition + 
           (1|subj) + (1|order)  , 
           data=na.omit(m),
           family=binomial)
summary(m3)
m3a=update(m3, formula=.~.-(feature  + condition + baseline  )^2 + (feature  + condition + baseline  )^4 - C(diagram,sum):baseline - C(diagram,sum):condition )
anova(m3, m3a)   # alles jenseits der vierfachinteraktion kann ignoriert werden.

dotplot(ranef(m3a, postVar=TRUE))$subj
dotplot(ranef(m3a, postVar=TRUE))$order

# ist feature unter Diagramm?
m3a=update(m2, formula=.~.-baseline:diagram)
anova(m3,m3a) # interaktion n.s.



m4 = glmer(correct ~  ( condition + feature  +  diagram + baseline)^3 +
           (1|subj), na.action = na.exclude,
           data=m,
           family=binomial)
m4 = glmer(correct ~  condition * baseline +
           (1|subj) + (1|order) + (1|diagram/feature), na.action = na.exclude,
           data=m,
           family=binomial)
summary(m4)

           
summary(m4)

library(geepack)
g1 = geeglm( correct ~  ( condition + feature  +  diagram + baseline)^4,
                id = subj, 
            corstr = "exchangeable",
              data = m,
              wave = as.numeric(m$baseline),
            family = binomial)
anova(g1)
           
summary(g1)

# # 'id = ...' is unused in this call but exported to the next
# with(m, xyplot( correct ~ baseline|order*feature , group = diagram,  id = subj, type = 'p', pch = 21 , 
#      auto.key=list(space='right', points=FALSE, lines=TRUE)) ) + 
# # 'group = ...' ist not changed in terms of color but in terms of belongingness of points
# glayer( panel.xyplot(x, y, group = id, type='l', alpha=.5,...) ) + 
# # average horizontally
# glayer( panel.average(x, y, type='l', horizontal=F, lwd=3, alpha=.8,...) )
library(MCMCglmm)

Model1 <- MCMCglmm(correct ~  ( condition + feature  +  diagram + baseline)^4,
  random = ~ subj + order, family = "categorical",
  data = na.omit(m), prior = prior1, pr = TRUE, pl=FALSE)

prior1 <- list(
  R = list(V=1,fix=1),  
  G = list(
    G1 = list(V = 1, nu = .002),
    G2 = list(V = 1, nu = .002)
  )
)

summary(Model1)
install.packages("glmmADMB",
  repos=c("http://glmmadmb.r-forge.r-project.org/repos",getOption("repos")),
  type="source")
  
library(glmmADMB)
ma <- glmmadmb(correct ~  ( condition + feature  +  diagram + baseline)^4 +
  (1|subj) + (1|order), family = "binomial", data = na.omit(m)
)
library(glmmPQL)
library(MASS)

pql = glmmPQL( correct ~  ( condition + feature  +  diagram + baseline)^4,
                random = ~1|subj, data = na.omit(m),
            family = binomial)
print(pql, corr=FALSE)
summary(pql)





  



                  
                  
                  

