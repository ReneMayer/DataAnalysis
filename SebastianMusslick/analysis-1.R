setwd("/home/rene/Dokumente/Sebastian/logfiles/")

files <- list.files(pattern = "*pilot_PrepSwitch_Exp5_")
names(files) <- files
library(plyr)

input <- ldply(files, read.table, header = TRUE, as.is = TRUE)


afterbreaktrials <- c(1, 2, 100, 101, 199, 200, 298, 299, 397, 398, 496, 497)
breaktrials      <- c(99, 198, 297, 396, 495)
Nblocks          <- 6
allsubjects      <- c(1:28)
Nallsubjects     <- length(allsubjects)
toExclude        <- c(15) # 15 - system window came up
datapath         <- "logfiles/"
errorbar         <- 576*0.15 # only people under 15% error?
mincellsize      <- 28 # unused
RT_lower_outlier <- 200 # lower RT outlier in ms


input = ddply(input, .(.id), mutate, mean.error    = mean(Error)) 

# was ist mit trials nach Fehlern? - sollten auch raus!
data <- subset(input, 	!(Trial %in% breaktrials) &
			!(Trial %in% afterbreaktrials) &
			RT >= RT_lower_outlier & 
			Error != 1 &
			mean.error <= .15
)
factors <- 
 c("Subj", "Task", "Task.n.1", "LetterCategory", "NumberCategory", "Letter", "Number", "Transition", "Transition.n.1", "ColorTrans")      
data[ ,factors] <- lapply(data[ ,factors], factor) 

data <- within(data, { 
    Subj             <- factor(Subj)
    AnswerRec        <- factor(AnswerRec, labels=c("left", "right"))
    Task             <- factor(Task)
    Task.n.1         <- factor(Task.n.1)
    LetterCategory   <- factor(LetterCategory)
    NumberCategory   <- factor(NumberCategory)
    Letter           <- factor(Letter)
    Number           <- factor(Number)
    Transition       <- factor(Transition)
    Transition.n.1   <- factor(Transition.n.1)
    Incongruency     <- factor(Incongruency, labels=c("con", "inc"))
    Incongruency.n.1 <- factor(Incongruency.n.1, labels=c("con", "inc"))
    ColorTrans       <- factor(ColorTrans)
    RespTrans        <- factor(RespTrans, labels=c("r", "s"))
    PrepInfo         <- factor(PrepInfo, labels=c("task", "feature"))
    PrepInfo.n.1     <- factor(PrepInfo.n.1, labels=c("task", "feature"))
    PrepTrans        <- factor(PrepTrans, labels=c("r", "s"))
})


ftable(data$Subj)
ftable(xtabs(~Subj+Transition + Incongruency + ColorTrans + PrepInfo, data=data), col.vars = 'Subj')
# factors of interest: (Task-)Transition, ColorTrans, Incongruency, PrepInfo

library(reshape)
m <- melt(data, measure.vars='RT', id.vars=c('Subj','Transition', 'Incongruency', 'ColorTrans', 'PrepInfo'))
dd <- cast(Subj ~ Transition + Incongruency + ColorTrans + PrepInfo, data=m, fun.aggregate=mean)

names(dd) # note permutation order with index Y_ijkl = Y_Transition_Incongruency_ColorTrans_PrepInfo

library(car)

# structure of intra-subject-data (idata)
idata <- with(data, expand.grid (
   PrepInfo     = levels(PrepInfo) ,
   ColorTrans   = levels(ColorTrans),
   Incongruency = levels(Incongruency),
   Transition   = levels(Transition)
   )
)

# test if it matches
( cbind(idata, names(dd)[-1]) )

# fit sebastian Musslick
fit.sm <-  lm( cbind(r_con_r_task, r_con_r_feature, r_con_s_task, r_con_s_feature, r_inc_r_task, r_inc_r_feature, 
                     r_inc_s_task, r_inc_s_feature, s_con_r_task, s_con_r_feature, s_con_s_task, s_con_s_feature, 
                     s_inc_r_task, s_inc_r_feature, s_inc_s_task, s_inc_s_feature) ~ 1, data=dd)

# Repeated-Measures MANOVA
av.sm <- Anova(fit.sm, idata=idata, idesign= ~ Transition * Incongruency * ColorTrans * PrepInfo) # , type="III"

options(width=120)

summary(av.sm)

# compare with aov(...) is the same, e.g. 
# aov() gives Transition:ColorTrans:PrepInfo  1  31120   31120   12.83 0.00138 **
# Anova()     Transition:ColorTrans:PrepInfo  31120 1 63062 26   12.8306  0.001377 ** 

# 
summary(av.sm, multivariate=TRUE)

RT_anova <- aggregate(RT ~ Subj * Transition * Incongruency * ColorTrans * PrepInfo, data = data, FUN = mean)
summary( ANOVA <- 
  aov(RT ~ Transition * Incongruency * ColorTrans * PrepInfo +  Error(Subj/(Transition * Incongruency * ColorTrans * PrepInfo)),
  RT_anova)
)



library(lme4)
# convergiert nicht - zu komplex: 'keep it maximal' hat praktische Grenzen
LMM <- lmer(RT ~  Transition * Incongruency * ColorTrans * PrepInfo + (Transition+Incongruency+ColorTrans+PrepInfo|Subj) , data)

library(latticeExtra)  

# marinale Effekte
xyplot(RT ~ Transition, group=Subj, type='a',data=data, ylim=c(500, 1000))
xyplot(RT ~ Incongruency, group=Subj, type='a',data=data, ylim=c(500, 1000))
xyplot(RT ~ ColorTrans, group=Subj, type='a',data=data, ylim=c(500, 1000))
xyplot(RT ~ PrepInfo, group=Subj, type='a',data=data, ylim=c(500, 1000))

# Incongruency x ColorTrans x Prepinfo -  hier braucht es eine zweite schicht subject-effekte, um zu sehen was auf Personen-level passeirt
xyplot(RT ~ PrepInfo|Transition, group=ColorTrans, type='a',data=data, ylim=c(500, 1000))
xyplot(RT ~ Incongruency|PrepInfo, group=ColorTrans, type='a',data=data, ylim=c(500, 1000))

xyplot(RT ~ PrepInfo|Transition, group=ColorTrans, type='l',data=data, ylim=c(500, 1000))

rt <- aggregate(RT ~ Subj * Transition * ColorTrans * PrepInfo, data = data, FUN = mean)
n <- aggregate(RT ~ Subj * Transition * ColorTrans * PrepInfo, data = data, FUN = length)

max=aggregate(RT ~ Transition * ColorTrans * PrepInfo, data = n, FUN = max)

rt<-cbind(rt,n=n$RT)

# 'id = ...' is unused in this call but exported to the next
with(rt, 
     xyplot( RT ~ Transition|PrepInfo , group = ColorTrans,  id = Subj, type = 'p', pch = 21 , cex=n/100, 
             auto.key=list(space='right', points=FALSE, lines=TRUE)) ) + 
# 'group = ...' ist not changed in terms of color but in terms of belongingness of points
glayer( panel.xyplot(x, y, group = id, type='l', alpha=.5,...) ) + 
# average horizontally
glayer( panel.average(x, y, type='l', horizontal=F, lwd=3, alpha=.8,...) )


#-----------
library(grid)

pushViewport(viewport(w = 2/3, angle = -90))
print(with(subset(rt, PrepInfo == 'task' ), densityplot( ~ RT, group = ColorTrans) ), newpage = FALSE)
invisible(popViewport())
grid.text(lab = "i.i.d. Normal Variates", rot = 90,
vp = viewport(x = 1/6))


# ------------- Alexander's code

normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
    require(plyr)

    # Measure var on left, idvar + between vars on right of formula.
    data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
     .fun = function(xx, col, na.rm) {
        c(subjMean = mean(xx[,col], na.rm=na.rm))
      },
      measurevar,
      na.rm
    )

    # Put the subject means with original data
    data <- merge(data, data.subjMean)

    # Get the normalized data in a new column
    measureNormedVar <- paste(measurevar, "_norm", sep="")
    data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"]  +
                               #mean(data[,measurevar], na.rm=na.rm)

    # Remove this subject mean column
    data$subjMean <- NULL

    return(data)
}


# ---------------------------

dfwNorm.long <- normDataWithin(data=rt, idvar=c("Subj",'PrepInfo','ColorTrans') , measurevar="RT")


detach(library:lattice)
library(ggplot2)

# solution: subject-effekte, um zu sehen was auf Personen-level passeirt
ggplot(data) +
geom_smooth(method="lm",data=subset(data, ColorTrans == 'r'), aes(x=PrepInfo, y=RT, group=Subj), alpha=.5,se=FALSE, colour='red') +
geom_smooth(method="lm",data=subset(data, ColorTrans == 's'), aes(x=PrepInfo, y=RT, group=Subj), se=FALSE, colour='blue') +
geom_smooth(method="lm",data=data, aes(x=PrepInfo, y=RT, group=ColorTrans, colour=ColorTrans), se=FALSE, lwd=3) 
    
    


# wenn du mit '+ (1|Letter:Number)' über die verwendeten Zahlen unter den Buchstaben generalieseiren willst, 
# und dies mit einer ANOVA vergleichen willst, dann musst du eine 2. ANOVA Eroor(Number/...) rechnen und diese
# mit der ANOVA über die Subjekte Error(Subj/...) verrechnen, siehe Bayen. Analysing linguistic data. oder Bates and Bayen.
LMM <- lmer(RT ~  Transition * Incongruency * ColorTrans * PrepInfo +  (1|Subj/PrepInfo)+ (0+ColorTrans|Subj) +
            (0+Incongruency|Subj), data)
            
summary(LMM)

# RT ~ Transition * Incongruency * ColorTrans * PrepInfo + (1 |Subj/PrepInfo) + (0 + ColorTrans |Subj) + (0 + Incongruency |Subj) 
# Transitions:ColorTranss:PrepInfofeature                  37.6996    22.2185    1.70
