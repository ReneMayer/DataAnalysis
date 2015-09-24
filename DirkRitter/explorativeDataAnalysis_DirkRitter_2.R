library(foreign)
setwd('/home/rene/Dokumente/DirkRitter/')


s <- read.spss(file = "10 Suizidakten/Vgl. Suizid-Kontroll (incl. Daten).sav", 
   use.value.labels = TRUE,  # SPSS variables with value labels into R factors with levels
   max.value.labels = Inf,   # can be any real number 
      to.data.frame = TRUE,  # we want it to be an data.frame 
       use.missings = TRUE  # recode SPSS set missings-code to NA<
)

names(s)

#xqplot(s, ask=T)

library(mice)
library(VIM)


md.pattern(s)

sg=s[,  !names(s) %in% c("Inhaft_Dat", "Dat_SPB", "Su_erhdat", "angekünd", "Absch_br", "Grund_Sui", "Beson_Sui", "angekünd", "Suizidvorgeschichte",
"Anz_SV", "SV_Wann", "Sui_1_Wo", "Suizidmeth_1", "Suizidmeth_2" , "Tt_WT", "Tt_Ft", "Tt_FT_WE", "TT_Jz", "Tz_Az", "Ergeb_GA", "Art_Erkrank2",
"SUR_1","SUR_2", "SUR_3", "SUR_4", "SUR_5", "SUR_6" , "Su_erhdat" , "Dat_SPB" , "Inhaft_Dat", "Nr","Code" ) ]


## convert strage spss -date format into usual dates, and yes we need 'chron'

library(chron)

# suizidgruppe fehlt
as.Date(s$Inhaft_Dat/86400, origin = "1582-10-14") # spss saves in seconds starting from "1582-10-14"

as.Date(s$Dat_SPB/86400, origin = "1582-10-14") # spss saves in seconds starting from "1582-10-14"

#install.packages('xlsx', dep=T) #15.08.2005



require(xlsx)
dd=read.xlsx2("Zsfsg. Suizidakten (Endversion).xlsx", sheetName = "Tabelle1")
write.table(t(dd), file='dd.txt', row.names = F,  col.names = F)
dd=read.table('dd.txt', header=T)

#  ---- hier weiter ...
as.Date(dd$Haftbeginn.aktuell, origin = "1899-12-30") # spss saves in seconds starting from "1582-10-14"



g=s[,  names(s) %in% c("Inhaft_Dat", "Dat_SPB", "Su_erhdat", "angekünd",  "Grund_Sui", "Beson_Sui", "angekünd", "Suizidvorgeschichte",
"Anz_SV", "SV_Wann", "Sui_1_Wo", "Suizidmeth_1", "Suizidmeth_2" , "Tt_WT", "Tt_Ft", "Tt_FT_WE", "TT_Jz", "Tz_Az", "Ergeb_GA", "Art_Erkrank2",
"SUR_1","SUR_2", "SUR_3", "SUR_4", "SUR_5", "SUR_6" , "Su_erhdat" , "Dat_SPB" , "Inhaft_Dat", "Nr","Code" ) ]

## noch dazu! 
# Inhaft_Dat  -   Dat_SPB
#  
# 
# Anz_SV
# SV_Wann 
# Sui_1_Wo
# Ergeb_GA





# Nr 51 fehlt - evtl. einlesefehler
aggr(sg[-51,], prop = F, numbers = T, cex.axis=.6)


sg=sg[-51,]
# skalennivau: kontinuierlich und faktoriel, nicht ordinal - das wird ein Fehler sin, aber der ist nicht so wichtig
( o=sapply( sg, class  ) )

ftable(  (o[ order( o ) ]) )




d=md.pattern(sg)
d[8,] 


# nur 37 vollständige Fälle, 9 vollständige Variablen
# beginne mit der Imputation
mi=mice(data=sg)

# bei sg$Mißbr_Such kommt eine Faktorstufe nie vor. Entferne diese
cbind(unique(sg$Mißbr_Such))
levels(sg$Mißbr_Such)

sg$Mißbr_Such = factor(as.character(sg$Mißbr_Such))

mi=mice(data=sg)
attributes(mi)

library(psy)

mm=complete(mi)

m_fpca=fpca(data=mm, Gruppe~.)


# wir haben aufgrund der unterschiedlichen skalennivaus mixed data ... 
# Lösungen: 	1. verwende polychorische cov [wieso nicht!, aber bedarf dummy coding aller factoren]
#		2. FactoMineR kann mit mixed data umghen [vermtl. am leichtesten]
#		3. cannonical correspondence analysis mit cca( . ~ gruppe, ...) [vermtl. am besten]

# FactoMineR
# install.packages('FactoMineR', dep=T)
library(FactoMineR)



# variablen-Gruppen
#
# Gruppe
#
# demographics: 
#  "JVA", "Alter_Inh", "Alter_E", "Geschlecht", "Sprache", "Alt_Inhaft", "Alt_End" 
# 
# soziographics: 
#  "Familie", "Kind", "Kinder", "Bezieh", "Besuche", "National"
# 
# education: 
#    "Religion", "Schule", "Ausbild"
#
# Haft
#	"Mon_Haftbeg", "Haftart", "Verleg", "Anz_Verl", "Dau_Inha", "Gesamtst", "Arb_Haft", "Unterbri",  "Verh_Haft"  
#
# Dilikt
#   "Indexdelikt", "Hafterfa", "Anzahl_Vo", "Jurist_Dat"
#
# Suizidrelatiert
#  "SPB",  "Erg_SPB", "Erg_zsfs", "SPB_TT", "Suizidversuche" 
#
# Risikofaktoren
#  "Arbeitstätigkeit", "kindheit", "PP_Auff", "GA_vorh", "Psych_Erk", "Anzahl_Erkr", "Art_Erkrank", "Mißbr_Such", "Behandlung", "Raucher", "VISCI"
#

osg = sg[ , c(
 "Gruppe", 
 "JVA",  "Geschlecht", "Sprache", "National", 
 "Alter_Inh", "Alter_E", "Alt_Inhaft", "Alt_End",  
 "Familie", "Kind",  "Bezieh", "Besuche", 
 "Religion", "Schule", "Ausbild",
 "Haftart", "Arb_Haft", "Unterbri",  "Verh_Haft", 
 "Anz_Verl", "Dau_Inha", 
 "Indexdelikt", "Hafterfa", 
 "SPB",  "Erg_SPB", "Erg_zsfs", "Suizidversuche", "Arbeitstätigkeit", "kindheit", "PP_Auff", "GA_vorh", "Psych_Erk",  "Art_Erkrank", "Mißbr_Such", "Behandlung", "Raucher", "VISCI",
 "Anzahl_Erkr") ]
                                       
# > names(sg)[25]
# [1] "Gesamtst"


d=
data.frame(
          group= c(1,             4,      4,          4,           3,       4,        2,          2,      14,          1 ), 
          type= c("n",         "n",    "c",         "n",         "n",    "n",        "c",      "n",    "n",          "c"),
    name.group= c("suicide","demo", "time", "relations", "education", "haft", "delinq.c", "delinq", "risk", "disorder.c"))
  
  
data.frame( d[rep(seq_len(nrow(d)), d$group),], sapply(osg, class), names(sapply(osg, class)), sapply(osg, function(x) length(levels(x))) )

require(missMDA)
completed <- imputeMFA(osg,ncp=2,
group= c(1,             4,      4,          4,           3,       4,        2,          2,      14,          1 ), 
          type= c("n",         "n",    "c",         "n",         "n",    "n",        "c",      "n",    "n",          "c"),
    name.group= c("suicide","demo", "time", "relations", "education", "haft", "delinq.c", "delinq", "risk", "disorder.c"))

res <- MFA(osg,
         group= c(1,             4,      4,          4,           3,       4,        2,          2,      14,          1 ), 
          type= c("n",         "n",    "c",         "n",         "n",    "n",        "c",      "n",    "n",          "c"),
    name.group= c("suicide","demo", "time", "relations", "education", "haft", "delinq.c", "delinq", "risk", "disorder.c"))

summary(res, nbelements=Inf)




# eigenwerte zerfallen negativ exponentiell für 30% der varianz in den Daten brauchen wir 5 Dimensionen
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))


# Missings in diesem Datensatz eher gering

# wie gut sind die gruppen getrennt?
# cats = apply(osg, 2, function(x) nlevels(as.factor(x)))
coord = data.frame(res$ind$coord)

library(ggplot2)

ggplot(data = coord, aes(x = Dim.1, y = Dim.2, group=osg$Gruppe)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  stat_density2d(geom="density2d", aes(color = osg$Gruppe,alpha=..level..),
                 size=1,
                 contour=TRUE)
                 
  
# stimmt es, dass die numerischen Kovariten orthogonal zur Gruppe sind?
# LDA

library(MASS)


# genau genommen sind die alle ordinal, d.h. poisson und nicht normalverteilt. (evtl. später)
dd=completed$completeObs
res.lda=lda( Gruppe ~  Anzahl_Erkr + Dau_Inha +  Anz_Verl + Alt_Inhaft + Alter_E + Alter_Inh , data = dd)

# auch hier tun wir uns den default-plot nciht an
# library(devtools)
# install_github('fawda123/ggord')
# stimmt, nur eine LDA-Achse, das geht also nicht

# nur 5% richtig klassifiziert  - stimmt also, d.h. orthogonal
res.lda$svd^2

# graphisch sieht man das auch!
p=predict(res.lda, dd[,c('Anzahl_Erkr','Dau_Inha','Anz_Verl','Alt_Inhaft','Alter_E','Alter_Inh')])
df=data.frame(LDA1=p$x, g=dd$Gruppe)

ggplot(df, aes(x=LD1, fill=g, colour=g)) + geom_density(alpha=.3) +
  geom_rug(data=subset(df,g=="Suizidenten"),aes(x=LD1)) +
  geom_rug(data=subset(df,g=="Kontrollgruppe"),aes(x=LD1),sides="t")


# library(ggord)
# ggord(res.lda)

library(iplots)

data=dd[,c('Anzahl_Erkr','Dau_Inha','Anz_Verl','Alt_Inhaft','Alter_E','Alter_Inh','Gruppe')]

# auch nichts zu sehen!
attach(data)
ihist(Alter_E)
ibar(Gruppe)
iplot(mpg, wt)
ibox(data[ , c('Anzahl_Erkr','Dau_Inha','Anz_Verl','Alt_Inhaft','Alter_E','Alter_Inh')])
imosaic( , )
detach(data)





# mache weiter mit mixedPCA COV und LDA. Das wird die obigen Analyse validieren und gleichzeitig die Variablen weiter Bündeln.
# install.packages('PCAmixdata', dep=T)

library(PCAmixdata)

quali = osg[, sapply(osg, class) == 'factor']
quanti = osg[, sapply(osg, class) == 'numeric']

pca.mix=PCAmix(X.quanti = quanti, X.quali = quali, rename.level=TRUE)

sapply(quali, function(x) levels(x)) # warnung, dass faktorstufen 'levels' von verschiedenen faktoren gleich benannt sind -> imbenennnen

summary(pca.mix) #  Returns the matrix of squared loadings.

barplot(pca.mix$eig[,1],main="Eigenvalues",names.arg=1:nrow(pca.mix$eig))

# auch mit pca.mix scheint der Datensatz hochdimensional
pca.mix$eig

par(mfrow=c(2,2))
plot(pca.mix, choice = "levels")
plot(pca.mix, choice = "sqload") 
plot(pca.mix, choice = "cor") 
plot(pca.mix, choice = "ind")
par(mfrow=c(1,1))

#install.packages('ClustOfVar', dep=T)
library(ClustOfVar)

tree <- hclustvar(X.quanti = quanti, X.quali = quali) # alternativ. kmeansvar()
plot(tree)

part <- cutreevar(tree, 6)
summary(part)

# Squared correlation (resp. correlation ratio) between Gruppe and the synthetic variable of cluster4 =  0.75

## dritte Methode : decision-calssification trees
#install.packages("VSURF", dep=T)
# CART - root of all evel!
library(VSURF)

cc=completed$completeObs

x=cc[, names(cc) != 'Gruppe'] 
suicide=cc$Gruppe
s.vsurf <- VSURF(x, suicide)

names( x[s.vsurf$varselect.thres])    # candidates
names( x[s.vsurf$varselect.interp])   #  terminal nodes

# confusion matrix? - vsurf ruft randomForest auf, diese gibt die confusion-matrix aus
# out of bag error estimate: oob ... bagging and boosting?
# internal nodes (cut|variable)

# wir müssen VSURF.R oder unterfunktionen hacken, damit wir den pruned tree bekommen!
#    alles byte-compiled nehme den quellcode: der forest wird nicht zurückgegeben, aber alles basiert auf randomForest

library(randomForest)

s.rf = randomForest(Gruppe ~ Mißbr_Such + Besuche + Bezieh + VISCI, data = tree.data)
getTree(rfobj = s.rf, k=1, labelVar=T)

require(tree)
t1 <- tree(Gruppe ~ ., data = tree.data)
summary(t1)
plot(t1);text(t1, cex=.8)


# besuch keine Angaben ~ 

library(lattice)

with( s, xyplot(
(Alter_E - Alter_Inh) ~ Besuche, jitter.x=T,group=Gruppe, auto.key=T)
)



plot(s.vsurf);  text(t, cex=.8)

#      plot(s.vsurf, nvar.imp.mean = 50, nvar.imp.sd = 50)
#      
#      ## Step 1
#      suicide.thres <- VSURF_thres(x, suicide) 
#      # Thresholding step is dedicated to roughly eliminate irrelevant
# #      variables a the dataset. This is the first step of the ‘VSURF’
# #      function. For refined variable selection, see VSURF other steps:
# #      ‘VSURF_interp’ and ‘VSURF_pred’.
#      plot(suicide.thres)
#      plot(suicide.thres, nvar.imp.mean = 70, imp.sd = FALSE)
#      
#      ## Step 2
# #      Interpretation step aims to select all variables related to the
# #      response for interpretation prupose. This is the second step of
# #      the ‘VSURF’ function. It is designed to be executed after the
# #      thresholding step ‘VSURF_thres’.
# #      suicide.interp <- VSURF_interp(x, suicide, vars = suicide.thres$varselect.thres)
#      plot(suicide.interp, var.names = TRUE)
#      
#      ## Step 3 
# #      Prediction step refines the selection of intepretation step
# #      ‘VSURF_interp’ by eliminating redundancy in the set of variables
# #      selected, for prediction prupose. This is the third step of the
# #      ‘VSURF’ function.
#      suicide.pred <- VSURF_pred(x, suicide, err.interp = suicide.interp$err.interp,
#                              varselect.interp = suicide.interp$varselect.interp)
#      plot(suicide.pred, var.names = TRUE)
#      ## End(Not run)
     
     
# draw the tree     
library(rpart)

 tree.data=cc[ , c("Gruppe", "Mißbr_Such", "Besuche", "Bezieh", "VISCI")]
 
fit <- rpart(Gruppe ~ Mißbr_Such + Besuche + Bezieh + VISCI, data = tree.data, method="class",  
  control = rpart.control(minsplit=2, minbucket = 1,cp = -1))
 
par(mfrow = c(1,1), xpd = NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit, pretty=1,  use.n = TRUE, cex=.7)
text(fit, use.n = TRUE)

# install.packages('plotmo', dep=T)
library(plotmo)

plotmo(fit, type="prob", nresponse="Suizidenten")

plotcp(fit)

# install.packages('rpart.plot', dep=T)
library(rpart.plot)
dev.new()




 

# Matrix of substitutions

 



# swiss2 <- function( data){
#          a <- c("ä", "ae")
# A <- c("Ä", "Ae")
# u <- c("ü", "ue")
# U <- c("Ü", "Ue")
# o <- c("ö", "oe")
# O <- c("Ö", "Oe")
# 
#    m <- cbind(a,A,u,U,o,O, deparse.level=0) 
#    for (i in seq_len(ncol(m))){
#        data <- gsub(paste(m[,i], sep=",")[1],paste(m[,i], sep=",")[2], data,
#                 ignore.case=F, perl = F, fixed = F, useBytes = F)
#    }
#    data
# }
# 
# 
# tree.data=apply(FUN=swiss2, MAR=2, tree.data)
# 
# tree.data=data.frame(lapply(FUN=factor, X=data.frame(tree.data)))
# 
# 
# sapply(tree.data, function(x) iconv(levels(x), "latin1", "ASCII", sub=""))
# 
tree.data=data.frame(lapply(X=tree.data, FUN=function(x) iconv(x, "latin1", "ASCII", sub="") ))





prp(fit, 
box.col=c("pink", "palegreen3")[fit$frame$yval], nn=TRUE, extra=101)

 prp( fit, type=4, box.col=c("pink", "palegreen3")[fit$frame$yval],
 extra=101, nn=TRUE, fallen.leaves=TRUE,faclen=0, varlen=0, branch.lty=3)

 heat.tree <- function(tree, low.is.green=FALSE, ...) { # dots args passed to prp
y <- tree$frame$yval
if(low.is.green)
y <- -y
max <- max(y)
min <- min(y)
cols <- rainbow(3, end=.36)[
ifelse(y >  y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
(y-min)  * (50-1)  / (y[1]-min) + 1)]
prp(tree, branch.col=cols, box.col=cols, ...)
}
 
 heat.tree(fit, type=4, varlen=0, faclen=0, fallen.leaves=TRUE)
 