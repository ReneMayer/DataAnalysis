#daten$Vp<- as.numeric(daten$Vp)?
daten_feat_steig$Vp<- as.factor(daten_feat_steig$Vp)
daten_feat_steig$condition <- as.factor(daten_feat_steig$condition)
daten_feat_steig$order <- as.factor(daten_feat_steig$order)
daten_feat_steig$diagram <- as.factor(daten_feat_steig$diagram)
daten_feat_steig$featuresteig <- as.factor(daten_feat_steig$featuresteig)
daten_feat_steig$time <- as.factor(daten_feat_steig$time)
daten_feat_steig$correct <-as.factor(daten_feat_steig$correct)

is.factor(daten_feat_steig$Vp)
is.factor(daten_feat_steig$condition)
is.factor(daten_feat_steig$order)
is.factor(daten_feat_steig$diagram)
is.factor(daten_feat_steig$featuresteig)
is.factor(daten_feat_steig$time)
is.factor(daten_feat_steig$correct)

daten_feat_kreuz$Vp<- as.factor(daten_feat_kreuz$Vp)
daten_feat_kreuz$condition <- as.factor(daten_feat_kreuz$condition)
daten_feat_kreuz$order <- as.factor(daten_feat_kreuz$order)
daten_feat_kreuz$diagram <- as.factor(daten_feat_kreuz$diagram)
daten_feat_kreuz$featurekreuz <- as.factor (daten_feat_kreuz$featurekreuz)
daten_feat_kreuz$time <- as.factor(daten_feat_kreuz$time)
daten_feat_kreuz$correct <-as.factor(daten_feat_kreuz$correct)

is.factor(daten_feat_kreuz$Vp)
is.factor(daten_feat_kreuz$condition)
is.factor(daten_feat_kreuz$order)
is.factor(daten_feat_kreuz$diagram)
is.factor(daten_feat_kreuz$featurekreuz)
is.factor(daten_feat_kreuz$time)
is.factor(daten_feat_kreuz$correct)

daten_feat_bez$Vp<- as.factor(daten_feat_bez$Vp)
daten_feat_bez$condition <- as.factor(daten_feat_bez$condition)
daten_feat_bez$order <- as.factor(daten_feat_bez$order)
daten_feat_bez$diagram <- as.factor(daten_feat_bez$diagram)
daten_feat_bez$featurebez <- as.factor(daten_feat_bez$featurebez)
daten_feat_bez$time <- as.factor(daten_feat_bez$time)
daten_feat_bez$correct <-as.factor(daten_feat_bez$correct)

is.factor(daten_feat_bez$Vp)
is.factor(daten_feat_bez$condition)
is.factor(daten_feat_bez$order)
is.factor(daten_feat_bez$diagram)
is.factor(daten_feat_bez$featurebez)
is.factor(daten_feat_bez$time)
is.factor(daten_feat_bez$correct)

#Model 1c, Vorteil: nur ein einziges Modell
#Nachteil: für die dreifach gestufte UV Feature (Steigung, Kreuzung, Beziehung) gibt es keine sinnvolle Referenzkategorie
model1c<- glmer(correct ~ condition + feature + time + condition:time + (1|Vp) + (1|diagram)+ (1|order), daten, family=binomial, doFit=TRUE)
model1c
#3 Modelle: 1 Modell pro Feature
model1<-glmer(correct ~ condition + order + time + condition:time + (1|Vp) + (1|diagram), daten_feat_steig, family=binomial, doFit=TRUE)
model1
model1a<-glmer(correct ~(1+condition | Vp) + order + time + condition:time + (1|diagram), daten_feat_steig, family=binomial, doFit=TRUE)
model1a
model2<- glmer(correct ~condition + order + time + condition:time + (1|Vp) + (1|diagram), daten_feat_kreuz, family=binomial, doFit=TRUE)
model2
model2a<- glmer(correct ~(1+condition | Vp) + order + time + condition:time + (1|diagram), daten_feat_kreuz, family=binomial, doFit=TRUE)
model2a
#modell 3: signifikant negativer Intercept --> was heißt das?
model3<- glmer(correct ~condition + order + time + condition:time + (1|Vp) + (1|diagram), daten_feat_bez, family=binomial, doFit=TRUE)
model3
model3a<- glmer(correct ~(1+condition | Vp) + order + time + condition:time + (1|diagram), daten_feat_bez, family=binomial, doFit=TRUE)
model3a