setwd('~/Dokumente/Eva/')

library(foreign)

music <- read.spss (file = "Musikdaten 2012_reduced.sav", 
                    to.data.frame=TRUE,
                    use.value.labels=FALSE)

library(car)

# Auswahl der benötigten Variablen
# Datensatz durchnummerieren
id <- factor(c(1:125))
id
ma <- music$Note_ma
hsu <- music$Note_hsu
sex <- factor(music$Smale+1)

# Datensatz umstrukturieren
# ma Zeitpunkt 1
# hsu Zeitpunkt 2
install.packages("tidyr")
library(tidyr)
data2 <- data.frame(id, ma, hsu, sex)
data2
data2$id<-factor(data2$id)

data2$sex <- factor(data2$sex)
mdata2 <- gather(data2, Zeitpunkt, Wert, ma, hsu, 
                 na.rm=T, convert=T)

# Variablen müssen Faktoren sein außer Wert
mdata2 <- within(mdata2, {
  sex <- factor(sex)
  id <- factor(id)
  Zeitpunkt <- factor(Zeitpunkt)
})

# Anova mit Messwiederholung
d=with(mdata2, table(id)   )

m=subset(mdata2, id %in% names(which(d==2)) )
m$id=factor(as.character(m$id))
with(m, table(id)   )

summary( fit <- 
         aov(Wert ~ Zeitpunkt+sex + Error(id/Zeitpunkt), data=m)
)



library(ez)

anova = ezANOVA(
    data = m
    , dv = Wert
    , wid = id
    , within = .(Zeitpunkt)
    , between = sex
    , type = 3
)
 
print(anova)
