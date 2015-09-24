library(foreign)
setwd('/home/rene/Dokumente/DirkRitter/')
g <- read.spss(file = "Gefangenenauswertung (neu, 113 Prob) Version 01.10.13.sav", 
   use.value.labels = TRUE,  # SPSS variables with value labels into R factors with levels
   max.value.labels = Inf,   # can be any real number 
      to.data.frame = TRUE,  # we want it to be an data.frame 
       use.missings = TRUE  # recode SPSS set missings-code to NA<
)


library(spidadev)

# variablen
xqplot(g, ask=T)

# possible outcome vars
Suiz_lt 2/3 : 1/3
SV_lifetime
suizid_aktuel



# Fragen
was sind die AMDP T-Werte
PSSI-T-Werte: wo sind die Rohwerte?
wie mit den zwei Missingszeilen umgehen? FIML, MI?

# das scheien Meinungen von leuten zu sein? variablen sind anders
f <- read.spss(file = "Fachdienstauswertung (neu, 27 Prob).sav", 
   use.value.labels = TRUE,  # SPSS variables with value labels into R factors with levels
   max.value.labels = Inf,   # can be any real number 
      to.data.frame = TRUE,  # we want it to be an data.frame 
       use.missings = TRUE  # recode SPSS set missings-code to NA<
)

xqplot(f, ask=T)




b <- read.spss(file = "Bedienstetenauswertung für Vergleich.sav", 
   use.value.labels = TRUE,  # SPSS variables with value labels into R factors with levels
   max.value.labels = Inf,   # can be any real number 
      to.data.frame = TRUE,  # we want it to be an data.frame 
       use.missings = TRUE  # recode SPSS set missings-code to NA<
)

xqplot(b, ask=T)


# Veränderung also 2 Zeitpunkte, wenn ja wo sind die Rohwerte?









