setwd('/home/rene/Dokumente/romy/lmms gaze transfer')
library(knitr)
knit('romy_scratch.Rmd')
system('pandoc romy_scratch.md -o romy_scratch.docx ') 
