library(car)
library(lme4)
#library(lattice)
options(contrasts=c("contr.sum","contr.poly"))



mig <- read.csv("TrialData.csv")

# since the file is not large, copying is ok..., select relevant cols
m1 <- subset(mig, select=c(RT, Block_type, Trial_type, reply3, Vpn))
# rename in accordance to paper
colnams <- c("reaction_time", "cursor", "triad", "response_type", "subject")
names(m1) <- colnams

# ******************************************************************************************************
# 1. full dataset - test influence of cursor, response_type, triad and their interaction
# ******************************************************************************************************
# trial average for each subject in the factor-combinations
m1.avg <- aggregate(m1$reaction_time, by=list(m1$cursor, m1$response_type, m1$triad, m1$subject), mean)
names(m1.avg) <- c("cursor", "response_type", "triad", "subject", "reaction_time") # rename columns

# plot that stuff:
par(cex.axis=.6, cex.main = .8, cex.lab = .8 )
with(m1.avg, boxplot(reaction_time~response_type+triad+cursor, at = c(1:12), 
                     col=c("white", "gray", "cyan"), 
                     #notch=TRUE,
                     xlab = "factor combination", ylab = "reaction time", main = "Meaning in gaze - reaction times"))
m1.mean <- aggregate(m1.avg$reaction_time, by=list(m1.avg$response_type, m1.avg$triad, m1.avg$cursor), mean)
#m1.median <- aggregate(m1.avg$reaction_time, by=list(m1.avg$response_type, m1.avg$triad, m1.avg$cursor), median)
lines(c(1:12), m1.mean$x, col="red", lwd=2)
points(c(1:12), m1.mean$x, pch = 19, col="red", bg = "red")
legend(.5, 27000, c("mean reaction times"), pch =21, col = "red", pt.bg="red", cex = .8)


# define the linear mixed models: 
# 1. Null-hypothesis
lmm1.0 <- lmer(reaction_time ~ 1 + (1|subject), data = m1.avg, REML = FALSE)
# 2. cursor
lmm1.1 <- lmer(reaction_time ~ cursor + (1|subject), data = m1.avg, REML = FALSE)
# 3. add response type
lmm1.2a <- lmer(reaction_time ~ cursor + response_type + (1|subject), data = m1.avg, REML = FALSE) 
# 4. include interaction between cursor and response type
lmm1.2b <- lmer(reaction_time ~ cursor * response_type + (1|subject), data = m1.avg, REML = FALSE) 
# 5. include triad
lmm1.3a <- lmer(reaction_time ~ cursor * response_type + triad + (1|subject), data = m1.avg, REML = FALSE)
# 6. include interaction of triad with others
lmm1.3b <- lmer(reaction_time ~ cursor * response_type * triad + (1|subject), data = m1.avg, REML = FALSE)


AN.1 <- anova(lmm1.0, lmm1.1, lmm1.2a, lmm1.2b, lmm1.3a, lmm1.3b)


# only unknon responses, cursor and traid
m2.avg <- subset(m1.avg, m1.avg$response_type == "unknown")
#m2 <- subset(m1, m1$response_type == "unknown")
#m2.avg <- aggregate(m2$reaction_time, by=list(m2$cursor, m2$triad, m2$subject), mean)
#names(m2.avg) <- c("cursor", "triad", "subject", "reaction_time")

# 1. Null-.hypothesis
lmm2.0 <- lmer(reaction_time ~ 1 + (1|subject), data = m2.avg, REML = FALSE)
# 2. triad
lmm2.1 <- lmer(reaction_time ~ triad + (1|subject), data = m2.avg, REML = FALSE)
# 3. add cursor
lmm2.2a <- lmer(reaction_time ~ cursor + triad + (1|subject), data = m2.avg, REML = FALSE)
# 4. add interaction
lmm2.2b <- lmer(reaction_time ~ cursor * triad + (1|subject), data = m2.avg, REML = FALSE)
# model selection
AN.2 <- anova(lmm2.0, lmm2.1, lmm2.2a, lmm2.2b)

# ========================================================================================
# -> reduce to the cued trials with response "unknown"
# ----------------------------------------------------------------------------------------
m3.avg <- subset(m1.avg, m1.avg$response_type=="unknown" & m1.avg$triad == "cued")

# define the models:
# 1. Null-hypothesis
lmm3.0 <- lmer(reaction_time ~ 1 + (1|subject), data = m3.avg, REML = FALSE) 
# 2. cursor dependence?
lmm3.1 <- lmer(reaction_time ~ cursor + (1|subject), data = m3.avg, REML = FALSE)  
# model selection
AN.3 <- anova(lmm3.0, lmm3.1)
print("================================================================")
print("Full data set")
print("----------------------------------------------------------------")
print(AN.1)
print("----------------------------------------------------------------")
#print(coef(lmm1.3b))

print("================================================================")
print("only 'unknown' responses")
print("----------------------------------------------------------------")
print(AN.2)
print("----------------------------------------------------------------")

print("================================================================")
print("only cued 'unknown' responses")
print("----------------------------------------------------------------")
print(AN.3)
print("----------------------------------------------------------------")

# print("================================================================")
# print("coefficients")
# print("----------------------------------------------------------------")
# print(coef(lmm3.1))

# # print summaries to file
# sink(file="LMM_gazecued_vs_dotcued.txt")
# print("1.")
# print(summary(lmm1.0))
# print("-------------------------------------------------------------")
# print("-------------------------------------------------------------")
# print(summary(lmm1.1))
# print("-------------------------------------------------------------")
# print("-------------------------------------------------------------")
# sink(NULL)

