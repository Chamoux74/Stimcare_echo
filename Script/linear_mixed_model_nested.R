library(lme4)
library(lmerTest)
require(nlme)
library(ggplot2)
library(outliers)
library(kableExtra)
library(pander)
library(tigerstats)
library(performance)
library(MuMIn)
library(phia)
library(emmeans)


dfechofmd$condition <- factor(dfechofmd$condition, levels = c("placebo", "patch"))
dfechofmd$instant <-
  factor(dfechofmd$instant,
         levels = c("BASEPRE", "FROIDPRE", "BASEPOST", "BASEPOST48"))

test <- filter(dfechofmd, instant != "BASEPOST")

modelemixte <- lmer(FMD ~ condition*instant + (1|sujet), data = test, REML = T)

summary(modelemixte)

modelemixte1 <- lme(FMD ~ condition*instant, data= test, random=~1|sujet, method = "REML", na.action = na.omit)


#description data

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# on crée le graphique en indiquant le jeu de données et les variables d'intérêt
p<- ggplot(data=test, aes(x= instant, y= FMD, fill=condition) )
# on indique la forme du graphique, un graphe violon en l'occurrence
p<-p+ geom_violin()
# ici on choisit le pallette de couleur (automatique pour ne pas devoir spécifier manuellement)
p<-p+scale_fill_brewer(palette="PRGn")
# on indique où la légende doit apparaître
p<-p + theme(legend.position="right")
# on affiche chaque point individuel pour identifier la présence éventuelle de valeurs influentes
p<- p+ geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize=1/4)
# on ajoute la moyenne par un point rouge et l'écart-type par un trait rouge.
p<-p + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))
p

#outlier and application condition
## lme model

residus <-residuals(modelemixte1, type="normalized") #1
test$residus<-residus #2
grubbs.test(test$residus, type = 10, opposite = FALSE, two.sided = FALSE)

## clean data

test_clean <- test

# On réalise un boucle du type: tant que la probabilité est inferieure a 0.05, on continue.
data.frame()->valeur.influentes
while(grubbs.test(test_clean$residus, type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  {
  max<-which.max(abs(test_clean$residus)) #cherche la valeur maximale qu'on stocke dans l'objet max                            # récupère les observations considérées comme influentes et les stocke
  valeur.influentes<-rbind(valeur.influentes,test_clean[max, ])
  test_clean<- test_clean[ -max, ] # supprime la valeur maximale de rat.clean
}

##lmer model

test1 <- test

residus2<-residuals(modelemixte, type="pearson",scaled=TRUE)
test1$residus2<-residus2
grubbs.test(test1$residus2, type = 10, opposite = FALSE, two.sided = FALSE)

##clean

test_clean1 <- test1

data.frame()->valeur.influentes
while(grubbs.test(test_clean1$residus2, type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  {
  max<-which.max(abs(test_clean1$residus2)) #cherche la valeur maximale qu'on stocke dans l'objet max                            # récupère les observations considérées comme influentes et les stocke
  valeur.influentes<-rbind(valeur.influentes,test_clean1[max, ])
  test_clean1<-test_clean1[ -max, ] # supprime la valeur maximale de rat.clean
}

## normality test for residuals

n1<-shapiro.test(test$residus)
n2<-shapiro.test(test_clean$residus)
n3<-shapiro.test(test_clean1$residus2)
r<-data.frame(W=c(n1$statistic, n2$statistic, n3$statistic),
              p=c(n1$p.value, n2$p.value, n3$p.value))
dimnames(r)[[1]]<-c("jeu de données complet", "jeu de données nettoyées lme", "jeu de données nettoyées lmer")
kable(pandoc.table(r, style='simple',split.tables=150))

#plot normality

ggplot(test_clean1, aes(x=residus2)) +
  geom_histogram(aes(y=after_stat(density)),      # Histogram with density instead of count on y-ax,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#ggqqplot

ggplot(test_clean, aes(sample=residus))+stat_qq()

#not normally distributed -> log tansformation
test_clean3 <- test_clean

test_clean3$FMD <- sqrt(test_clean$FMD)

modelemixte2 <- lme(FMD ~ condition*instant, data= test_clean3, random=~1|sujet, method = "ML", na.action = na.omit)
modelemixte3 <- lmer(FMD ~ instant*condition +(1|sujet), data = test_clean3, REML = F)

residus3 <- residuals(modelemixte2, type="normalized")
residus4<- residuals(modelemixte3, type="pearson",scaled=TRUE)#1
test_clean3$residus4<- residus4
test_clean3$residus3 <- residus3#2

n4 <- shapiro.test(test_clean3$residus3)
n5 <- shapiro.test(test_clean3$residus4)
r<-data.frame(W=c(n1$statistic, n2$statistic, n3$statistic, n4$statistic, n5$statistic),
              p=c(n1$p.value, n2$p.value, n3$p.value, n4$p.value, n5$p.value))
dimnames(r)[[1]] <-
  c(
    "jeu de données complet",
    "jeu de données nettoyées lme",
    "jeu de données nettoyées lmer",
    "jeu de données transformé sqrt / netoyées lme", "jeu de données transformé sqrt / netoyées lmer"
  )
kable(pandoc.table(r, style='simple',split.tables=150))

#normality random parameters model lmer

aleatoires <- lmer(FMD~1+(1|sujet), data=test_clean3)

pr01 <- profile(aleatoires)
xyplot(pr01, aspect = 1.3, layout=c(3,1))
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)

r_int<- ranef(modelemixte3)$sujet$"(Intercept)"
qqnorm(r_int)
shapiro.test(r_int)

#independance between random var and residual

splom(pr01)

#Testting interest of random effect lmer

ranova(modelemixte3)

#plot interest of random effect

dotplot(ranef(modelemixte3, condVar=T))

#ICC for random factor Sujet

icc(modelemixte3)

#test effet fixe

modelemixte4 <- lmer(FMD ~ condition:instant + (1|sujet), data = test_clean3, REML = F)

anova(modelemixte3, modelemixte4)

#test fixed and random effects

st <- step(modelemixte3) # this function test each fixed and random effect of model and determine
                        #if there is some effects who are not necessary to the mdoel
st

#plot(st)

#determination of variance in model trough F test value

anova(modelemixte3, type = 1)

# effect size

r.squaredGLMM(modelemixte3) # only for normal distributed variable for each condition
r.squaredLR(modelemixte3) # ok for all, with adjusted r squared which is more precise

#testing contrast

contrasts(test_clean3$instant)

#contr.instant(3, base = 1, contrasts = TRUE, sparse = FALSE)

#interaction test

testInteractions(modelemixte3)
means <- interactionMeans(modelemixte3)

plot(means)

testInteractions(modelemixte3, adjustment = "holm") #with holm correction to avoid type 1 error
testInteractions(modelemixte3, pairwise = "condition", fixed = "instant")

#second method to calculate interraction contrast

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(modelemixte3, ~instant*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant"), adjust = "holm")
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)

#plot contrast

emmeans_out1 <- summary(emmeans_out)
pd <- position_dodge(0.1)
ggplot(emmeans_out1, aes(x=instant , y=emmean, colour=condition, group= condition)) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)

#variance homogeneity

modelemixte3_vara <-
  lme(
    FMD ~ condition * instant,
    data = test_clean3,
    random =  ~ 1 | sujet,
    method = "ML",
    na.action = na.omit, weights = varIdent(form = ~1|instant))

VarCorr(modelemixte3_vara)
summary(modelemixte3_vara)$modelStruct$varStruct

#if not homogenous anova between two model

modelemixte1_1 <-  lme(
  FMD ~ condition * instant,
  data = test_clean3,
  random =  ~ 1 | sujet,
  method = "ML",
  na.action = na.omit)

anova(modelemixte1_1, modelemixte3_vara)
