library(sjPlot)

# plot to investigated crossed fixed and random effect in repeated measures
interaction.plot(test_clean3$condition, test_clean3$sujet, test_clean3$FMD, las=1,
                 trace.label="identifiant du sujet", xlab="Traitement", ylab="Flux médié par dilatation (sqrt transform)")

interaction.plot(test_clean3$instant, test_clean3$sujet, test_clean3$FMD, las=1,
                 trace.label="identifiant du sujet", xlab="instant", ylab="Flux médié par dilatation (sqrt transform)")

#analysis different crossed model

modelemixte3_1 <- lmer(FMD ~ instant*condition +(1|sujet/condition), data = test_clean3, REML = F)
modelemixte1_1 <- lme(FMD ~ condition*instant, data= test_clean3, random=~1|sujet, method = "ML", na.action = na.omit)


anova(modelemixte3 ,modelemixte3_1)

lmeControl(modelemixte3_1)

# plot individual variation

p<-ggplot(test_clean3, aes(x=instant, y=FMD, colour=condition, group=condition))+
  geom_line() +
  facet_wrap(~sujet)
p

#fmdgl <- groupedData(FMD ~ instant | sujet,
                         #outer = ~ condition, data = test_clean3)
plot(fmdgl, display = "sujet", outer = TRUE,
     key = F, xlab = "Timing mesure", ylab = "FMD (sqrt)",
     main = "Données individuelle en fonction de la codition ")

#covariance matrix structure

ACF(modelemixte3_1)

modelemixte3_1 <- update(modelemixte3_1, correlation=corAR1(form=~1|sujet))
#covariance structure = autoregressiv

anova(modelemixte3_1, modelemixte3_2)
#no differences, with spécifics covariance model

#plot linear model

plot_model(modelemixte3, show.values = T, width = 0.1)

newdat <- expand.grid(condition=unique(test_clean3$condition),
                      instant=c(min(as.numeric(test_clean3$instant)),
                            max(as.numeric(test_clean3$instant))))

newdat$instant <- as.factor(newdat$instant)

pLMM <- ggplot(test_clean3, aes(x=instant, y=FMD, colour=condition)) +
  geom_point(size=3) +
  geom_line(aes(y=predict(modelemixte1_1), group= sujet, size="sujet")) +
  geom_line(data=newdat, aes(y=predict(modelemixte1_1 , level=0, newdata=newdat), size="Population")) +
  scale_size_manual(name="Predictions", values=c("sujet"=0.5, "Population"=3)) +
  theme_bw(base_size=22)

pLMM
