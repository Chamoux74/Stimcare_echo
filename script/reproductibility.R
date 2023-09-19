#library

library(purrr)
library(dplyr)
library(tidyverse)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(BlandAltmanLeh)
library(goeveg)
library(irr)
library(survival)
#devtools::install_github("jcherubini/Rtery")

# extraction mesure 1

mesure1 <-
  list.files(
    path = "C:/Users/maxch/Git/ECHO/data/test/mesure_1",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

read <- function(bob) {
  read.csv(bob , sep = ";", dec = ",", skip = 7)
}

listmesure1 <- lapply(mesure1 , read)
names(listmesure1) <- tools::file_path_sans_ext(basename(mesure1))

remove <-
  function(bob) {
    select(bob ,
           8:10)
  }

listmesure1 <- lapply(listmesure1 , remove)

n <- c("temps",
       "diameterinstant",
       "diametermean")

listmesure1 <- lapply(listmesure1 , setNames , n)

listmesure1 <- lapply(listmesure1, na.omit)

listmesure1 <- lapply(listmesure1 , function (bob){rownames(bob) <- 1:nrow(bob)
return(bob)})

#extraction mesure 2

mesure2 <-
  list.files(
    path = "C:/Users/maxch/Git/ECHO/data/test/mesure_2",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

listmesure2 <- lapply(mesure2 , read)
names(listmesure2) <- tools::file_path_sans_ext(basename(mesure2))

listmesure2 <- lapply(listmesure2 , remove)

listmesure2 <- lapply(listmesure2 , setNames , n)

listmesure2 <- lapply(listmesure2, na.omit)

listmesure2 <- lapply(listmesure2 , function (bob){rownames(bob) <- 1:nrow(bob)
return(bob)})

#séparation BASE/OCLU

listmesure1base <- listmesure1[grepl("BASE" , names(listmesure1))]
listmesure1oclu <- listmesure1[grepl("OCLU" , names(listmesure1))]

listmesure2base <- listmesure2[grepl("BASE" , names(listmesure2))]
listmesure2oclu <- listmesure2[grepl("OCLU" , names(listmesure2))]

#calcule diamètre moyen base sur 10"

meandiam <-
  function(bob) {
    mean(bob$diameterinstant[991:1291], na.rm = T)
  }

dfmeanbasemesure1 <- as.data.frame(lapply(listmesure1base , meandiam))
dfmeanbasemesure1 <- as.data.frame(t(dfmeanbasemesure1))

colnames(dfmeanbasemesure1)[1] <- "meandiameter1"

dfmeanbasemesure2 <- as.data.frame(lapply(listmesure2base , meandiam))
dfmeanbasemesure2 <- as.data.frame(t(dfmeanbasemesure2))

colnames(dfmeanbasemesure2)[1] <- "meandiameter2"

dfmeanbase <- cbind(dfmeanbasemesure1, dfmeanbasemesure2)

#plot blant altman

attach(dfmeanbase)
bland.altman.plot(meandiameter1, meandiameter2 , conf.int = 0.95, silent = FALSE)


icc(dfmeanbase, model = "oneway", type = "agreement")

write.csv(dfmeanbase, "C:/Users/maxch/Documents/Analyse/DATA/repeatability_echo.csv")

##FMD analysis

#plot oclu pour voir la selection du max

ggplot(listmesure1oclu$BriceAlmuniaPREBOCLU, aes(x = temps , y = diameterinstant , na.rm = TRUE))+
  geom_point() +
  geom_line()
#selectionner diamètre pic

test <- filter(listmesure1oclu$BriceAlmuniaPREBOCLU, temps > 120000 & temps < 130000)

ggplot(test, aes(x = temps , y = diameterinstant , na.rm = TRUE))+
  geom_point(color = "red") +
  geom_line()

max(test$diameterinstant)

which(listmesure1oclu$ALBANTESTOCLU1$diameterinstant == 6.444)

#fonction mesure FMD

linenumbers1 <- read.csv("C:/Users/maxch/Git/ECHO/data/test/listnbmesure1.csv")
linenumbers1 <- column_to_rownames(linenumbers1 , var = "nom")

linenumbers2 <- read.csv("C:/Users/maxch/Git/ECHO/data/test/listnbmesure2.csv")
linenumbers2 <- column_to_rownames(linenumbers2 , var = "nom")

#extraction du diamètre pic en fonction de l'index de la ligne selectionné
#graphiquement puis stockage dans un dataframe
#mesure 1

selection_lignes <- function(df, nombre) {
  return(data.frame(temps = df$temps[nombre],
                    diameterinstant = df$diameterinstant[nombre],
                    diametermean = df$diametermean[nombre]))
}

# Utilisation de lapply pour appliquer la fonction à chaque dataframe de la liste

dfpicdiammesure1 <- lapply(seq_along(listmesure1oclu), function(bob) {
  nombre <- linenumbers1$linenumbers[bob]
  selection_lignes(listmesure1oclu[[bob]], nombre)
})

dfpicdiammesure1 <- do.call(rbind, dfpicdiammesure1)

dfpicdiammesure1 <- as.data.frame(dfpicdiammesure1[,2])
rownames(dfpicdiammesure1) <- names(listmesure1oclu)
colnames(dfpicdiammesure1)[1] <- "peakdiameter1"

#mesure 2

dfpicdiammesure2 <- lapply(seq_along(listmesure1oclu), function(bob) {
  nombre <- linenumbers2$linenumbers[bob]
  selection_lignes(listmesure2oclu[[bob]], nombre)
})

dfpicdiammesure2 <- do.call(rbind, dfpicdiammesure2)

dfpicdiammesure2 <- as.data.frame(dfpicdiammesure2[,2])
rownames(dfpicdiammesure2) <- names(listmesure2oclu)
colnames(dfpicdiammesure2)[1] <- "peakdiameter2"

dfbase <- dfmeanbase[-c(6,8),]

dffinal <- cbind(dfbase, dfpicdiammesure1, dfpicdiammesure2)

dffinal <- mutate(dffinal , FMD1 = (peakdiameter1-meandiameter1)/meandiameter1*100)
dffinal <- mutate(dffinal , FMD2 = (peakdiameter2-meandiameter2)/meandiameter2*100)

#reproductibility

dffinal <- dffinal[,c(5,6)]

attach(dffinal)
bland.altman.plot(FMD1, FMD2 , conf.int = 0.95, silent = FALSE)

icc(dffinal, model = "oneway", type = "agreement")

write_csv(dffinal, "C:/Users/maxch/Documents/Analyse/DATA/repeatability_FMD.csv")
