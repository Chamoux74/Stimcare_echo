library(xml2)
library(tibble)
library(purrr)
library(dplyr)
library(tidyverse)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(zoo)
library(outliers)
library(pracma)

echopatch <-
  list.files(
    path = "C:/Users/maxch/Nextcloud/Data_STIMCARE/ECHO/data/PATCH",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

read <- function(bob) {
  read.csv(bob , sep = ";", dec = ",", skip = 7)
  }

dfechopatch <- lapply(echopatch , read)
names(dfechopatch) <- tools::file_path_sans_ext(basename(echopatch))

remove <-
  function(bob) {
    bob[,8:12]
  }

dfechopatch <- lapply(dfechopatch , remove)

n <- c("temps",
       "diameterinstant",
       "diametermean",
       "positivespeed",
       "negativespeed")

dfechopatch <- lapply(dfechopatch , setNames , n)

dfechopatch<- lapply(dfechopatch, na.omit)

dfechopatch <- lapply(dfechopatch , function (bob){rownames(bob) <- 1:nrow(bob)
return(bob)})

#selection des 2 mesures : BASE / OCLU

echopatchbase <- dfechopatch[grepl("BASE" , names(dfechopatch))]
echopatchoclu <- dfechopatch[grepl("OCLU" , names(dfechopatch))]

echopatchoclu$BastienRiegerPREBOCLUFROID$diameterinstant <-
  as.numeric(echopatchoclu$BastienRiegerPREBOCLUFROID$diameterinstant)

echopatchoclu$BastienRiegerPREBOCLUFROID$diametermean <-
  as.numeric(echopatchoclu$BastienRiegerPREBOCLUFROID$diametermean)

echopatchoclu$BastienRiegerPREBOCLUFROID$positivespeed <-
  as.numeric(echopatchoclu$BastienRiegerPREBOCLUFROID$positivespeed)

echopatchoclu$BastienRiegerPREBOCLUFROID$negativespeed <-
  as.numeric(echopatchoclu$BastienRiegerPREBOCLUFROID$negativespeed)

#moyenne du diamètre sur 10"

meandiam <-
  function(bob) {
    mean(bob$diameterinstant[100:410], na.rm = T)
  }

dfmeanbasepatch <- as.data.frame(lapply(echopatchbase , meandiam))
dfmeanbasepatch <- as.data.frame(t(dfmeanbasepatch))

colnames(dfmeanbasepatch)[1] <- "meandiameter"

test_type <- gsub(".*?(BASE|FROID).*", "\\1" , row.names(dfmeanbasepatch))
timing <- gsub(".*?(PRE|POST|POST48).*", "\\1" , row.names(dfmeanbasepatch))
sujet <- substr(rownames(dfmeanbasepatch), 1, 8)
patch <- "patch"

#ajouter la condition, l'instant de mesure, le sujet, et froid ou normal , base ou oclu

dfmeanbasepatch <- cbind(dfmeanbasepatch, test_type, timing, sujet, patch)
colnames(dfmeanbasepatch)[5] <- "condition"

#Calcule du diamètre pic

#plot oclu pour voir la selection du max


ggplot(echopatchoclu$PierreEmmanuelleNaulletPOSTAOCLU, aes(x = temps , y = diameterinstant , na.rm = TRUE))+
  geom_point() +
  geom_line()
#selectionner diamètre pic

test <- filter(echopatchoclu$PierreEmmanuelleNaulletPOSTAOCLU, temps > 86000 & temps < 86500)

ggplot(test, aes(x = temps , y = diameterinstant , na.rm = TRUE))+
  geom_point(color = "red") +
  geom_line()

max(test$diameterinstant)

which.max(echopatchoclu$ALBANLEGALLPOST48AOCLU$diameterinstant)

which(echopatchoclu$PierreEmmanuelleNaulletPOSTAOCLU$diameterinstant == 6.399)

#calcule FMD
#chargement du fichier avec les numéros de ligne

linenumberspatch <- read.csv("C:/Users/maxch/Nextcloud/Data_STIMCARE/ECHO/data/linenumberspatch.csv")
linenumberspatch <- column_to_rownames(linenumberspatch , var = "nom")

#extraction du diamètre pic en fonction de l'index de la ligne selectionné
#graphiquement puis stockage dans un dataframe
#mesure 1

selection_lignes <- function(df, nombre) {
  return(data.frame(temps = df$temps[nombre],
                    diameterinstant = df$diameterinstant[nombre],
                    diametermean = df$diametermean[nombre]))
}

# Utilisation de lapply pour appliquer la fonction à chaque dataframe de la liste

dfpicdiampatch <- lapply(seq_along(echopatchoclu), function(bob) {
  nombre <- linenumberspatch$linenumber[bob]
  selection_lignes(echopatchoclu[[bob]], nombre)
})

dfpicdiampatch <- do.call(rbind, dfpicdiampatch)

dfpicdiampatch <- as.data.frame(dfpicdiampatch[,2])
rownames(dfpicdiampatch) <- names(echopatchoclu)
colnames(dfpicdiampatch)[1] <- "peakdiameter"

#liaison avec les diamètre moyen pour calcule du FMD

dfpatch <- cbind(dfpicdiampatch, dfmeanbasepatch)
dfpatch <- mutate(dfpatch , FMD = (peakdiameter-meandiameter)/meandiameter*100)
dfpatch <- mutate(dfpatch , FMD_abs = peakdiameter-meandiameter)
dfpatch <- mutate(dfpatch, instant = paste0(test_type , timing))
dfpatch <- dfpatch[,-c(3,4)]

#calcule du shear rate diamètre de base

speeddiamb <-
  function(bob) {
    mean(bob$positivespeed[100:410], na.rm = T)
  }

dfpatch <-
  mutate(dfpatch, speedbase = as.numeric(lapply(echopatchbase , speeddiamb)))

dfpatch <-
  mutate(dfpatch,
         bloodflowbase = speedbase * (3.141593 / 4) * ((meandiameter / 10) * (meandiameter /
                                                                                10)))

dfpatch <- mutate(dfpatch, SRbase = (8*speedbase)/(meandiameter/10))

#calcule speed, shear rate pour le diamètre pic

# selection_lignes_pospeed <- function(df, nombre) {
#   return(positivespeed = df$positivespeed[nombre])
# }

dfpatch <-
  mutate(dfpatch, meanvelocity = as.numeric(lapply(echopatchoclu, function(bob) {
    (max(bob$positivespeed)/2)
  })))

dfpatch <-
  mutate(dfpatch,
         bloodflowpic = meanvelocity * (3.141593 / 4) * ((peakdiameter / 10) * (peakdiameter /
                                                                                           10)))

dfpatch <- mutate(dfpatch, SRpic = (8*meanvelocity)/(peakdiameter/10))

select_temps <- function(df, nombre) {
  return(tempspic = (df$temps[nombre] - df$temps[1])/1000)
}

dfpatch <-
  mutate(dfpatch, time_to_peak = as.numeric(lapply(seq_along(echopatchoclu), function(bob) {
    nombre <- linenumberspatch$linenumber[bob]
    select_temps(echopatchoclu[[bob]], nombre)
  })))

select_AUC <- function(df, nombre) {
  return(hypermia_AUC = (trapz(df$temps[1:nombre], df$positivespeed[1:nombre]))/1000)
}

dfpatch <-
  mutate(dfpatch, hyperemia_AUC = as.numeric(lapply(seq_along(echopatchoclu), function(bob) {
    nombre <- linenumberspatch$linenumber[bob]
    select_AUC(echopatchoclu[[bob]], nombre)
  })))
