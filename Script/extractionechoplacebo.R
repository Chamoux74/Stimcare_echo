echoplacebo <-
  list.files(
    path = "C:/Users/maxch/Nextcloud/Data_STIMCARE/ECHO/data/PLACEBO",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

dfechoplacebo <- lapply(echoplacebo , read)
names(dfechoplacebo) <- tools::file_path_sans_ext(basename(echoplacebo))

dfechoplacebo <- lapply(dfechoplacebo , remove)

dfechoplacebo <- lapply(dfechoplacebo , setNames , n)

dfechoplacebo <- lapply(dfechoplacebo, na.omit)

dfechoplacebo <- lapply(dfechoplacebo , function (bob){rownames(bob) <- 1:nrow(bob)
return(bob)})

#selection des 2 mesures : BASE / OCLU

echoplacebobase <- dfechoplacebo[grepl("BASE" , names(dfechoplacebo))]
echoplacebooclu <- dfechoplacebo[grepl("OCLU" , names(dfechoplacebo))]

#moyenne du diamètre sur 10"

dfmeanbaseplacebo <- as.data.frame(lapply(echoplacebobase , meandiam))
dfmeanbaseplacebo <- as.data.frame(t(dfmeanbaseplacebo))

colnames(dfmeanbaseplacebo)[1] <- "meandiameter"

test_type <- gsub(".*?(BASE|FROID).*", "\\1" , row.names(dfmeanbaseplacebo))
timing <- gsub(".*?(PRE|POST|POST48).*", "\\1" , row.names(dfmeanbaseplacebo))
sujet <- substr(rownames(dfmeanbaseplacebo), 1, 8)
placebo <- "placebo"

#ajouter la condition, l'instant de mesure, le sujet, et froid ou normal , base ou oclu

dfmeanbaseplacebo <- cbind(dfmeanbaseplacebo, test_type, timing, sujet, placebo)
colnames(dfmeanbaseplacebo)[5] <- "condition"

#Calcule du diamètre pic

#plot oclu pour voir la selection du max

ggplot(echoplacebooclu$GaetanSteenbergenPOSTAOCLU, aes(x = temps , y = diameterinstant , na.rm = TRUE))+
  geom_point() +
  geom_line()
#selectionner diamètre pic

test <- filter(echoplacebooclu$GaetanSteenbergenPOSTAOCLU, temps > 80000 & temps < 103000)

ggplot(test, aes(x = temps , y = diameterinstant , na.rm = TRUE))+
  geom_point(color = "red") +
  geom_line()

max(test$diameterinstant)

which.max(echoplacebooclu$JulesSavignacPREAOCLU$diameterinstant)

which(echoplacebooclu$GaetanSteenbergenPOSTAOCLU$diameterinstant == 6.147)

#calcule FMD
#chargement du fichier avec les numéros de ligne

linenumbersplacebo <- read.csv("C:/Users/maxch/Nextcloud/Data_STIMCARE/ECHO/data/linenumbersplacebo.csv")
linenumbersplacebo <- column_to_rownames(linenumbersplacebo , var = "nom")

#extraction du diamètre pic en fonction de l'index de la ligne selectionné
#graphiquement puis stockage dans un dataframe

# Utilisation de lapply pour appliquer la fonction à chaque dataframe de la liste

dfpicdiamplacebo <- lapply(seq_along(echoplacebooclu), function(bob) {
  nombre <- linenumbersplacebo$linenumbers[bob]
  selection_lignes(echoplacebooclu[[bob]], nombre)
})

dfpicdiamplacebo <- do.call(rbind, dfpicdiamplacebo)

dfpicdiamplacebo <- as.data.frame(dfpicdiamplacebo[,2])
rownames(dfpicdiamplacebo) <- names(echoplacebooclu)
colnames(dfpicdiamplacebo)[1] <- "peakdiameter"

#liaison avec les diamètre moyen pour calcule du FMD

dfplacebo <- cbind(dfpicdiamplacebo, dfmeanbaseplacebo)
dfplacebo <- mutate(dfplacebo , FMD = (peakdiameter-meandiameter)/meandiameter*100)
dfplacebo <- mutate(dfplacebo , FMD_abs = peakdiameter-meandiameter)
dfplacebo <- mutate(dfplacebo, instant = paste0(test_type , timing))
dfplacebo <- dfplacebo[,-c(3,4)]

#calcule du shear rate diamètre de base

dfplacebo <-
  mutate(dfplacebo, speedbase = as.numeric(lapply(echoplacebobase , speeddiamb)))

dfplacebo <-
  mutate(dfplacebo,
         bloodflowbase = speedbase * (3.141593 / 4) * ((meandiameter / 10) * (meandiameter /
                                                                                10)))

dfplacebo <- mutate(dfplacebo, SRbase = (8*speedbase)/(meandiameter/10))

#calcule speed, shear rate pour le diamètre pic

dfplacebo <-
  mutate(dfplacebo, meanvelocity = as.numeric(lapply(echoplacebooclu, function(bob) {
    (max(bob$positivespeed)/2)
  })))

dfplacebo <-
  mutate(dfplacebo,
         bloodflowpic = meanvelocity * (3.141593 / 4) * ((peakdiameter / 10) * (peakdiameter /
                                                                                           10)))

dfplacebo<- mutate(dfplacebo, SRpic = (8*meanvelocity)/(peakdiameter/10))

select_temps <- function(df, nombre) {
  return(tempspic = (df$temps[nombre] - df$temps[1])/1000)
}

dfplacebo <-
  mutate(dfplacebo, time_to_peak = as.numeric(lapply(seq_along(echoplacebooclu), function(bob) {
    nombre <- linenumbersplacebo$linenumbers[bob]
    select_temps(echoplacebooclu[[bob]], nombre)
  })))

select_AUC <- function(df, nombre) {
  return(hypermia_AUC = (trapz(df$temps[1:nombre], df$positivespeed[1:nombre]))/1000)
}

dfplacebo <-
  mutate(dfplacebo, hyperemia_AUC = as.numeric(lapply(seq_along(echoplacebooclu), function(bob) {
    nombre <- linenumbersplacebo$linenumbers[bob]
    select_AUC(echoplacebooclu[[bob]], nombre)
  })))
