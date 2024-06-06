#==============================================================================#
# SD1 - SAE - Régression sur données réelles - SSTI203
# Auteur : GAMONDELE Maxime
# Date : 10/03/2024
# Soucre : https://ecampus.unicaen.fr/my/
#==============================================================================#

#==============================================================================#
# ---- Chargement des données ----#

readLines(con = "../Data/penguins.txt",
          n = 10)

dataset = read.table(file = "../data/penguins.txt",
                     sep="\t",
                     header = TRUE)
str(dataset)


#==============================================================================#
# ---- Préparation des données ----

names(dataset) <- c("Espèces", "Ile","Longueur_crête","Profondeur_crête",
                    "Longueur_nageoires","Poids","Sexe","Année")
within(dataset, 
       {
         Espèces <- factor(Espèces)
         Longueur_crête <- Longueur_crête/10
         Profondeur_crête <- Profondeur_crête/10
         Longueur_nageoires <- Longueur_nageoires/10
         Sexe <- factor(Sexe)
         Poids <- Poids/1000
       }) -> dataset

# Suppression des lignes contenant NA
# Filtrer le dataset pour inclure uniquement les lignes complètes dans la variable cible
data <- dataset[complete.cases(dataset[, "Poids"]), ]


# Vérification du succès du changement de type et du remplacement de la colonne
str(data)


#==============================================================================#
# ---- Analyse exploratoire sur 1 niveau ----

# -- fonction 1 --

RegLin1 = function(var_X,var_Y,nom_X,unité_de_mesure_X){
  model = lm(formula = data[[var_Y]] ~ data[[var_X]],
             data = data)
  summary(object = model)
  anova(object = model)
  
  
  # ------------- Calcul des indicateurs statistiques ------------- #
  
  # Coefficient de détermination
  R_Squared <- summary(model)$r.squared
  
  # Critère des moindres carrées
  bêta1 = summary(model)$coefficients[2, "Estimate"]
  bêta0 = summary(model)$coefficients[ 1, "Estimate"]
  
  # Cor Pearson
  cor_p = sign(bêta1)*cor(fitted(model),data[[var_Y]], method = "pearson")
  
  ## Études 
  # Cor Pearson
  print(paste("Le coefficient de corrélation linéaire de Pearson est de ",cor_p))
  # Critère des moindres carrées
  print(paste("À l'aide du critère des moindres carrées nous obtenons β₁ = ",
              round(bêta1,3),"et β₀ =",round(bêta0,3)))
  print(paste("L'augmentation de 1",unité_de_mesure_X,"de",nom_X,
              "induit une augmentation moyenne de",round(bêta1,3),"kg"))
  # Coefficient de détermination
  print(paste("La variable ",nom_X,"explique", round(R_Squared * 100, 2),
              "% de la variabilité de la variable Poids non prise en compte dans le modèle constant"))
  
  
  # ------------- Représentation graphique ------------- #
  
  within(data,{
    std.residuals = rstandard(model);
  }) -> data
  
  data = data.frame(data,
                    predict = predict(model,
                                      interval = "prediction"))
  
  with(data,
       loess(formula = data[[var_Y]] ~ data[[var_X]])) -> smooth
  
  data = data.frame(data,
                    loess = fitted(smooth))
  data[order(data[[var_X]]),] -> data

  with(dataset,
       plot(x = data[[var_X]],
            y = data[[var_Y]],
            pch = 21,
            cex = 0.8,
            col = "black",
            bg = "blue",
            main = paste("Nuage de dispersion \n ",var_Y,"vs",nom_X,sep = " "),
            ylab = paste(var_Y,"(kg)",sep = " "),
            xlab = paste(nom_X," (",unité_de_mesure_X,")",sep = ""),
            las = 1))
  
  # Courbe de la régression lissée
  with(data,
       lines(x=data[[var_X]],
             y = loess,
             col = "purple3",
             lty = 2,
             lwd = 2))
  
  # Droite des moindres carrés
  with(data,
       lines(x = data[[var_X]],
             y = predict.fit,
             col = "red2",
             lty = 1,
             lwd = 3))
  
  # Bande des valeurs prédites par le modèle de régression linéaire dans 95% des cas
  with(data,
       lines(x = data[[var_X]],
             y = predict.lwr,
             col = "orange2",
             lty = 4,
             lwd = 3))
  with(data,
       lines(x = data[[var_X]],
             y = predict.upr,
             col = "orange2",
             lty = 4,
             lwd = 3))
  
  # Affichage du text à droite ou à gauche en fonction de bêta 1
  
  if (bêta1 > 0) {
    text(min(data[[var_X]]) + max(data[[var_X]]) / 20, 6,
         paste("Équation de la droite\nm(x) =",
               round(bêta1, 2), "x + (", round(bêta0, 2), ")"),
         col = "red",
         cex = 1)
  } else {
    text(max(data[[var_X]]) - max(data[[var_X]]) / 15, 6,
         paste("Équation de la droite\nm(x) =",
               round(bêta1, 2), "x + (", round(bêta0, 2), ")"),
         col = "red",
         cex = 1)
  }
  
  if (bêta1 > 0) {
    text(min(data[[var_X]]) + max(data[[var_X]]) / 20, 5.5,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p, 2)),
         col = "red",
         cex = 1)
  } else {
    text(max(data[[var_X]]) - max(data[[var_X]]) / 15, 5.5,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p, 2)),
         col = "red",
         cex = 1)
  }
  

  # # Résidus stantdardisés (fonctionne)
  # 
  # with(data,
  #      loess(formula = std.residuals ~ predict.fit)) -> smooth
  # data = data.frame(data,
  #                   loess = fitted(smooth))
  # data[order(data$predict.fit),] -> data
  # head(x = data,
  #      n=10)
  # with(data,
  #      plot(x = predict.fit,
  #           y = std.residuals,
  #           col = "black",
  #           bg = "blue",
  #           pch = 21,
  #           cex = 0.8,
  #           las = 1,
  #           main = "Nuage de dispersion des résidus standardisé \n versus les valeurs ajustées",
  #           xlab = "Valeurs ajustées",
  #           ylab = "Résidus standardisés"))
  # abline(h = 0,
  #        col = "red2",
  #        lty =1,
  #        lwd = 3)
  # 
  # with(data,
  #      lines(x = predict.fit,
  #            y = loess.1,
  #            col = "purple3",
  #            lty = 2,
  #            lwd = 3))
  # abline(h = c(-1.96,+1.96),
  #        col = "orange2",
  #        lty = 2,
  #        lwd = 3)
  
}

#==============================================================================#
# ---- Analyse exploratoire sur 2 niveaux ----

# vérification des niveaux disponibles dans la variable sexe
levels(data$Sexe) 

# -- fonction 2 --

RegLin2 = function(var_X,var_Y,nom_X,unité_de_mesure_X){

  data <- subset(data,
                 !is.na(Sexe))
  
  data <- within(data,
                 {
                   Sexe = droplevels(Sexe)
                 }) 
  
  dataMale = subset(data,
                    Sexe == "male",
                    select = c(Poids,data$var_X,data[[var_Y]]))
  dataFemelle = subset(data,
                       Sexe == "female",
                       select = c(Poids,data$var_X,data[[var_Y]]))
  
  model_1_2 = lm(formula = dataMale[[var_Y]] ~ dataMale[[var_X]],
                 data = dataMale) 
  model_2_2 = lm(formula =  dataFemelle[[var_Y]] ~ dataFemelle[[var_X]],
                 data = dataFemelle) 

  
  # ------------- Calcul des indicateurs statistiques ------------- #
  
  # Coefficient de détermination
  R_Squared1 <- summary(model_1_2)$r.squared
  R_Squared2 <- summary(model_2_2)$r.squared
  
  ## Critère des moindres carrés
  # Mâles
  bêta1_1 = summary(model_1_2)$coefficients[2, "Estimate"]
  bêta0_1 = summary(model_1_2)$coefficients[1, "Estimate"]
  # Femelles
  bêta1_2 = summary(model_2_2)$coefficients[2, "Estimate"]
  bêta0_2 = summary(model_2_2)$coefficients[1, "Estimate"]
  
  # Cor Pearson
  cor_p1 = sign(bêta1_1)*cor(fitted(model_1_2),dataMale[[var_Y]], method = "pearson")
  cor_p2 = sign(bêta1_2)*cor(fitted(model_2_2),dataFemelle[[var_Y]], method = "pearson")
  
  ##Études
  
  print(paste( var_Y ,"vs",nom_X,"selon le sexe"))
  
  # Cor Pearson
  # Mâles
  print(paste("le coefficient de corrélation linéaire de Pearson pour les pingouins mâles est de",
              round(cor_p1,2)))
  # Femelles
  print(paste("le coefficient de corrélation linéaire de Pearson pour les pingouins femelles est de",
              round(cor_p2,2)))
  
  # Coefficient de détermination
  # Mâles
  print(paste("Chez les mâles la variable",nom_X,"explique",
              round(R_Squared1 * 100, 2), "% de la variabilité de la variable Poids non prise en compte dans le modèle constant"))
  # Femelles
  print(paste("Chez les femelles la variable",nom_X,"explique",
              round(R_Squared2 * 100, 2), "% de la variabilité de la variable Poids non prise en compte dans le modèle constant"))
  
  # Critère des moindres carrés
  # Mâles
  phrase_betaj="À l'aide du critère des moindres carrées nous obtenons β₁ = "
  print(paste(phrase_betaj,bêta1_1,"et β₀ =",bêta0_1,"chez les mâles"))
  print(paste("L'augmentation de 1",unité_de_mesure_X,"de",nom_X,
              "induit une augmentation moyenne de",round(bêta1_1,3),
              "kg chez les pinguins mâles"))
  # Femelles
  print(paste(phrase_betaj,bêta1_2,"et β₀ =",bêta0_2,"chez les femelles"))
  print(paste("L'augmentation de 1",unité_de_mesure_X,"de",nom_X,
              "induit une augmentation moyenne de",round(bêta1_2,3),
              "kg chez les pinguins femelles"))
  
  
  # ------------- Représentation graphique ------------- #

  prediction_1=data.frame(dataMale,
                          predict = predict(model_1_2))

  prediction_2=data.frame(dataFemelle,
                          predict = predict(model_2_2))

  with(data, 
       plot(x = data[[var_X]],
            y = data[[var_Y]],
            pch = 21,
            bg = ifelse(Sexe == "male","#52BAFF","#FF5CFB"),
            las = 1,
            cex = 1.2,
            main = paste("Nuage de dispersion \n ",var_Y,"vs",nom_X,sep = " "),
            ylab = paste(var_Y,"(kg)",sep = " "),
            xlab = paste(nom_X," (",unité_de_mesure_X,")",sep = "")))
  
  # Affichage de la legende à droite ou à gauche en fonction de bêta 1
  
  if (bêta1_1 > 0) {
  legend("topleft",
         pch = 21,
         legend = c("mâle","femelle"),
         pt.bg = c("#52BAFF","#FF5CFB"),
         horiz = FALSE,
         cex = 1,
         bty = "n")
  } else {
    legend("topright",
           pch = 21,
           legend = c("mâle","femelle"),
           pt.bg = c("#52BAFF","#FF5CFB"),
           horiz = FALSE,
           cex = 1,
           bty = "n")
  }
  
  # Droite des moindres carrés

  with(prediction_1,
       lines(x = dataMale[[var_X]],
             y = predict,
             col = "#52BAFF",
             lwd = 2))

  with(prediction_2,
       lines(x = dataFemelle[[var_X]],
             y = predict,
             col = "#FF5CFB",
             lwd = 2))
  
  # Affichage du text à droite ou à gauche en fonction de bêta 1
  
  if (bêta1_1 > 0) {
    text(min(data[[var_X]]) + max(data[[var_X]]) / 15, 6.2,
         paste("Équation de la droite\nm(x) =",
               round(bêta1_1, 2), "x + (", round(bêta0_1, 2), ")"),
         col = "#52BAFF",
         cex = 1)
  } else {
    text(max(data[[var_X]]) - max(data[[var_X]]) / 15, 6.2,
         paste("Équation de la droite\nm(x) =",
               round(bêta1_1, 2), "x + (", round(bêta0_1, 2), ")"),
         col = "#52BAFF",
         cex = 1)
  }
  
  if (bêta1_2 > 0) {
    text(min(data[[var_X]]) + max(data[[var_X]]) / 15, 5.9,
         paste("Équation de la droite\nm(x) =",
               round(bêta1_2, 2), "x + (", round(bêta0_2, 2), ")"),
         col = "#FF5CFB",
         cex = 1)
  } else {
    text(max(data[[var_X]]) - max(data[[var_X]]) / 15 , 5.9,
         paste("Équation de la droite\nm(x) =",
               round(bêta1_2, 2), "x + (", round(bêta0_2, 2), ")"),
         col = "#FF5CFB",
         cex = 1)
  }
  
  # PEARSON
  
  if (bêta1_1 > 0) {
    text(min(data[[var_X]]) + max(data[[var_X]]) / 7.5, 6.2,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p1, 2)),
         col = "#52BAFF",
         cex = 1)
  } else {
    text(max(data[[var_X]]) - max(data[[var_X]]) / 7.5, 6.2,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p1, 2)),
         col = "#52BAFF",
         cex = 1)
  }
  
  if (bêta1_2 > 0) {
    text(min(data[[var_X]]) + max(data[[var_X]]) / 7.5, 5.9,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p2, 2)),
         col = "#FF5CFB",
         cex = 1)
  } else {
    text(max(data[[var_X]]) - max(data[[var_X]]) / 7.5, 5.9,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p2, 2)),
         col = "#FF5CFB",
         cex = 1)
  }
  
}

#==============================================================================#
# ---- Analyse exploratoire sur 3 niveaux ----

# Vérification des niveaux disponibles dans la variable sexe
levels(data$Espèces) 

# -- fonction 3 --
RegLin3 = function(var_X,var_Y,nom_X,unité_de_mesure_X){
  data <- subset(data,
                 !is.na(Espèces))
  data <- within(data,
                 {
                   droplevels(Espèces)
                 })
  
  data_Adelie = subset(data,
                       Espèces == "Adelie",
                       select = c(Espèces, data[[var_Y]],data$var_X))
  
  data_Chinstrap = subset(data,
                          Espèces == "Chinstrap",
                          select = c(Espèces, data[[var_Y]],data$var_X))
  data_Gentoo = subset(data,
                       Espèces == "Gentoo",
                       select = c(Espèces, data[[var_Y]],data$var_X))
  
  model_1_3 = lm(formula = data_Adelie[[var_Y]] ~ data_Adelie[[var_X]],
                 data = data_Adelie) 
  
  model_2_3 = lm(formula = data_Chinstrap[[var_Y]] ~ data_Chinstrap[[var_X]],
                 data = data_Chinstrap) 
  
  model_3_3 = lm(formula = data_Gentoo[[var_Y]] ~ data_Gentoo[[var_X]],
                 data = data_Gentoo) 
  
  # ------------- Calcul des indicateurs statistiques ------------- #
  
  # Coefficient de détermination
  R_Squared1 <- summary(model_1_3)$r.squared
  R_Squared2 <- summary(model_2_3)$r.squared
  R_Squared3 <- summary(model_3_3)$r.squared
  
  ## Critère des moindres carrés
  # Adelie
  bêta1_1 = summary(model_1_3)$coefficients[2, "Estimate"]
  bêta0_1 = summary(model_1_3)$coefficients[1, "Estimate"]
  # Chinstrap
  bêta1_2 = summary(model_2_3)$coefficients[2, "Estimate"]
  bêta0_2 = summary(model_2_3)$coefficients[1, "Estimate"]
  # Gentoo
  bêta1_3 = summary(model_3_3)$coefficients[2, "Estimate"]
  bêta0_3 = summary(model_3_3)$coefficients[1, "Estimate"]
  
  # Cor Pearson  
  # Adelie
  cor_p1 = sign(bêta1_1)*cor(fitted(model_1_3),data_Adelie[[var_Y]], method = "pearson")
  # Chinstrap
  cor_p2 = sign(bêta1_2)*cor(fitted(model_2_3),data_Chinstrap[[var_Y]], method = "pearson")
  # Gentoo
  cor_p3 = sign(bêta1_3)*cor(fitted(model_3_3),data_Gentoo[[var_Y]], method = "pearson")
  
  
  ##Études
  
  print(paste( var_Y ,"vs",nom_X))
  
  phrase_coeff_ping = "le coefficient de corrélation linéaire de Pearson pour les pingouins de la race"
  
  # Cor Pearson
  phrase_coeff_ping = "le coefficient de corrélation linéaire de Pearson pour les pingouins de la race"
  # Adelie
  print(paste( phrase_coeff_ping,"Adelie est de",
               round(cor_p1,2)))
  # Chinstrap
  print(paste(phrase_coeff_ping,"Chinstrap est de",
              round(cor_p2,2)))
  # Gentoo
  print(paste(phrase_coeff_ping,"Gentoo est de",
              round(cor_p3,2)))

  # Coefficient de détermination
  phrase_coeff_det = "% de la variabilité de la variable Poids non prise en compte dans le modèle constant"
  # Adelie
  print(paste("Chez l'éspèce Adelie la variable",nom_X,"explique",
              round(R_Squared1 * 100, 2),phrase_coeff_det))
  # Chinstrap
  print(paste("Chez l'éspèce Chinstrap la variable",nom_X,"explique",
              round(R_Squared2 * 100, 2),phrase_coeff_det))
  # Gentoo
  print(paste("Chez l'éspèce Gentoo la variable",nom_X,"explique",
              round(R_Squared3 * 100, 2),phrase_coeff_det))
  
  # Critère des moindres carrés
  phrase_betaj_1 = "A l'aide des critère des moindres carrées nous obtenons β₁ = "
  phrase_betaj_2 = paste("L'augmentation de 1",unité_de_mesure_X,"de",nom_X)
  
  # Adelie
  print(paste(phrase_betaj_1,bêta1_1,"et β₀ =",bêta0_1,"chez l'éspèce Adelie"))
  print(paste(phrase_betaj_2,"induit une augmentation moyenne de",
              round(bêta1_1,3),"kg chez l'éspèce Adelie"))
  # Chinstrap
  print(paste(phrase_betaj_1,bêta1_2,"et β₀ =",bêta0_2,"chez l'éspèce Chinstrap"))
  print(paste(phrase_betaj_2,"induit une augmentation moyenne de",
              round(bêta1_2,3),"kg chez l'éspèce Chinstrap"))
  
  # Gentoo
  print(paste(phrase_betaj_1,bêta1_3,"et β₀ =",bêta0_3,"chez l'éspèce Gentoo"))
  print(paste(phrase_betaj_2,"induit une augmentation moyenne de",
              round(bêta1_3,3),"kg chez l'éspèce Gentoo"))

  
  # ------------- Représentation graphique ------------- #
  
  prediction_1 = data.frame(data_Adelie,
                            predict = predict(model_1_3))

  prediction_2 = data.frame(data_Chinstrap,
                          predict = predict(model_2_3))

  prediction_3 = data.frame(data_Gentoo,
                          predict = predict(model_3_3))

  with(data, 
       plot(x = data[[var_X]],
            y = data[[var_Y]],
            pch = 21,
            bg = ifelse(Espèces == "Adelie","#5269FF",
                        ifelse(Espèces == "Chinstrap","purple","orange")),
            las = 1,
            cex = 1.2,
            main = paste("Nuage de dispersion \n ",var_Y,"vs",nom_X,sep = " "),
            ylab = paste(var_Y,"(kg)",sep = " "),
            xlab = paste(nom_X," (",unité_de_mesure_X,")",sep = "")))

  with(prediction_1,
       lines(x = data_Adelie[[var_X]],
             y = predict,
             col = "#5269FF",
             lwd = 2))

  with(prediction_2,
       lines(x = data_Chinstrap[[var_X]],
             y = predict,
             col = "purple",
             lwd = 2))


  with(prediction_3,
       lines(x = data_Gentoo[[var_X]],
             y = predict,
             col = "orange",
             lwd = 2))
  
  # Affichage du text/legende à droite ou à gauche en fonction des droites pour
  # un affichage esthétique 
  
  if (var_X == "Profondeur_crête") {
    legend("topright",
           pch = 21,
           legend = c("Adélie","Chinstrap","Gentoo"),
           pt.bg = c("#5269FF","purple","orange"),
           horiz = FALSE,
           cex = 1,
           bty = "n")
    text(max(data[[var_X]]) - max(data[[var_X]]) / 13, 6.2,
         paste("Équation de la droite\nm(x) =",
               round(bêta1_1, 2), "x + (", round(bêta0_1, 2), ")"),
         col = "#5269FF",
         cex = 1)
    text(max(data[[var_X]]) - max(data[[var_X]]) / 13, 5.9,
         paste("Équation de la droite\nm(x) =",
               round(bêta1_2, 2), "x + (", round(bêta0_2, 2), ")"),
         col = "purple",
         cex = 1)
    text(max(data[[var_X]]) - max(data[[var_X]]) / 13, 5.6,
         paste("Équation de la droite\nm(x) =",
               round(bêta1_3, 2), "x + (", round(bêta0_3, 2), ")"),
         col = "orange",
         cex = 1)
    text(max(data[[var_X]]) - max(data[[var_X]]) / 7.2, 6.2,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p1, 2)),
         col = "#5269FF",
         cex = 1)
    text(max(data[[var_X]]) - max(data[[var_X]]) / 7.2, 5.9,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p2, 2)),
         col = "purple",
         cex = 1)
    text(max(data[[var_X]]) - max(data[[var_X]]) / 7.2, 5.6,
         paste("Coefficient de corrélation\nde Pearson =",
               round(cor_p3, 2)),
         col = "orange",
         cex = 1)
    
  } else {
    legend("topleft",
           pch = 21,
           legend = c("Adélie","Chinstrap","Gentoo"),
           pt.bg = c("#5269FF","purple","orange"),
           horiz = FALSE,
           cex = 1,
           bty = "n")
    if (bêta1_1 > 0) {
      text(min(data[[var_X]]) + max(data[[var_X]]) / 15, 6.2,
           paste("Équation de la droite\nm(x) =",
                 round(bêta1_1, 2), "x + (", round(bêta0_1, 2), ")"),
           col = "#5269FF",
           cex = 1)
    } else {
      text(max(data[[var_X]]) - max(data[[var_X]]) / 13, 6.2,
           paste("Équation de la droite\nm(x) =",
                 round(bêta1_1, 2), "x + (", round(bêta0_1, 2), ")"),
           col = "#5269FF",
           cex = 1)
    }
    
    if (bêta1_2 > 0) {
      text(min(data[[var_X]]) + max(data[[var_X]]) / 15, 5.9,
           paste("Équation de la droite\nm(x) =",
                 round(bêta1_2, 2), "x + (", round(bêta0_2, 2), ")"),
           col = "purple",
           cex = 1)
    } else {
      text(max(data[[var_X]]) - max(data[[var_X]]) / 13, 5.9,
           paste("Équation de la droite\nm(x) =",
                 round(bêta1_2, 2), "x + (", round(bêta0_2, 2), ")"),
           col = "purple",
           cex = 1)
    }
    
    if (bêta1_3 > 0) {
      text(min(data[[var_X]]) + max(data[[var_X]]) / 15, 5.6,
           paste("Équation de la droite\nm(x) =",
                 round(bêta1_3, 2), "x + (", round(bêta0_3, 2), ")"),
           col = "orange",
           cex = 1)
    } else {
      text(x = max(data[[var_X]]) - max(data[[var_X]]) / 13,
           y = 5.6,
           paste("Équation de la droite\nm(x) =",
                 round(bêta1_3, 2), "x + (", round(bêta0_3, 2), ")"),
           col = "orange",
           cex = 1)
    }
    #PEARSON
    if (bêta1_1 > 0) {
      text(min(data[[var_X]]) + max(data[[var_X]]) / 7.3, 6.2,
           paste("Coefficient de corrélation\nde Pearson =",
                 round(cor_p1, 2)),
           col = "#5269FF",
           cex = 1)
    } else {
      text(max(data[[var_X]]) - max(data[[var_X]]) / 7.3, 6.2,
           paste("Coefficient de corrélation\nde Pearson =",
                 round(cor_p1, 2)),
           col = "#5269FF",
           cex = 1)
    }
    
    if (bêta1_2 > 0) {
      text(min(data[[var_X]]) + max(data[[var_X]]) / 7.3, 5.9,
           paste("Coefficient de corrélation\nde Pearson =",
                 round(cor_p2, 2)),
           col = "purple",
           cex = 1)
    } else {
      text(max(data[[var_X]]) - max(data[[var_X]]) / 7.3, 5.9,
           paste("Coefficient de corrélation\nde Pearson =",
                 round(cor_p2, 2)),
           col = "purple",
           cex = 1)
    }
    
    if (bêta1_3 > 0) {
      text(min(data[[var_X]]) + max(data[[var_X]]) / 7.3, 5.6,
           paste("Coefficient de corrélation\nde Pearson =",
                 round(cor_p3, 2)),
           col = "orange",
           cex = 1)
    } else {
      text(x = max(data[[var_X]]) - max(data[[var_X]]) / 7.3,
           y = 5.6,
           paste("Coefficient de corrélation\nde Pearson =",
                 round(cor_p3, 2)),
           col = "orange",
           cex = 1)
    }
  }
}


# --- Appelle des fonctions ---
# étude + représentation graphique

# Fonction 1 (X, Y, Libellé de X, unité de mesure de X)

RegLin1("Longueur_nageoires","Poids","Longueur des nageoires","cm")
RegLin1("Longueur_crête","Poids","Longueur de la crête","cm")
RegLin1("Profondeur_crête","Poids", "Profondeur de la crête","cm")

# Fonction 2 (X, Y, Libellé de X, unité de mesure de X)

RegLin2("Longueur_nageoires","Poids","Longueur des nageoires","cm")
RegLin2("Longueur_crête","Poids","Longueur de la crête","cm")
RegLin2("Profondeur_crête","Poids", "Profondeur de la crête","cm")

# Fonction 3 (X, Y, Libellé de X, unité de mesure de X)

RegLin3("Longueur_nageoires","Poids","Longueur des nageoires","cm")
RegLin3("Longueur_crête","Poids","Longueur de la crête","cm")
RegLin3("Profondeur_crête","Poids", "Profondeur de la crête","cm")


