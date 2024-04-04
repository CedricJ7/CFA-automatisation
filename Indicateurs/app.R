library(shiny)
library(ggplot2)
library(bslib)
library(readxl)
library(DT)
library(tidyverse)
ui <- navbarPage(
  title = "Indicateurs clés",
  selected = "Formations vs partenaires",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "",
    fileInput("file", label = "Charger données export", accept = c(".xlsx")),
    fileInput("file2", label = "Charger données réussite examen", accept = c(".xlsx"))
  ),
  tabPanel(
    title = "Formations vs partenaires",
    DTOutput("t1")
  ),
  tabPanel(
    title = "Effectifs par formations",
    DTOutput("t2")
  ),
  tabPanel(
    title = "Répartition sexe",
    DTOutput("s")
  ),
  tabPanel(
    title = "Entreprises privés/publiques",
    DTOutput("pp")
  ),
  tabPanel(
    title = "Origine géo des entreprises",
    DTOutput("geo"),
    DTOutput("geo2")
  ),
  tabPanel(
    title = "Réussite examen",
    DTOutput("r"),
    DTOutput("r2")
  )
)

server <- function(input, output, session) {
  
  #reactive
  d <- reactive({
    req(input$file) # Vérifie si un fichier est sélectionné
    d <- read_excel(input$file$datapath, 1)
    niveaux <- c("BACHELOR" = "Niveau 6",
                 "MASTER" = "Niveau 7",
                 "BUT" = "Niveau 6",
                 "TP 3" = "Niveau 6",
                 "LP" = "Niveau 6",
                 "INGENIEUR" = "Niveau 7",
                 "DE" = "Niveau 5",
                 "DCG" = "Niveau 6")
    
    for (i in 1:nrow(d)) { 
      intitule <- as.character(d[i, 9])
      mots <- unlist(strsplit(intitule, " "))
      niveau_trouve <- NA
      
      for (val in mots) {
        if (val %in% names(niveaux)) {
          niveau_trouve <- niveaux[val]
          break
        }
      }
      
      if (!is.na(niveau_trouve)) {
        d$Niveau[i] <- niveau_trouve
      } else {
        d$Niveau[i] <- "Niveau ?"
      }
    }
    
    return(d)
  })
  d2 = reactive({
    req(input$file2)
    d2 <- read_excel(input$file2$datapath, 1)
    return(d2)
  })
  output$t1 <- renderDT({
    data <- d()
    
    # Calculer les informations
    nb_diplomes_unique <- length(unique(data$`Nom court diplôme`))
    nb_etablissements_unique <- length(unique(data$`Etablissement inscription`))
    liste_etablissements <- as.character(unique(data$`Etablissement inscription`))
    liste_etablissements_combined <- paste(liste_etablissements, collapse = ", ")
    
    # Créer un data frame avec les informations
    table_info <- data.frame(
      `Nombre unique de diplômes` = nb_diplomes_unique,
      `Nombre unique d'établissements` = nb_etablissements_unique,
      `Liste des établissements` = liste_etablissements_combined
    )
    
    # Afficher le tableau
    datatable(table_info, 
              caption = "Informations sur les diplômes et les établissements")
  })
  
  
  output$t2 <- renderDT({
    req(input$file)
    d <- read_excel(input$file$datapath,1)  
    req(input$file)
    d <- read_excel(input$file$datapath,1)  
    
    niveaux <- c("BACHELOR" = "Niveau 6",
                 "MASTER" = "Niveau 7",
                 "BUT" = "Niveau 6",
                 "TP 3" = "Niveau 6",
                 "LP" = "Niveau 6",
                 "INGENIEUR" = "Niveau 7",
                 "DE" = "Niveau 5",
                 "DCG" = "Niveau 6")
    
    for (i in 1:(nrow(d))) { 
      # Extraire l'intitulé de la ligne i
      intitule <- as.character(d[i, 9]$`Nom court diplôme`)  # Assurez-vous que l'intitulé est converti en chaîne de caractères
      
      # Séparer les mots de l'intitulé en utilisant l'espace comme séparateur
      mots <- as.list(strsplit(intitule, " ")[[1]])  # strsplit retourne une liste, donc nous prenons le premier élément
      
      # Initialiser une variable pour stocker le niveau
      niveau_trouve <- NA
      
      # Parcourir les mots et vérifier si un niveau est trouvé
      for (val in mots) {
        if (val %in% names(niveaux)) {
          niveau_trouve <- niveaux[val]
          break  # Sortir de la boucle dès qu'un niveau est trouvé
        }
      }
      
      # Si un niveau est trouvé, l'assigner à la ligne i, sinon assigner "Niveau ?"
      if (!is.na(niveau_trouve)) {
        d$Niveau[i] <- niveau_trouve
      } else {
        d$Niveau[i] <- "Niveau ?"
      }
    }
    #group_by formation et affiche les effectifs par formation
  
    
    selected_data <- d %>% group_by(`Nom court diplôme`, Niveau) %>% summarise(Effectif = n())
    
    datatable(selected_data)  # Vous pouvez régler 'pageLength' selon vos besoins
  })
  

  output$s <- renderDT({
    data <- d()
    data$Genre <- ifelse(data$`Genre candidat` == "Mme", "Femme", "Homme")
    data$Niveau <- ifelse(data$Niveau %in% c("Niveau 5","Niveau 6"), "Niveau 5-6", "Niveau 7")
    prop_sexe <- data %>% 
      group_by(Niveau, Genre) %>% 
      summarise(
        Effectif = n()
      ) 
    
    # Calculer la proportion par niveau
    prop_sexe <- prop_sexe %>% 
      group_by(Niveau) %>%
      mutate(Proportion = Effectif / sum(Effectif))
    
    total_global <- data %>% 
      group_by(Genre) %>% 
      summarise(
        Effectif = n()
      ) %>% 
      ungroup() %>%
      mutate(Niveau = "Total global") %>% 
      select(Niveau, Genre, Effectif) %>%
      mutate(Proportion = Effectif / sum(Effectif))
    
    # Concaténer les données avec les totaux globaux
    prop_sexe <- bind_rows(prop_sexe, total_global)
    
    # Afficher la table avec DT
    datatable(prop_sexe, caption = "Tableau global", options = list(paging = FALSE))
    
  })
  
  output$pp <- renderDT({
    d = d()
    d_ent = d %>% select(`Raison sociale entreprise accueil`,`Type entreprise accueil`, `Effectif entreprise`) %>% group_by(`Raison sociale entreprise accueil`) %>% summarise(n=n(), `Type entreprise` = first(`Type entreprise accueil`), `Effectif entreprise` = first(`Effectif entreprise`)) %>% arrange(desc(n))
    table(d_ent$`Type entreprise`)
    d_ent$`Type entreprise` <- ifelse(d_ent$`Type entreprise` %in% c("Fonction Publique d'état","Fonction Publique hospitalière","Fonction Publique d'état"), "Public", "Privé")
    table(d_ent$`Type entreprise`)
    prop.table(table(d_ent$`Type entreprise`))
    
    d_ent$Intervalles <- cut(d_ent$`Effectif entreprise`, 
                             breaks = c(-Inf, 10, 50, 250, 500, Inf),
                             labels = c("< 11", "de 11 à 49", "de 50 à 249", "de 250 à 499", "> 499"))
    table(d_ent$Intervalles)
    round(prop.table(table(d_ent$Intervalles)),2)
    d$`Type entreprise accueil`
    
    length(d_ent$`Type entreprise`)
    
    table(d_ent$Intervalles)
    
    ####################
    # Obtenir la longueur du vecteur 'Type entreprise'
    nombre_d_entreprises <- length(d_ent$`Type entreprise`)
    
    # Créer un tableau de fréquences pour les 'Intervalles'
    freq_intervalles <- table(d_ent$Intervalles)
    
    # Convertir le tableau de fréquences en dataframe pour les totaux
    df_freq_intervalles <- as.data.frame(freq_intervalles)
    colnames(df_freq_intervalles) <- c("Intervalles", "Nombres")
    
    # Obtenir un tableau croisé de 'Type entreprise' par 'Intervalles'
    table_entreprises <- table(d_ent$`Type entreprise`, d_ent$Intervalles)
    
    # Convertir le tableau en dataframe
    df_table <- as.data.frame.matrix(table_entreprises)
    rownames(df_table) <- unique(d_ent$`Type entreprise`)
    colnames(df_table) <- names(freq_intervalles)
    
    # Ajouter les totaux par ligne pour 'Nombres d'entreprises'
    df_table$Total <- rowSums(df_table)
    
    # Calculer les pourcentages pour chaque 'Type entreprise' et chaque 'Intervalle'
    df_table_percent <- prop.table(table_entreprises, margin = 1) * 100
    df_table_percent <- round(df_table_percent, 2)
    df_table_percent <- as.data.frame.matrix(df_table_percent)
    rownames(df_table_percent) <- paste0(unique(d_ent$`Type entreprise`), " %")
    colnames(df_table_percent) <- names(freq_intervalles)
    
    # Ajouter les totaux de pourcentage par ligne (doit être 100 % pour chaque 'Type entreprise')
    df_table_percent$Total <- rep(100, nrow(df_table_percent))
    
    # Combiner les données numériques et les pourcentages dans un seul dataframe
    df_final <- rbind(df_table, df_table_percent)
    
    # Afficher le dataframe final
    datatable(df_final)
  })
  
  output$geo = renderDT({
    d <- d()
    codes_postaux <- na.omit(d$`CP commune entreprise accueil`)
    codes_postaux <- codes_postaux[codes_postaux != ""]
    
    # Définir les préfixes des codes postaux pour Auvergne et Rhône-Alpes
    prefixes_auvergne <- c("03", "15", "43", "63")
    prefixes_rhone_alpes <- c("01", "07", "26", "38", "42", "69", "73", "74")
    
    # Extraire les préfixes et classifier les codes postaux
    prefixes <- substr(codes_postaux, 1, 2)
    regions <- ifelse(prefixes %in% prefixes_auvergne, "Auvergne", 
                      ifelse(prefixes %in% prefixes_rhone_alpes, "Rhone-Alpes", "Autres"))
    
    # Créer un tableau de données avec les codes postaux et leur région respective
    data <- data.frame(CodePostal = prefixes, Region = regions)
    
    # Calculer les effectifs et les pourcentages par code postal pour Auvergne et Rhône-Alpes
    summary <- data %>%
      filter(Region %in% c("Auvergne", "Rhone-Alpes")) %>%
      group_by(Region, CodePostal) %>%
      summarise(Effectif = n(), .groups = 'drop') %>%
      mutate(Taux = (Effectif / sum(Effectif)) * 100)
    
    # Calculer les totaux et les pourcentages par région
    totals <- summary %>%
      group_by(Region) %>%
      summarise(TotalEffectif = sum(Effectif), TotalTaux = sum(Taux), .groups = 'drop')
    
    # Ajouter les totaux de la catégorie "Autres"
    autres_effectif <- nrow(data[data$Region == "Autres",])
    total_effectif <- nrow(data)
    autres_taux <- (autres_effectif / total_effectif) * 100
    totals <- rbind(totals, c("Autres", autres_effectif, autres_taux))
    
    # Préparer le tableau final avec les effectifs et taux par code postal et par région
    final_table <- list(
      Details = summary,
      Totals = totals
    )
    # datatable(final_table)
    # # Afficher le tableau final
    # print(final_table$Details)
    datatable(final_table$Totals)
  })
  output$geo2 = renderDT({
    d <- d()
    codes_postaux <- na.omit(d$`CP commune entreprise accueil`)
    codes_postaux <- codes_postaux[codes_postaux != ""]
    
    # Définir les préfixes des codes postaux pour Auvergne et Rhône-Alpes
    prefixes_auvergne <- c("03", "15", "43", "63")
    prefixes_rhone_alpes <- c("01", "07", "26", "38", "42", "69", "73", "74")
    
    # Extraire les préfixes et classifier les codes postaux
    prefixes <- substr(codes_postaux, 1, 2)
    regions <- ifelse(prefixes %in% prefixes_auvergne, "Auvergne", 
                      ifelse(prefixes %in% prefixes_rhone_alpes, "Rhone-Alpes", "Autres"))
    
    # Créer un tableau de données avec les codes postaux et leur région respective
    data <- data.frame(CodePostal = prefixes, Region = regions)
    
    # Calculer les effectifs et les pourcentages par code postal pour Auvergne et Rhône-Alpes
    summary <- data %>%
      filter(Region %in% c("Auvergne", "Rhone-Alpes")) %>%
      group_by(Region, CodePostal) %>%
      summarise(Effectif = n(), .groups = 'drop') %>%
      mutate(Taux = (Effectif / sum(Effectif)) * 100)
    
    # Calculer les totaux et les pourcentages par région
    totals <- summary %>%
      group_by(Region) %>%
      summarise(TotalEffectif = sum(Effectif), TotalTaux = sum(Taux), .groups = 'drop')
    
    # Ajouter les totaux de la catégorie "Autres"
    autres_effectif <- nrow(data[data$Region == "Autres",])
    total_effectif <- nrow(data)
    autres_taux <- (autres_effectif / total_effectif) * 100
    totals <- rbind(totals, c("Autres", autres_effectif, autres_taux))
    
    # Préparer le tableau final avec les effectifs et taux par code postal et par région
    final_table <- list(
      Details = summary,
      Totals = totals
    )
    # datatable(final_table)
    # # Afficher le tableau final
    datatable(final_table$Details)
   
  })
  output$r = renderDT({
    d2 = d2()
    df <- data.frame(Formation = d2$Formation,
                     Oui = d2$Oui...17,
                     Total = d2$Total,
                     Taux_de_réussite_examens = d2$`Taux de réussite examens`)
    
    # Ajout d'une ligne pour la somme des Oui et Total, et la moyenne des Taux de réussite examens
    sum_row <- c("Somme", sum(df$Oui), sum(df$Total), "")
    mean_row <- c("Moyenne", "", "", mean(as.numeric(strsplit(df$Taux_de_réussite_examens,"%")),na.rm = TRUE))
    df <- rbind(df, sum_row, mean_row) 
    datatable(df)
    
  })
}

shinyApp(ui, server)
#colnames(read_excel("C:/Users/cedri/Desktop/Stage/export_de_gestion_311223.xlsx"))
