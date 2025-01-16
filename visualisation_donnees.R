plot_moyennes_par_annee <- function(data) {
    data_ensemble <- subset(data, Caractéristiques == "Ensemble")
    
    moyennes_par_annee <- data_ensemble %>%
        group_by(Année, Discipline) %>%
        summarise(Moyenne_Score = mean(Score.moyen, na.rm = TRUE)) %>%
        ungroup()

    moyennes_par_annee$Couleur <- ifelse(moyennes_par_annee$Discipline == "Mathématiques", "Mathématiques (vert)", "Français (bleu)")

    ggplot(moyennes_par_annee, aes(x = factor(Année), y = Moyenne_Score, fill = Couleur)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Mathématiques (vert)" = "green", "Français (bleu)" = "blue")) +
        scale_y_continuous(
            limits = c(0, 320),  # Définir la plage de l'axe des y
            breaks = seq(0, 320, by = 20)  # Ajouter plus de graduations (tous les 20)
        ) +
        labs(
            title = "Moyenne des scores par année (Mathématiques vs Français)",
            x = "Année",
            y = "Score moyen"
        ) +
        theme_minimal()
}


plot_maths_par_genre <- function(data) {
    library(dplyr)
    library(ggplot2)

    data_maths <- subset(data, Discipline == "Mathématiques")

    moyennes_par_genre <- data_maths %>%
        group_by(Année, Caractéristiques) %>%
        summarise(Moyenne_Score = mean(Score.moyen, na.rm = TRUE)) %>%
        ungroup()
    

    moyennes_par_genre <- subset(moyennes_par_genre, Caractéristiques %in% c("Garçon", "Fille"))

    moyennes_par_genre$Couleur <- ifelse(moyennes_par_genre$Caractéristiques == "Fille", "Fille (orange)", "Garçon (rose)")

    ggplot(moyennes_par_genre, aes(x = factor(Année), y = Moyenne_Score, fill = Couleur)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Fille (orange)" = "orange", "Garçon (rose)" = "pink")) +
        scale_y_continuous(
            limits = c(0, 320),  # Définir la plage de l'axe des y
            breaks = seq(0, 320, by = 20)  # Ajouter plus de graduations (tous les 20)
        ) +
        labs(
            x = "Année",
            y = "Score moyen"
        ) +
        theme_minimal()
}


plot_maths_ips_par_annee_couleur <- function(data) {
    library(dplyr)
    library(ggplot2)
    
    # Filtrer les données pour "Mathématiques" et "Ensemble"
    data_maths_ensemble <- subset(data, Discipline == "Mathématiques" & Caractéristiques == "Ensemble")
    
    # Ajouter une colonne 'Plage' pour définir la plage d'années associée à chaque point
    data_maths_ensemble$Plage <- case_when(
        data_maths_ensemble$Année %in% c(2017, 2018, 2019) ~ "2017-2019",
        data_maths_ensemble$Année %in% c(2020, 2021) ~ "2020-2021",
        data_maths_ensemble$Année %in% c(2022, 2023) ~ "2022-2023",
        TRUE ~ "Autres"  # Optionnel, au cas où d'autres années sont présentes
    )
    
    # Créer le nuage de points avec IPS en abscisse et score moyen en ordonnée
    ggplot(data_maths_ensemble, aes(x = IPS.moyen, y = Score.moyen, color = Plage)) +
        geom_point(size = 1) +  # Taille des points réduite à 1
        scale_color_manual(
            values = c("2017-2019" = "blue", "2020-2021" = "red", "2022-2023" = "green"),
            labels = c("2017-2019", "2020-2021", "2022-2023")  # Noms des plages dans la légende
        ) + 
        labs(
            x = "IPS moyen",
            y = "Score moyen en Mathématiques",
            color = "Plage d'années"  # Titre de la légende
        ) +
        theme_minimal()
}

plot_groupe6_maths_2017 <- function(data) {
    library(dplyr)
    library(ggplot2)
    
    # Filtrer les données pour "Mathématiques", "Ensemble" et l'année 2017
    data_2017 <- data %>%
        filter(Discipline == "Mathématiques", 
               Caractéristiques == "Ensemble", 
               Année == 2017)
    
    # Créer le nuage de points avec une régression linéaire
    ggplot(data_2017, aes(x = IPS.moyen, y = groupe.6)) +
        geom_point(size = 2, color = "blue") +  # Points bleus pour le groupe 6
        geom_smooth(method = "lm", se = TRUE, color = "red") +  # Ligne de régression rouge continue
        labs(
            x = "IPS moyen du département",
            y = "Proportion d'élèves dans le groupe 6 (%)"
        ) +
        theme_minimal()
}

plot_groupe6_maths_toutes_annees <- function(data) {
    library(dplyr)
    library(ggplot2)
    
    # Filtrer les données pour "Mathématiques" et "Ensemble"
    data_maths_ensemble <- data %>%
        filter(Discipline == "Mathématiques", Caractéristiques == "Ensemble")
    
    # Ajouter une colonne 'Couleur' pour définir la couleur des points en fonction de l'année
    data_maths_ensemble$Couleur <- case_when(
        data_maths_ensemble$Année %in% c(2017, 2018, 2019) ~ "2017-2019",    # 2017-2019 en bleu
        data_maths_ensemble$Année %in% c(2020, 2021) ~ "2020-2021",           # 2020-2021 en rouge
        data_maths_ensemble$Année %in% c(2022, 2023) ~ "2022-2023",         # 2022-2023 en vert
        TRUE ~ "black"  # Optionnel pour d'autres années
    )
    
    # Créer le nuage de points
    ggplot(data_maths_ensemble, aes(x = IPS.moyen, y = groupe.6, color = Couleur)) +
        geom_point(size = 1) +  # Points plus petits (taille 1)
        scale_color_manual(
            values = c("2017-2019" = "blue", "2020-2021" = "red", "2022-2023" = "green"),
            labels = c("2017-2019", "2020-2021", "2022-2023")  # Associer les années aux couleurs dans la légende
        ) +
        labs(
            x = "IPS moyen du département",
            y = "Proportion d'élèves dans le groupe 6 (%)",
            color = "Années"
        ) +
        theme_minimal()
}


plot_groupe1_maths_toutes_annees <- function(data) {
    library(dplyr)
    library(ggplot2)
    
    # Filtrer les données pour "Mathématiques" et "Ensemble"
    data_maths_ensemble <- data %>%
        filter(Discipline == "Mathématiques", Caractéristiques == "Ensemble")
    
    # Ajouter une colonne 'Couleur' pour définir la couleur des points en fonction de l'année
    data_maths_ensemble$Couleur <- case_when(
        data_maths_ensemble$Année %in% c(2017, 2018, 2019) ~ "2017-2019",    # 2017-2019 en bleu
        data_maths_ensemble$Année %in% c(2020, 2021) ~ "2020-2021",           # 2020-2021 en rouge
        data_maths_ensemble$Année %in% c(2022, 2023) ~ "2022-2023",         # 2022-2023 en vert
        TRUE ~ "black"  # Optionnel pour d'autres années
    )
    
    # Créer le nuage de points
    ggplot(data_maths_ensemble, aes(x = IPS.moyen, y = groupe.1, color = Couleur)) +
        geom_point(size = 1) +  # Points plus petits (taille 1)
        scale_color_manual(
            values = c("2017-2019" = "blue", "2020-2021" = "red", "2022-2023" = "green"),
            labels = c("2017-2019", "2020-2021", "2022-2023")  # Associer les années aux couleurs dans la légende
        ) +
        labs(
            x = "IPS moyen du département",
            y = "Proportion d'élèves dans le groupe 1 (%)",
            color = "Années"
        ) +
        theme_minimal()
}
