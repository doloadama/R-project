# Vérifier si la colonne est vide
verifier_colonne_vide <- function(colonne) {
  # La colonne ne doit pas être vide
  return(!all(is.na(colonne) | colonne == ""))
}

# Fonction pour vérifier la validité d'une date
est_date_valide <- function(date_str) {
  # Si la colonne est vide, retourner FALSE
  if (is.na(date_str) || length(date_str) == 0) {
    return(FALSE)
  }

  # Liste des formats de date couramment utilisés
  date_formats <- c("%d/%m/%y", "%d-%m-%y", "%m/%d/%y", "%m|%d|%y", "%m/%d/%Y", "%m-%d-%Y",
                    "%d %m %Y", "%d %B %Y", "%d,%m,%Y", "%d:%m:%Y", "%d|%m|%Y", "%d,%B,%y",
                    "%d/%B/%y", "%d.%B.%y", "%d|%B|%y", "%d-%B-%y", "%d:%B:%y", "%d,%B,%Y",
                    "%d/%B/%Y", "%d.%B.%Y", "%d|%B|%Y", "%d-%B-%Y", "%d:%B:%Y", "%d_%m_%Y",
                    "%d_%m_%y")

  # Essayer de convertir la date avec chaque format
  for (format_str in date_formats) {
    tryCatch({
      # Tenter de convertir la chaîne de caractères en objet Date
      as.Date(date_str, format = format_str)
      return(TRUE)  # La conversion a réussi, la date est valide
    }, error = function(e) {
      # Si une erreur se produit, passer au format suivant
      next
    })
  }

  # Si aucune conversion n'a réussi, retourner FALSE
  return(FALSE)
}

# Fonction pour formater une date dans un nouveau format
format_date <- function(date_str) {
  # Liste des formats de date à essayer
  date_formats <- c('%d/%m/%y', '%d-%m-%y', '%m/%d/%y', '%m|%d|%y', '%m/%d/%Y', '%m-%d-%Y',
                    '%d %m %Y', '%d %B %Y', '%d,%m,%Y', '%d:%m:%Y', '%d|%m|%Y', '%d,%B,%y',
                    '%d/%B/%y', '%d.%B.%y', '%d|%B|%y', '%d-%B-%y', '%d:%B:%y', '%d,%B,%Y',
                    '%d/%B/%Y', '%d.%B.%Y', '%d|%B|%Y', '%d-%B-%Y', '%d:%B:%Y', '%d_%m_%Y',
                    '%d_%m_%y')

  # Essayer de convertir la date avec chaque format
  for (fmt in date_formats) {
    tryCatch({
      # Tenter de convertir la chaîne de caractères en objet Date
      date_obj <- as.Date(date_str, format = fmt)
      # Si la conversion réussit, formater la date dans le format spécifié
      formatted_date <- format(date_obj, "%d/%m/%Y")
      return(formatted_date)
    }, error = function(e) {
      # Si une erreur se produit, passer au format suivant
      next
    })
  }

  # Si aucun format ne fonctionne, retourner NULL ou une valeur par défaut
  return(NULL)  # ou lancer une erreur
}

# Vérifier le numéro
verifier_numero <- function(numero) {
  # Le numéro doit être composé de lettres majuscules et de chiffres et sa taille doit être 7
  return(grepl("^[A-Z0-9]{7}$", numero))
}

# Vérifier le prénom
verifier_prenom <- function(prenom) {
  # Le prénom doit commencer par une lettre et contenir au moins 3 lettres
  return(grepl("^[A-Za-z]{3,}$", prenom))
}

# Vérifier le nom
verifier_nom <- function(nom) {
  # Le nom doit commencer par une lettre et contenir au moins 2 lettres
  return(grepl("^[A-Za-z]{2,}$", nom))
}

valide_classe <- function(classe) {
  # Fonction qui vérifie la validité de la classe.
  # La classe doit commencer par '6', '5', '4' ou '3' et se terminer par 'A', 'B', 'C' ou 'D'.

  # Args:
  #   classe (character): La classe à vérifier.

  # Returns:
  #   logical: TRUE si la classe est valide, FALSE sinon.

  classe <- gsub(" ", "", classe)
  debut <- c('6', '5', '4', '3')
  fin <- c('A', 'B', 'C', 'D', 'a', 'b', 'c', 'd')

  if (is.na(classe) || classe == "" || !(substr(classe, 1, 1) %in% debut) || !(substr(classe, nchar(classe), nchar(classe)) %in% fin)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}



format_classe <- function(classe) {
  # Fonction qui formate la classe.
  # La classe doit être sous la forme '6emA', '5emB', '4emC' ou '3emD'.

  # Args:
  #   classe (character): La classe à formater.

  # Returns:
  #   character: La classe formatée.

  classe <- gsub(" ", "", classe)
  debut <- substr(classe, 1, 1)
  fin <- substr(classe, nchar(classe), nchar(classe))
  classe <- paste0(debut, "em", fin)

  return(classe)
}


traiter_donnees <- function(filename) {
  # Lire le fichier CSV
  data <- read.csv(filename, sep = ",", header = TRUE)

  # Initialiser les dataframes pour stocker les données valides et invalides
  valide <- data.frame()
  invalide <- data.frame()

  # Ajouter une colonne pour la raison de l'invalidité
  data$Raison <- NA

  # Parcourir chaque ligne du fichier CSV
  for (i in 1:nrow(data)) {
    ligne <- data[i, ]

    # Vérifier la validité de la ligne
    if (verifier_numero(ligne$Numero) && verifier_nom(ligne$Nom) && verifier_prenom(ligne$Prénom) && est_date_valide(ligne$Date.de.naissance) && valide_classe(ligne$Classe)) {
      # Formater la date correctement
      ligne$Date.de.naissance <- format_date(ligne$Date.de.naissance)

      # Formater la classe correctement
      ligne$Classe <- format_classe(ligne$Classe)

      # Ajouter la ligne aux données valides
      valide <- rbind(valide, ligne)
    } else {
      # Déterminer la raison de l'invalidité
      if (!verifier_numero(ligne$Numero)) {
        ligne$Raison <- "Numéro invalide"
      } else if (!verifier_nom(ligne$Nom)) {
        ligne$Raison <- "Nom invalide"
      } else if (!verifier_prenom(ligne$Prénom)) {
        ligne$Raison <- "Prénom invalide"
      } else if (!est_date_valide(ligne$Date.de.naissance)) {
        ligne$Raison <- "Date de naissance invalide"
      } else if (!valide_classe(ligne$Classe)) {
        ligne$Raison <- "Classe invalide"
      }

      # Ajouter la ligne aux données invalides
      invalide <- rbind(invalide, ligne)
    }
  }

  # Écrire les données valides et invalides dans des fichiers CSV
  write.csv(valide, file = "donnees_valides.csv", row.names = FALSE)
  write.csv(invalide, file = "donnees_invalides.csv", row.names = FALSE)
}





filename <- "data.csv"
traiter_donnees(filename)

