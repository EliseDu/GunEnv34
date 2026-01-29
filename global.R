library(readODS)
library(readxl)
library(tidyverse)
library(timevis)
library(janitor)
library(glue)
library(htmltools)
library(lubridate)
library(shinyjs)
options(shiny.reactlog = TRUE)
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")
options(encoding = "UTF-8")

###############FONCTIONS########################################
source(file = "functions/utils.R", encoding = "UTF-8")
source(file = "module/ODT_export.R", encoding = "UTF-8")

###############DONNEES##########################################
# Lister les fichiers du répertoire
APP_DIR <- normalizePath(getwd())
#si Guillaume
#data <- "data"
#system("gio mount -u smb://10.34.9.19/dossiers/")
#Florian
# Lire le mot de passe depuis le fichier
#message(system("cat /home/adl/motdepasse.txt"))
system("gio mount smb://10.34.9.19/dossiers/ < motdepasse.txt")
#system("gio mount smb://10.34.9.19/dossiers/ < /srv/gunenv34/motdepasse.txt")

#pour vérifier où gvfs a positionné ses points de montage
#mountGvfs <- system("mount | grep gvfs")
#message(paste("mountGvfs",mountGvfs ))
# #pour vérifier si le point de montage est fonctionnel
#system("gio mount -li")
#system("echo$USER")
# penser à démonter à la fin du script
data <- "/run/user/103/gvfs/smb-share:server=10.34.9.19,share=dossiers/2-Dossiers_transversaux/01-Connaissance-Territoire/1-Etudes/2-Etudes_en_cours/2025_GUNenv_SERN/data/"
# message(paste("data",data ))

#
#
#
#
#
#
#
# #si DDTM
# #data <-"Y:/2-Dossiers_transversaux/01-Connaissance-Territoire/1-Etudes/2-Etudes_en_cours/2025_GUNenv_SERN/data"
fichiers <- list.files(data, full.names = TRUE)
#files <- system("smbclient //10.34.9.19/dossiers -U ATE.AT.INT/DULACEL -c \"cd 2-Dossiers_transversaux/01-Connaissance-Territoire/1-Etudes/2-Etudes_en_cours/2025_GUNenv_SERN/data; ls\"", intern = TRUE)
# Sélectionner celui qui commence par 7 chiffres puis Auto_environnementales_OCC.ods
fichier_ods <- fichiers[
  grepl("^[0-9]{8}_Auto_environnementales_OCC\\.ods$", basename(fichiers))
]

# Sécurité : si plusieurs → prendre le plus récent
fichier_ods <- fichier_ods[which.max(file.info(fichier_ods)$mtime)]

tictoc::tic("read_ods OCC")
gunenv <- read_ods(fichier_ods, sheet = 1) %>%
  clean_names()
tictoc::toc()

#sort(names(gunenv))
dates <- c("depot_du_dossier", "date", "date_conclusion","derniere_demande_de_complement", "dernier_complement_de_dossier_recu",
           "date_demande_avis_ae", "date_avis_ae", "date_de_proposition_de_mise_a_la_consultation_du_public",
           "date_debut_consultation_public", "date_fin_consultation_public", "date_reunion_coderst_cndps")

gunenv <- gunenv %>%
  mutate(
    across(
      all_of(dates),
      ~ dmy(.x)
      )
    )
#gunenv %>%
#  summarise(across(all_of(dates), ~ sum(is.na(safe_dmy(.x)) & !is.na(.x))))
# info <- tibble(
#   code = union(projet$code_aiot, gunenv$code_aiot),
#   dans_projet = if_else(code %in% projet$code_aiot, 1,0),
#   dans_gunenv = if_else(code %in% gunenv$code_aiot, 1,0)
# )
fichiers <- fichiers[
  grepl("\\([0-9]{10}\\)", basename(fichiers))
]

tictoc::tic("read_ods import_aiot")
projet <- import_aiot(fichiers) %>%
  clean_names()
tictoc::toc()

tictoc::tic("projetTime")
projetTime <- projet %>% filter(!is.na(nom_du_destinataire)) %>% mutate(
  content = nom_du_destinataire,
  start = dmy(date_de_la_correspondance),
  end = dmy(date_de_reponse),
  group = type_de_correspondance,
  duree_jours = ifelse(
    !is.na(end),
    as.integer(end - start),
    NA
  ),
  title = ifelse(
    !is.na(duree_jours),
    paste0(content, "<br>Durée : ", duree_jours, " jours", "<br>Début : ", start, "<br>Fin : ", end),
    paste0(content, "<br>Évènement ponctuel<br>Date : ", start)
  )
) %>% select(code_aiot, content, start, end, group, duree_jours, title, avis) %>%
  filter(!group %in% c("Actualisation de l’échéance de réponse à saisine/sollicitation",
                       "Transmission de pièces de procédure aux agents",
                       "Transmission de pièces de procédure au contributeur",
                       "Transmission de pièces de procédure au gestionnaire",
                       "Transmission de pièces de procédure à l'organisme"
                       )) %>%
  mutate(agent_procedure = gunenv$agent_procedure[match(code_aiot, gunenv$code_aiot)])
# gestion de la phase amont
# Vérifier s'il existe des dossiers AMONT
if (any(is.na(projet$nom_du_destinataire))) {
  projetTime_amont <- projet %>% filter(is.na(nom_du_destinataire)) %>% mutate(
  content = if_else(is.na(gunenv$aiot[match(code_aiot, gunenv$code_aiot)]),code_aiot, gunenv$aiot[match(code_aiot, gunenv$code_aiot)]),
  start = dmy(date_du_document),
  end = NA,
  group = nom_de_letape,
  duree_jours = ifelse(
    !is.na(end),
    as.integer(end - start),
    NA
  ),
  title =
    paste0("Sollicitation de l'exploitant", "<br>Évènement ponctuel<br>Date : ", start),
  avis = NA
) %>% select(code_aiot, content, start, end, group, duree_jours, title, avis) %>%
  filter(!group %in% c("Réunion échanges amonts")) %>% unique() %>%
  mutate(agent_procedure = gunenv$agent_procedure[match(code_aiot, gunenv$code_aiot)])


projetTime <- rbind(projetTime, projetTime_amont) 
}

projetTime <- projetTime %>%
  mutate(
    # --- STYLE PAR DÉFAUT : POINT vs RANGE ---
    style = ifelse(
      !is.na(end) | grepl("^(Information|Transmission|Proposition)", group),
      "background-color:#6ACAFC; color:black;",   # POINT
      "background-color:#ea4258; color:black;"    # RANGE
    ),

    # --- STYLE SPÉCIFIQUE : DEMANDE D'AVIS À UN ORGANISME ---
    style = case_when(
      grepl("^Demande d'avis à un organisme", group) & avis == "F"  ~ "background-color:#289C07; color:white;",
      grepl("^Demande d'avis à un organisme", group) & avis == "FR" ~ "background-color:#DE811D; color:black;",
      grepl("^Demande d'avis à un organisme", group) & avis == "D"  ~ "background-color:#DB2309; color:white;",
      grepl("^Demande d'avis à un organisme", group) & is.na(avis)  ~ "background-color:#969696; color:black;",
      TRUE ~ style   # conserver le style déjà calculé
    ),
    modalite = gunenv$modalite[match(code_aiot, gunenv$code_aiot)]
  )
tictoc::toc()

tictoc::tic("StructEnsemble")
StructEnsemble <- build_struct_all(gunenv) %>%
  filter(!is.na(start)) %>%
  mutate(
    duree_jours = ifelse(
      !is.na(end),
      as.integer(end - start),
      NA
    ),
    title = ifelse(
      !is.na(duree_jours),
      paste0(content, "<br>Durée : ", duree_jours, " jours", "<br>Début : ", start, "<br>Fin : ", end),
      paste0(content, "<br>Évènement ponctuel<br>Date : ", start)
    ),
    style = ifelse(content == "Dépôt du dossier","background-color:#CD9C2A; color:black;",
      ifelse(
      is.na(end),
      "background-color:#9C2ACD; color:white;",   # POINT : violet
      "background-color:#BA64DF; color:black;"     # RANGE : rose
    )),
    avis = NA
  ) %>%  select(code_aiot, content, start, end, group, duree_jours, title, style, agent_procedure, modalite, avis)
# garder les suspensions
df <- projetTime %>% filter(group %in% c("Demande de compléments", "Demande de compléments (recevabilité du dossier)"))

today <- as.Date(Sys.time(), tz = "Europe/Paris")

df <- df %>%
  arrange(code_aiot, start) %>%
  group_by(code_aiot) %>%
  mutate(
    # récupérer le start de la ligne suivante
    next_start = lead(start),

    # si end est NA → remplacer
    end = case_when(
      !is.na(end) ~ end,                      # garder end existant
      is.na(end) & !is.na(next_start) ~ next_start,  # end = start suivant
      TRUE ~ today                             # sinon end = date du jour
    ),
    group = gunenv$procedure[match(code_aiot, gunenv$code_aiot)],
    content = "Demande de compléments",
    duree_jours = as.integer(end - start),
    style = "background-color:#ea4258; color:black;"
    ) %>%  select(code_aiot, content, start, end, group, duree_jours, title, style, agent_procedure, modalite, avis)

StructEnsemble_comp <- rbind(StructEnsemble, df)

codes_aiot <- unique(union(
  projetTime$code_aiot,
  StructEnsemble$code_aiot
))
#sort(codes_aiot)
phases_instruction_daenv <- bind_rows(
  lapply(codes_aiot, build_phases_aiot)
) %>%  filter(!is.na(start))%>%
  mutate(
    end = if_else(is.na(end), Sys.Date(), end),
    duree_jours = as.integer(end - start),
    title = ifelse(
      !is.na(duree_jours),
      paste0(content, "<br>Durée : ", duree_jours, " jours", "<br>Début : ", start, "<br>Fin : ", end),
      paste0(content, "<br>Évènement ponctuel<br>Date : ", start)
    ),
    style = case_when(content == "Phase amont" ~ "background-color:#f0768b; color:white;",   # POINT
                      content == "Complétude et régularité"~  "background-color:#add8e6; color:black;",
                      content == "Examen et consultations"~ "background-color:#0066cc; color:white;",
                      content == "Décision"~ "background-color:#006400; color:white;"),
    agent_procedure = gunenv$agent_procedure[match(code_aiot, gunenv$code_aiot)]
    )

phases_instruction_daenv_projet <- phases_instruction_daenv %>% mutate(group = group_projet) %>% select(!c(group_ensemble,group_projet)) %>%
  select(code_aiot, content, start, end, group, duree_jours, title, style, agent_procedure, modalite)
phases_instruction_daenv_ensemble <- phases_instruction_daenv %>% mutate(group = group_ensemble) %>% select(!c(group_ensemble,group_projet))%>%
  select(code_aiot, content, start, end, group, duree_jours, title, style, agent_procedure, modalite)

commentaire_brut <- read_ods(file.path(data, "Tableau_Info_Supp.ods"), sheet = 1, range = "C5:I100") %>%
  mutate(start = dmy(start),
    end = if_else(end =="01/01/00", today, dmy(end)),
    duree_jours = as.integer(end - start),
    title = ifelse(
     !is.na(duree_jours),
     paste0(content, "<br>Durée : ", duree_jours, " jours", "<br>Début : ", start, "<br>Fin : ", end),
     paste0(content, "<br>Évènement ponctuel<br>Date : ", start)),
    style = glue("background-color:{color}; color:black;"),
    agent_procedure = gunenv$agent_procedure[match(code_aiot, gunenv$code_aiot)],
    modalite = gunenv$modalite[match(code_aiot, gunenv$code_aiot)],
    avis = projet$avis[match(code_aiot, projet$code_aiot)])
commentaires <- commentaire_brut %>% select(code_aiot, content, start, end, group, duree_jours, title, style, agent_procedure, modalite, avis)
comment_sup <- commentaire_brut %>% filter(tps_suspension == 1) %>% select(code_aiot, content, start, end, group, duree_jours, title, style, agent_procedure, modalite, avis)


projetTime_data <- bind_rows(phases_instruction_daenv_projet,projetTime) %>%
  filter(!is.na(content), !is.na(start), !is.na(group)) %>%
  mutate(procedure = gunenv$procedure[match(code_aiot, gunenv$code_aiot)]) %>%
  arrange(procedure, start) %>%
  mutate(id = row_number())%>%
  select(id, content, start, end, group, everything())

StructEnsemble_data <- bind_rows(phases_instruction_daenv_ensemble, StructEnsemble_comp, commentaires) %>%
  filter(!is.na(content), !is.na(start), !is.na(group)) %>%
  arrange(group, start) %>%
  mutate(id = row_number())%>%
  select(id, content, start, end, group, everything())
tictoc::toc()

tictoc::tic("autres tables")
table_suspensions <-  bind_rows(projetTime_data, comment_sup) %>%
  group_by(code_aiot) %>%
  group_split() %>%
  map_df(function(df_aiot) {
    # 1) Code AIOT
    code <- unique(df_aiot$code_aiot)
    nom <- gunenv$procedure[match(code, gunenv$code_aiot)] %>% unique()
    # 2) Total suspensions
    total_suspension <- calcul_suspension_totale(df_aiot)
    # 3) Total instruction
    total_instruction <- df_aiot %>%
      filter(group == "Phases d'instruction") %>%
      summarise(total = sum(duree_jours, na.rm = TRUE)) %>%
      pull(total)
    # 4) Correction finale
    instruction_hs <- total_instruction - total_suspension
    # 5) Retour
    tibble(
      code_aiot = code,
      nom_aiot = nom,
      total_suspension = total_suspension,
      total_instruction = total_instruction,
      instruction_hs = instruction_hs
    )
  })


resume_moyennes <- table_suspensions %>%
  summarise(
    temps_moyen_suspension   = mean(total_suspension, na.rm = TRUE),
    temps_moyen_instruction  = mean(total_instruction, na.rm = TRUE)
  )

nb_dossiers_en_cours <- StructEnsemble_data %>%
  filter(content == "Dépôt du dossier", is.na(end)) %>%
  pull(code_aiot) %>%
  n_distinct()

procedures_embarquees <- read_ods(file.path(data, "Tableau_Info_Supp.ods"), sheet = 2, range = "B5:C100") %>%
  group_by(code_aiot) %>%
  summarise(
    procedure = paste(procedure, collapse = ", "),
    .groups = "drop"
  )

procedures_embarquees_list <- read_ods(file.path(data, "Tableau_Info_Supp.ods"), sheet = 2, range = "G4:H25") %>%
  left_join(read_ods(file.path(data, "Tableau_Info_Supp.ods"), sheet = 2, range = "B5:C100"), by = "procedure") %>%
  select(-procedure)

eval_environnementale <- read_ods(file.path(data, "Tableau_Info_Supp.ods"), sheet = 3, range = "B1:C100")%>%
  clean_names()

thematiques_services_org <- projetTime_data %>%
  filter(group %in% c("Demande de contribution aux services contributeurs (phase de recevabilité)","Demande de contribution", "Demande d'avis à un organisme")) %>%
  select(content) %>% rename(service_contributeur = content) %>% unique() %>% arrange(service_contributeur) %>%
  mutate(
    thematique = case_when(
      # Débuts de chaîne
      str_detect(service_contributeur, "^ARS") ~ "Aspects sanitaires",
      str_detect(service_contributeur, "^EPTB") ~ "Eau",
      str_detect(service_contributeur, "^OFB")  ~ "Biodiversité/eau",
      str_detect(service_contributeur, "^CLE")  ~ "Eau",
      str_detect(service_contributeur, "^Mairie ")  ~ "Territoire",
      # Correspondances exactes ou quasi exactes
      service_contributeur %in% c(
        "Chambre agriculture 34 - MESE 34",
        "DRAAF Occitanie - DRAAF Occitanie"
      ) ~ "Agriculture",

      service_contributeur == "DDTM 34 - SAF" ~ "Forêt",
      service_contributeur == "DDTM 34 - SERN Eau" ~ "Eau",
      service_contributeur == "DDTM 34 - SERN NB" ~ "Biodiversité/Natura 2000",
      service_contributeur == "DDTM 34 - SERN PRNT" ~ "Risques",
      service_contributeur == "DDTM 34 - STU" ~ "Alignement d’arbres",
      service_contributeur == "DDPP 34 - DDPP 34" ~ "Aspects sanitaires",
      service_contributeur == "DRAC Occitanie - Service Régional Archéologie" ~ "Patrimoine Archéo",
      service_contributeur == "DREAL Occitanie - DE/DB/DBE - Biodiversité Montpellier" ~
        "Biodiversité/Dérogation Espèces protégées",
      service_contributeur == "DREAL Occitanie - DEC DEDD - autorité environnementale" ~ "Environnement",
      service_contributeur == "DREAL Occitanie - UD 34" ~ "Risques technologiques",
      service_contributeur == "DREAL Occitanie - Sécurité ouvrages hydrauliques" ~ "Eau",
      service_contributeur == "DREAL Occitanie - DSP Est Montpellier" ~ "Aspect paysager",
      service_contributeur == "SDIS - 34" ~ "Feu",
      service_contributeur == "UDAP - 34" ~ "Patrimoine",
      service_contributeur == "VNF_canal du rhone a sete - UTI" ~ "Canal du Midi",
      service_contributeur == "CNPN" ~ "Nature",

      # Cas par défaut
      TRUE ~ NA_character_
    )
  )
tictoc::toc()

tictoc::tic("read_ods Rubriques_IOTA")
Rubriques_IOTA <- read_ods(file.path(data, "Tableau_Info_Supp.ods"), sheet = 4) %>%
  clean_names()
#shinyApp(ui, server)
tictoc::toc()

tictoc::tic("resume")
resume <- read_ods(file.path(data, "Tableau_Info_Supp.ods"), sheet = 5) %>%
  clean_names()
tictoc::toc()

system("gio mount -u smb://10.34.9.19/dossiers/")
