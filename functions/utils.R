# generate a random string of 16 characters
randomID <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
}

prettyDate <- function(d) {
  if (is.null(d)) return()
  posix <- as.POSIXct(d, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  corrected <- lubridate::with_tz(posix, tzone = Sys.timezone())
  format(corrected, "%Y-%m-%d %H:%M:%OS %Z")
}

calcul_suspension_totale <- function(df) {
  
  # garder les suspensions
  df <- df %>% filter(group %in% c("Demande de compléments", "Demande de compléments (recevabilité du dossier)", "Commentaire"))
  
  today <- Sys.Date()
  df %>% 
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
      duree_jours = as.integer(end - start)
    ) %>%
    summarise(total = sum(duree_jours, na.rm = TRUE)) %>%
    pull(total)
}

safe_dmy <- function(x) {
  x <- trimws(x)  # retirer espaces
  x[x %in% c("", " ", "-", "NA", "N/A", "nc", "NC", "s/o", "S/O", "ND", "nd")] <- NA
  
  suppressWarnings(lubridate::dmy(x))
}

import_aiot <- function(fichier) {
  purrr::map_dfr(fichier, function(f) {
    code <- sub(".*\\(([0-9]{10})\\).*", "\\1", basename(f))
    readxl::read_excel(f, skip = 1) |>
      dplyr::mutate(code_aiot = code)
  })
}

build_phases_aiot <- function(code) {
  
  projetTime_aiot <- projetTime %>% filter(code_aiot == code)
  struct_aiot <- StructEnsemble %>% filter(code_aiot == code)
  date_seuil <- dmy("01/11/2024")
  # CAS 1 — aucune ligne dans StructEnsemble → pas de dépôt → Phase amont
  if (nrow(struct_aiot) == 0) {
    return(tibble(
      content = "Phase amont",
      start = projetTime_aiot$start[
        projetTime_aiot$group == "Sollicitation de l'exploitant"
      ] |> first(),
      end = NA,
      code_aiot = code,
      group_ensemble = NA_character_,
      group_projet = "Phases d'instruction",
      modalite = NA,
      avis = projetTime_aiot$avis %>% first()
      
    ))
  }
  
  # CAS 2 — structure présente mais pas de dépôt du dossier
  has_depot <- any(struct_aiot$content == "Dépôt du dossier", na.rm = TRUE)
  
  if (!has_depot) {
    return(tibble(
      content = "Phase amont",
      start = projetTime_aiot$start[
        projetTime_aiot$group == "Sollicitation de l'exploitant"
      ] |> first(),
      end = NA,
      code_aiot = code,
      group_ensemble = struct_aiot$group %>% first(),
      group_projet = "Phases d'instruction",
      modalite = NA,
      avis = projetTime_aiot$avis %>% first()
      
    ))
  }
  
  # SI ON ARRIVE ICI → dépôt existe
  date_depot <- struct_aiot %>%
    filter(content == "Dépôt du dossier") %>%
    summarise(date_depot = max(start, na.rm = TRUE)) %>%
    pull(date_depot)
  
  
  # CAS 2 : dossier NON déposé → seulement AMONT
  if (is.na(date_depot)) {
    result <- tibble(
      content = "Phase amont",
      start = projetTime_aiot$start[projetTime_aiot$group == "Sollicitation de l'exploitant"] |> first(),
      end = NA,
      code_aiot = code,
      group_ensemble = struct_aiot$group |> first(),
      group_projet = "Phases d'instruction",
      modalite = NA,
      avis = projetTime_aiot$avis %>% first()
      
    )
    
    return(result)
  }
  
  
  # CAS 3 : dépôt avant seuil → 2 phases
  if (date_depot < date_seuil) {
    
    result <- tibble(
      content = c("Examen et consultations", "Décision"),
      start = c(
        date_depot,
        projetTime_aiot$start[
          projetTime_aiot$group ==
            "Information : démarrage de la phase de décision – Transmission rapport commissaire enquêteur"
        ] |> first()
      ),
      end = c(
        projetTime_aiot$start[
          projetTime_aiot$group ==
            "Information : démarrage de la phase de décision – Transmission rapport commissaire enquêteur"
        ] |> first(),
        
        projetTime_aiot$start[
          projetTime_aiot$group == "Information : transmission AP Décision"
        ] |> first()
      ),
      code_aiot = code,
      group_ensemble = struct_aiot$group %>% first(),
      group_projet = "Phases d'instruction",
      modalite = struct_aiot$modalite %>% first(),
      avis = projetTime_aiot$avis %>% first()
      
    )
    
  } else {
    
    # CAS 4 : dépôt après seuil → 4 phases
    result <- tibble(
      content = c(
        "Phase amont",
        "Complétude et régularité",
        "Examen et consultations",
        "Décision"
      ),
      start = c(
        projetTime_aiot$start[
          projetTime_aiot$group == "Sollicitation de l'exploitant"
        ] |> first(),
        
        date_depot,
        
        projetTime_aiot$start[
          projetTime_aiot$group ==
            "Information au pétitionnaire du démarrage de la phase d'examen/consultation"
        ] |> first(),
        
        projetTime_aiot$start[
          projetTime_aiot$group ==
            "Information : démarrage de la phase de décision – Transmission rapport commissaire enquêteur"
        ] |> first()
      ),
      end = c(
        date_depot,
        
        projetTime_aiot$start[
          projetTime_aiot$group ==
            "Information au pétitionnaire du démarrage de la phase d'examen/consultation"
        ] |> first(),
        
        projetTime_aiot$start[
          projetTime_aiot$group ==
            "Information : démarrage de la phase de décision – Transmission rapport commissaire enquêteur"
        ] |> first(),
        
        struct_aiot$end[struct_aiot$content == "Dépôt du dossier"] |> first()
      ),
      code_aiot = code,
      group_ensemble = struct_aiot$group %>% first(),
      group_projet = "Phases d'instruction",
      modalite = struct_aiot$modalite %>% first(),
      avis = projetTime_aiot$avis %>% first()
      
    )
  }
  
  
  # 🌟 FINAL PATCH : propagation de la date de décision vers les NA
  end_decision <- result$end[result$content == "Décision"] |> first()
  
  if (!is.na(end_decision)) {
    result <- result %>%
      mutate(
        end = if_else(
          is.na(end),
          end_decision,
          end
        )
      )
  }
  
  if (!is.na(end_decision)) {
    result <- result %>%
      mutate(
        start = case_when(
          content %in% c("Complétude et régularité", "Examen et consultations", "Décision") &
            is.na(start) ~ end_decision,
          TRUE ~ start
        )
      )
  }
  
  return(result)
}

build_struct <- function(ligne) {
  tibble(
  content = c(
            "Dépôt du dossier",
            ligne$derniere_etape,
            "Avis de l’AE",
            "Date de proposition de mise à la consultation du public",
            "Consultation publique",
            "Coderst"
            ),
  start = c(
            ligne$depot_du_dossier,
            ligne$date,
            ligne$date_demande_avis_ae,
            ligne$date_de_proposition_de_mise_a_la_consultation_du_public,
            ligne$date_debut_consultation_public,
            ligne$date_reunion_coderst_cndps
            ),
  end = c(
            ligne$date_conclusion,
            NA,
            ligne$date_avis_ae,
            NA,
            ligne$date_fin_consultation_public,
            NA
            ),
  group = ligne$procedure,
  agent_procedure = ligne$agent_procedure,
  modalite = ligne$modalite
  )
}



build_struct_all <- function(gunenv) {
  bind_rows(
    lapply(seq_len(nrow(gunenv)), function(i) {
        df <- build_struct(gunenv[i, ])
        df$code_aiot <- gunenv$code_aiot[i]
        return(df)
    })
  )
}

format_aiot <- function(x) {
  x <- sprintf("%011d", as.numeric(x))  # 11 chiffres fixes
  sub("^(..)(....)(.....)$", "\\1 \\2 \\3", x)
}