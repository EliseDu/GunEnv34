ODTexport_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        div(class = "input-card",
          selectInput(ns("agent"), "Agent chargé de la procédure", choices =  c("Tous les agents", sort(unique(StructEnsemble_data$agent_procedure))),
                      selected = "Tous les agents")
          ),
        div(class = "input-card",
          selectInput(ns("AIOT"), label = "Projet", choices = NULL)
        ),
        div(class = "input-card",
          textInput(
            inputId = ns("type_operation"),
            label = "Type d’opération",
            placeholder = "ex. : aménagement hydraulique, création d’ouvrage…")
          ),
        div(class = "input-card",  
          textInput(
            inputId = ns("description_courte"),
            label = "Description courte du projet",
            placeholder = "Résumé synthétique en une phrase")
        ),
        div(class = "input-card",  
          textAreaInput(
            inputId = ns("contexte"),
            label = "Contexte général et historique",
            rows = 5,
            placeholder = "Décrire le contexte réglementaire, territorial et historique du projet")
        ),
        div(class = "input-card",  
          textInput(
            inputId = ns("localisation"),
            label = "Localisation du projet",
            placeholder = "Communes concernées / secteur géographique")
        ),
        div(class = "input-card",  
          textAreaInput(
            inputId = ns("description_projet"),
            label = "Objectifs et présentation du projet",
            rows = 4,
            placeholder = "Le projet vise à : …")
          ),
        div(class = "input-card",  
          textAreaInput(
            inputId = ns("travaux"),
            label = "Description synthétique des travaux",
            rows = 4,
            placeholder = "Les travaux consistent en : …")
          ),
        div(class = "input-card",
          textAreaInput(
            inputId = ns("travaux_long"),
            label = "Description détaillée de la phase de travaux",
            rows = 6,
            placeholder = "Déroulement, phasage, durée prévisionnelle, mise en service")
        )
    ),
    column(
      width = 6, 
      div(class = "input-card",
        checkboxGroupInput(
          inputId = ns("rubriques_aiot"),
          label = "Rubriques IOTA",
          choices = setNames(
            Rubriques_IOTA$rubriques,
            paste0(
              Rubriques_IOTA$rubriques, " - ",
              Rubriques_IOTA$regime, " - ",
              Rubriques_IOTA$caracteristiques)
            )
            )
          ),
      div(class = "input-card",
      textAreaInput(
        inputId = ns("consultation_publique"),
        label = "Consultation du public",
        rows = 4,
        placeholder = "Modalités de la consultation du public")
      ),
      div(class = "input-card",
      textAreaInput(
        inputId = ns("reserves"),
        label = "Réserves et observations",
        rows = 4,
        placeholder = "Réserves éventuelles formulées par les services ou organismes")
      ),
      div(class = "input-card",
      textAreaInput(
        inputId = ns("recommandations"),
        label = "Recommandations",
        rows = 4,
        placeholder = "Recommandations principales issues de l’instruction")
      ),
      div(class = "input-card",
      textInput(
        inputId = ns("ratio_compensation"),
        label = "Ratio de compensation environnementale",
        placeholder = "ex. : ratio 1:2")
      ),
      div(class = "input-card",
      textAreaInput(
        inputId = ns("integration_envir"),
        label = "Mesures d’intégration environnementale",
        rows = 4,
        placeholder = "Mesures d’évitement, de réduction et de compensation")
      ),
      div(class = "input-card",
      textAreaInput(
        inputId = ns("integration_envir_travaux"),
        label = "Mesures environnementales en phase travaux",
        rows = 4,
        placeholder = "Mesures spécifiques prévues pendant la phase travaux")
      ),
      # useShinyjs(),
      downloadButton(
        outputId = ns("generate_report"),
        label   = "Générer le rapport CODERST",
        icon    = icon("file-export")
      )
      )
      )
  )
}
ODTexport_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      projets <- reactive({
        req(input$agent)
        data <- StructEnsemble_data
        # --- Filtre agent ---
        if (input$agent == "Tous les agents") {
          data
        }else {
          data <- data %>% filter(agent_procedure == input$agent)
        }
      })
      
      observe({
        df <- projets()
        req(nrow(df) > 0)
        
        codes <- unique(df$code_aiot)
        
        labels <- gunenv$procedure[
          match(codes, gunenv$code_aiot)
        ]
        
        updateSelectInput(
          session,
          "AIOT",
          choices = setNames(codes, labels)
        )
      })
      
      # observe({
      #   req_fields <- c(
      #     input$type_operation,
      #     input$description_courte,
      #     input$contexte,
      #     input$localisation,
      #     input$description_projet,
      #     input$travaux,
      #     input$travaux_long,
      #     input$rubriques_aiot,
      #     input$consultation_publique,
      #     input$reserves,
      #     input$ratio_compensation,
      #     input$integration_envir,
      #     input$integration_envir_travaux
      #   )
      #   
      #   if (all(nzchar(req_fields))) {
      #     shinyjs::enable("generate_report")
      #   } else {
      #     shinyjs::disable("generate_report")
      #   }
      # })
      
      output$generate_report <- downloadHandler(
        filename = function() {
          paste0("Rapport_CODERST_", Sys.Date(), ".odt")
        },
        
        content = function(file) {
        missing <- c()
        
        if (!nzchar(input$type_operation)) missing <- c(missing, "type d’opération")
        if (!nzchar(input$description_courte)) missing <- c(missing, "description courte")
        if (!nzchar(input$contexte)) missing <- c(missing, "contexte")
        if (!nzchar(input$localisation)) missing <- c(missing,  "localisation du projet")
        if (!nzchar(input$description_projet)) missing <- c(missing,  "objectifs et présentation du projet")
        if (!nzchar(input$travaux)) missing <- c(missing, "Description synthétique des travaux")
        if (!nzchar(input$travaux_long)) missing <- c(missing, "description détaillée de la phase de travaux")
        if (is.null(input$rubriques_aiot) || length(input$rubriques_aiot) == 0) missing <- c(missing, "rubriques IOTA")
        if (!nzchar(input$consultation_publique)) missing <- c(missing,  "consultation du public")
        if (!nzchar(input$reserves)) missing <- c(missing,  "réserves et observations")
        if (!nzchar(input$ratio_compensation)) missing <- c(missing,  "ratio de compensation environnementale")
        if (!nzchar(input$integration_envir)) missing <- c(missing,  "mesures d’intégration environnementale")
        if (!nzchar(input$integration_envir_travaux)) missing <- c(missing,  "mesures environnementales en phase travaux")
        
        if (length(missing) > 0) {
          showModal(
            modalDialog(
              title = "Champs obligatoires manquants",
              tagList(
                "Veuillez renseigner :",
                tags$ul(
                  lapply(missing, tags$li)
                )
              ),
              footer = modalButton("Fermer"),
              easyClose = TRUE
            )
          )
          stop("Téléchargement interrompu : champs manquants")
        }
        ordres <- c(
          "première", "deuxième", "troisième", "quatrième", "cinquième",
          "sixième", "septième", "huitième", "neuvième", "dixième"
        )
        demande_complements <- projetTime_data %>% 
          filter(code_aiot == input$AIOT,
                 group %in% c("Demande de compléments", "Demande de compléments (recevabilité du dossier)")) %>% 
          arrange(start) %>% select(start, end)
        
        if (nrow(demande_complements) == 0) {
          demande_complements_text <- NULL
        } else {
          demande_complements_text <- purrr::map_chr(
            seq_len(nrow(demande_complements)),
            function(i) {
              glue(
                "Une ", ordres[i],
                " demande de compléments a été transmise le ",
                format(demande_complements$start[i], "%d %B %Y"),
                " à laquelle ",
                gunenv$aiot[gunenv$code_aiot == input$AIOT],
                " a apporté une réponse le ",
                format(demande_complements$end[i], "%d %B %Y"),
                "."
              )
            }
          )
        }
        
        completude <- projetTime_data %>% 
          filter(code_aiot == input$AIOT,
                 group == "Information : démarrage de la phase de décision – Transmission rapport commissaire enquêteur") %>% 
          first() %>% pull(start)
        
        services_contributeurs <- projetTime_data %>% 
          filter(code_aiot == input$AIOT,
                 group %in% c("Demande de contribution aux services contributeurs (phase de recevabilité)","Demande de contribution")) %>% 
          select(content, end) %>% filter(!is.na(end)) %>% left_join(thematiques_services_org, by = c("content"= "service_contributeur")) %>% 
          mutate(end = format(end, "%d %B %Y")) %>% select(thematique, content, end) %>% rename("Date avis" =end, "Thématique" = thematique, "Nom du service" = content)
        
        organismes <- projetTime_data %>% 
          filter(code_aiot == input$AIOT,
                 group == "Demande d'avis à un organisme") %>% 
          select(content, end) %>% filter(!is.na(end)) %>% left_join(thematiques_services_org, by = c("content"= "service_contributeur")) %>% 
          mutate(end = format(end, "%d %B %Y")) %>% select(thematique, content, end) %>% rename("Date avis" =end, "Thématique" = thematique, "Nom du service" = content)
        
        cnpn_avis <- projetTime_data %>% 
          filter(code_aiot == input$AIOT,
                 group == "Demande d'avis à un organisme",
                 content == "CNPN - CNPN") %>% 
          mutate(avis = case_when(avis == "F" ~ "avis favorable",
                                  avis == "FR" ~ "avis favorable sous conditions",
                                  avis == "D" ~ "avis défavorable"),
                 date_avis = format(end, "%d %B %Y")) 
        cnpn <- glue("Le conseil national de protection de la nature (CNPN) a émis un {cnpn_avis$avis}, le {cnpn_avis$date_avis}")
        
        enquete_publique <- StructEnsemble_data %>% 
          filter(code_aiot == input$AIOT,
                 content == "Consultation publique") %>% 
          mutate(modalite = case_when(modalite == "Soumis à enquête publique" ~"enquête publique",
                                      modalite == "Enquête publique"~"enquête publique"),
                 modalite = str_to_lower(modalite),
                 start = format(start, "%d %B %Y"),
                 end = format(end, "%d %B %Y")) %>% 
          select(modalite, start, end)
        
        text_enquete_publique <- if (length(enquete_publique) == 0) {
          NULL
        } else {
          glue(
            "l’{enquete_publique$modalite}. L’{enquete_publique$modalite} s’est déroulée du {enquete_publique$start} au {enquete_publique$end} inclus."
          )
        }
        
        proced_embarq <- procedures_embarquees_list %>%
          filter(code_aiot %in% input$AIOT) %>% pull(intitule_long) %>% 
          na.omit() %>%
          unique()
        
        proced_embarq_md <- if (length(proced_embarq) == 0) {
          NULL
        } else {
          paste0("- ", proced_embarq, collapse = "\n")
        }
        
        rubriques_aiot <- Rubriques_IOTA %>%
          filter(rubriques %in% input$rubriques_aiot) %>% 
          rename("Rubriques" =rubriques, "Régime" = regime, "Caractéristiques" = caracteristiques)
        
        old_wd <- getwd()
        setwd(tempdir())
        on.exit(setwd(old_wd), add = TRUE)

        file.copy(from = file.path(APP_DIR, "template", "rapport_CODERST.Rmd"), to = file.path(tempdir(), "rapport_CODERST.Rmd"), overwrite = TRUE)
        file.copy(from = file.path(APP_DIR, "template", "style.odt"), to =file.path(tempdir(), "style.odt"), overwrite = TRUE)
        file.copy(from = file.path(APP_DIR, "template", "logopref.jpg"), to = file.path(tempdir(), "logopref.jpg"), overwrite = TRUE)
        #
        # message("WD = ", getwd())
        # message("Files in WD:")
        # print(list.files())
        # 
          output_file <- tryCatch(
            rmarkdown::render(
              input = "rapport_CODERST.Rmd",
              output_format = "odt_document",
              output_dir = tempdir(),
              params = list(
                aiot = gunenv$aiot[gunenv$code_aiot == input$AIOT],
                type_operation = input$type_operation, #input$text
                commune = gunenv$commune[gunenv$code_aiot == input$AIOT],
                procedure = gunenv$procedure[gunenv$code_aiot == input$AIOT],
                description_courte = input$description_courte, #input$text : déscription succinte (1 ligne)
                contexte = input$contexte, #input$text : Description du projet
                localisation = input$localisation,#input$text : de description de l’implantation du projet ou des communes traversées
                description_projet = input$description_projet, #input$text : Le projet vise à :
                travaux = input$travaux,#input$text : Les travaux consistent en :
                travaux_long = input$travaux_long, #input$text : La réalisation du COM s'échelonnera sur une durée prévisionnelle de 5 ans avec une mise en service prévue à l'horizon 2030. 
                depot_dossier = gunenv$depot_du_dossier[gunenv$code_aiot == input$AIOT],
                code_aiot = format_aiot(input$AIOT),
                rubriques_aiot = rubriques_aiot, #Checkbox listes à cocher, tableau 3 colonnes Rubriques, régime, Caractéristiques
                procedures_embarquees = proced_embarq_md,#
                demande_complements = demande_complements_text, 
                completude = format(completude, "%d %B %Y"),
                services_contributeurs = services_contributeurs,
                organismes = organismes,
                cnpn = cnpn,
                reserves = input$reserves,
                enquete_publique = text_enquete_publique,
                consultation_publique = input$consultation_publique,
                avis_consultation_publique = "avis favorable en date du 2 octobre 2025", #je ne trouve nulle part l'avis du commissaire enquêteur 
                constat_procedures_embarquees = procedures_embarquees$procedures[procedures_embarquees$code_aiot == input$AIOT],
                recommandations = input$recommandations,
                ratio_compensation = input$ratio_compensation,
                integration_envir = input$integration_envir,
                integration_envir_travaux = input$integration_envir_travaux
              ),
              envir = globalenv(),
              quiet = FALSE
            ),
            error = function(e) {
              message("ERREUR RENDER ODT : ", e$message)
              stop(e)
            }
          )
          
          file.copy(output_file, file, overwrite = TRUE)
    }
  )
    }
  )
}