#server <- 
  function(input, output, session) {
    
  #Déboggage
    # cmdResult <- eventReactive(input$run, {
    #   req(input$cmd)                     # assure que la commande n'est pas vide
    #   tryCatch(
    #     system(input$cmd, intern = TRUE),# execute la commande et capture la sortie
    #     error = function(e) paste("Erreur :", e$message)
    #   )
    # })
    # 
    # output$output <- renderText({
    #   cmdResult()
    # })
    #Fin déboggage

  projets <- reactive({
    data <- StructEnsemble_data
    # --- Filtre agent ---
    if (input$agent == "Tous les agents") {
      data
    }else {
      data <- data %>% filter(agent_procedure == input$agent)
    }

     if (isTRUE(input$en_cours)) {
      data %>%
        filter(content == "Dépôt du dossier", is.na(end)) %>%
        distinct(code_aiot) %>%
        pull(code_aiot)
    } else {
      data %>%
        distinct(code_aiot) %>%
        pull(code_aiot)
    }
  })

  observe({
    choix <- projets()
    noms <- gunenv$procedure[match(choix, gunenv$code_aiot)]
    labels <- paste0(
      "N° AIOT ",
      format_aiot(choix),
      " – ",
      noms
    )
    updateSelectInput(
      session,
      "AIOT",
      choices = setNames(choix, labels)
    )
  })


output$timelineList <- renderUI({
  tagList(
    lapply(projets(), function(p) {
      nom_projet <- StructEnsemble_data %>%
        filter(code_aiot == p) %>%
        slice(1) %>% pull(group)

      nom_commune <- gunenv %>%
        filter(code_aiot == p) %>%
        pull(commune)

      labels <- paste0(
          "N° AIOT ",
          format_aiot(p),
          " – ",
          nom_projet,
          ", ",
          nom_commune
        )

      suspension <- table_suspensions %>%
        filter(code_aiot == p) %>%
        pull(total_suspension)

      total_instruction <- table_suspensions %>%
        filter(code_aiot == p) %>%
        pull(total_instruction)

      instruction_hs <- table_suspensions %>%
        filter(code_aiot == p) %>%
        pull(instruction_hs)

      modalite <- StructEnsemble_data %>%
        filter(code_aiot == p) %>%
        slice(1) %>% pull(modalite)

      Phase_amont <- StructEnsemble_data %>%
        filter(code_aiot == p, content == "Phase amont") %>%
        pull(duree_jours)
      Completude_regularite <- StructEnsemble_data %>%
        filter(code_aiot == p, content == "Complétude et régularité") %>%
        pull(duree_jours)
      Examen_consultations <- StructEnsemble_data %>%
        filter(code_aiot == p, content == "Examen et consultations") %>%
        pull(duree_jours)
      Decision <- StructEnsemble_data %>%
        filter(code_aiot == p, content == "Décision") %>%
        pull(duree_jours)
      commentaire <- resume %>% filter(code_aiot == p) %>% pull(commentaire)


      div(
        #h3(paste(labels," : ", if(!is.na(modalite)) modalite else "")),
        h3(labels),


        div(
          style = "
      display: flex;
      gap: 20px;
      align-items: stretch;
      flex-wrap: wrap;
    ",

          # === Encadré informations ===
          div(
            style = "
        background-color:#DAE4F2;
        padding:12px 18px;
        border-radius:8px;
        margin-bottom:25px;
        flex: 1;
      ",
            h4(paste0("Délais : ", total_instruction, " jours d'instruction"), style = "font-weight:bold;"),

            p(paste0(
              "Temps d'instruction hors temps imputable au pétitionnaire : ",
              instruction_hs,
              " jours"
            )),
            p(paste0(
              "Temps imputable au pétitionnaire : ",
              suspension,
              " jours"
            )),
            div(
              style = "display: flex; gap: 10px; flex-wrap: wrap;",
              if (length(Phase_amont) > 0 && !is.na(Phase_amont))
                p(paste0("Phase amont : ", Phase_amont, " jours")),
              if (length(Completude_regularite) > 0 && !is.na(Completude_regularite))
                p(paste0("Complétude et régularité : ", Completude_regularite, " jours")),
              if (length(Examen_consultations) > 0 && !is.na(Examen_consultations))
                p(paste0("Examen et consultations : ", Examen_consultations, " jours")),
              if (length(Decision) > 0 && !is.na(Decision))
                p(paste0("Décision : ", Decision, " jours"))
            )
          ),

          # === Encadré suivi (conditionnel) ===

            div(
              style = "
          background-color:#DAE4F2;
          padding:12px 18px;
          border-radius:8px;
          margin-bottom:25px;
          flex: 1;
        ",

              if(!is.na(modalite)) h4(modalite, style = "font-weight:bold;"),
              if (length(commentaire) > 0 && !is.na(commentaire) && commentaire != "")
                div(
                  h4("Suivi", style = "font-weight:bold;"),
                  p(commentaire)
                )
            )
        ),

        timevisOutput(paste0("timeline_", p))
      )
    })
  )
})

observe({
  lapply(projets(), function(p) {
    output[[paste0("timeline_", p)]] <- renderTimevis({
      StructEnsemble_data
      data <- StructEnsemble_data %>% filter(code_aiot == p) %>% mutate(end = if_else(end == dmy("01-01-2000"), Sys.Date(), end)) %>%
      select( id, content, start, end ,title, style)
      timevis(data, options = list(editable = TRUE))
    })
  })
})

output$infoProjet <- renderUI({
  req(input$AIOT)

  suspension <- table_suspensions %>% filter(code_aiot == input$AIOT) %>%
    pull(total_suspension)
  instruction <- table_suspensions %>% filter(code_aiot == input$AIOT) %>%
    pull(instruction_hs)
  procedures <- procedures_embarquees %>% filter(code_aiot == input$AIOT) %>%
    pull(procedure)
  eval <- eval_environnementale %>% filter(code_aiot == input$AIOT) %>%
    pull(evaluation_environnementale)
  div(
    style = "background-color:#DAE4F2;
             padding:12px 18px;
             border-radius:8px;
             margin-bottom:25px;
             max-width:900px;",
    br(),
    p(glue("Temps d'instruction hors temps imputable au pétitionnaire : {instruction} jours")),
    p(glue("Temps imputable au pétitionnaire : {suspension} jours")),
    if (length(procedures) > 0 && !is.na(procedures))
      p(paste("Procédures embarquées :", procedures)),
    if (length(eval) > 0 && !is.na(eval))
      p(paste("Evaluation environnementale : ", eval))
    
  )
})

output$timelineProjet <- renderTimevis({
  data <- projetTime_data %>% filter(code_aiot == input$AIOT) %>%
    select( id, content, start, end, group, style, title)
   groups <- tibble(
     id = unique(data$group),
     content = unique(data$group)
   )
  timevis(data, groups = groups,
          options = list(editable = TRUE))
})

ODTexport_Server("export_ODT")



}
