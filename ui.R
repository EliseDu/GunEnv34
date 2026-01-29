#ui <- 
  fluidPage(
  #shinydisconnect::disconnectMessage2(),
  tags$head(
    tags$title("GUNenv 34"), 
    tags$link(rel="icon", 
              href="favicon.ico", 
              type="image/x-icon"),
    
    # Ajout du fichier CSS personnalisé
    tags$link(rel="stylesheet", type="text/css", href="styles.css")
  ),
  # ------ TITLE BANNER (fond bleu avec image de fond) -------------------------
  titlePanel(
    fluidRow(
      div(class = "header-logo",
            tags$a(
              href = 'https://www.herault.gouv.fr',
              tags$img(src = 'logopref.jpg', height = '150px', id = "logo1"),
              target = '_blank'),
          h1(paste0("GUNenv 34 - ", nb_dossiers_en_cours, " dossiers en cours"), style = "margin: 0;"),
      )
    )
  ),       
  
      
  # Titre centré dans l'espace restant
  tabsetPanel(
    id = "mainnav",
    #Deboggage
    # tabPanel( textInput("cmd", "Commande :", value = "ls -l"),
    #           actionButton("run", "Ex  cuter"),
    #           verbatimTextOutput("output"))
    tabPanel(
      div(icon("calendar"), "Projets"),
      div(
        style = "background-color:#DAE4F2;
                padding:12px 18px;
                border-radius:8px;
                color: #000091;
                margin-bottom:25px;
                display:flex;
                flex-direction:column;
                align-items:center;
                text-align:center;
                font-size:120%;
                gap:6px;",
        p(
          paste0("Temps moyen d'instruction (hors temps imputable au pétitionnaire) : ",
                 round(resume_moyennes$temps_moyen_instruction, 1),
                 " jours"),
        ),
        p(
          paste0("Temps moyen imputable au pétitionnaire : ",
                 round(resume_moyennes$temps_moyen_suspension, 1),
                 " jours"),
        )
      ),
      selectInput("agent", "Agent chargé de la procédure", choices =  c("Tous les agents", sort(unique(StructEnsemble_data$agent_procedure))),
                  selected = "Tous les agents"),
      checkboxInput("en_cours", "Projets en cours uniquement", value = TRUE),
      uiOutput("timelineList")
    ),
    tabPanel(
      div(icon("cog"), "Détail projet"),
      fluidRow(
        column(width = 2,
               br(),
          selectInput("agent", "Agent chargé de la procédure", choices =  c("Tous les agents", sort(unique(StructEnsemble_data$agent_procedure))),
                      selected = "Tous les agents"),
          checkboxInput("en_cours", "Projets en cours uniquement", value = TRUE),
          selectInput("AIOT", label = "Projet", choices = NULL)
          ),
        column(width = 3,
               br(),
          uiOutput("infoProjet")
        )
      ),
      timevisOutput("timelineProjet")
      ),
    tabPanel(
      div(icon("book"), "Générer rapport CODERST"),
      ODTexport_UI("export_ODT")
    )
)
)
    
  