#
# Application Shiny pour tester les commandes système
# Version améliorée avec capture complète des sorties et erreurs
#

library(shiny)

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Testeur de commandes système"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("cmd", "Commande :", value = "whoami"),
            actionButton("run", "Exécuter"),
            hr(),
            h4("Commandes utiles :"),
            actionButton("cmd_whoami", "whoami"),
            actionButton("cmd_id", "id"),
            actionButton("cmd_mount", "mount | grep gvfs"),
            actionButton("cmd_gio", "gio mount -l"),
            actionButton("cmd_ps", "ps aux | grep gvfsd"),
            actionButton("cmd_ls_y", "ls -la /Y/"),
            actionButton("cmd_ls_mnt", "ls -la /mnt/"),
            actionButton("cmd_test_smb", "test -d /run/user/103/gvfs && echo 'existe' || echo 'inexistant'"),
            hr(),
            h4("Démontage :"),
            actionButton("cmd_unmount", "gio mount -u smb://10.34.9.19/dossiers/"),
            actionButton("cmd_kill_gvfs", "pkill gvfsd-smb")
        ),
        
        mainPanel(
            h3("Résultat :"),
            verbatimTextOutput("output"),
            hr(),
            h4("Détails :"),
            verbatimTextOutput("details")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Fonction pour exécuter une commande et capturer tout
    executer_commande <- function(cmd) {
        # Créer un fichier temporaire pour stderr
        stderr_file <- tempfile()
        on.exit(unlink(stderr_file))
        
        # Exécuter la commande
        result <- tryCatch({
            stdout <- system2("bash", 
                            args = c("-c", cmd),
                            stdout = TRUE, 
                            stderr = stderr_file,
                            wait = TRUE)
            
            # Lire stderr
            stderr <- if (file.exists(stderr_file)) {
                readLines(stderr_file, warn = FALSE)
            } else {
                character(0)
            }
            
            # Récupérer le code de sortie
            exit_code <- attr(stdout, "status")
            if (is.null(exit_code)) exit_code <- 0
            
            list(
                stdout = stdout,
                stderr = stderr,
                exit_code = exit_code,
                success = exit_code == 0
            )
        }, error = function(e) {
            list(
                stdout = character(0),
                stderr = paste("Erreur R:", e$message),
                exit_code = -1,
                success = FALSE
            )
        })
        
        return(result)
    }
    
    # Gestionnaire pour les boutons de commandes prédéfinies
    observeEvent(input$cmd_whoami, { updateTextInput(session, "cmd", value = "whoami") })
    observeEvent(input$cmd_id, { updateTextInput(session, "cmd", value = "id") })
    observeEvent(input$cmd_mount, { updateTextInput(session, "cmd", value = "mount | grep gvfs") })
    observeEvent(input$cmd_gio, { updateTextInput(session, "cmd", value = "gio mount -l") })
    observeEvent(input$cmd_ps, { updateTextInput(session, "cmd", value = "ps aux | grep gvfsd") })
    observeEvent(input$cmd_ls_y, { updateTextInput(session, "cmd", value = "ls -la /Y/") })
    observeEvent(input$cmd_ls_mnt, { updateTextInput(session, "cmd", value = "ls -la /mnt/") })
    observeEvent(input$cmd_test_smb, { 
        updateTextInput(session, "cmd", value = "test -d /run/user/103/gvfs && echo 'existe' || echo 'inexistant'") 
    })
    observeEvent(input$cmd_unmount, { 
        updateTextInput(session, "cmd", value = "gio mount -u smb://10.34.9.19/dossiers/")
        click("run")
    })
    observeEvent(input$cmd_kill_gvfs, { 
        updateTextInput(session, "cmd", value = "pkill gvfsd-smb")
        click("run")
    })
    
    # Résultat de la commande
    cmdResult <- eventReactive(input$run, {
        req(input$cmd)
        executer_commande(input$cmd)
    })
    
    # Affichage de la sortie principale
    output$output <- renderText({
        result <- cmdResult()
        
        if (length(result$stdout) == 0 && length(result$stderr) == 0) {
            return("[Aucune sortie]")
        }
        
        output_text <- ""
        
        if (length(result$stdout) > 0) {
            output_text <- paste(result$stdout, collapse = "\n")
        }
        
        if (length(result$stderr) > 0) {
            stderr_text <- paste(result$stderr, collapse = "\n")
            if (nchar(output_text) > 0) {
                output_text <- paste0(output_text, "\n\n=== ERREURS ===\n", stderr_text)
            } else {
                output_text <- paste0("=== ERREURS ===\n", stderr_text)
            }
        }
        
        output_text
    })
    
    # Affichage des détails
    output$details <- renderText({
        result <- cmdResult()
        paste0(
            "Code de sortie: ", result$exit_code, "\n",
            "Succès: ", ifelse(result$success, "OUI", "NON"), "\n",
            "Lignes stdout: ", length(result$stdout), "\n",
            "Lignes stderr: ", length(result$stderr), "\n",
            "Utilisateur: ", system2("whoami", stdout = TRUE), "\n",
            "UID: ", system2("id", args = "-u", stdout = TRUE)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
