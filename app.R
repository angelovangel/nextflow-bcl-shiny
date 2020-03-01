 #### nextflow-bcl app ####
 
 # a shiny frontend for the nextflow-bcl pipeline
 # https://github.com/angelovangel/nextflow-bcl.git
 
 library(shiny)
 library(shinyFiles)
 library(shinyjs)
 library(shinyalert)
 library(processx)
 library(stringr)
 library(digest)
 

 #### ui ####
 ui <- function(x) {
   
   navbarPage(title = tags$button("nextflow-bcl",
                               id = "bclButton", # events can be listened to as input$bclButton in server
                               class = "action-button", #shiny needs this class def to work
                               title = "If you want to start over, just reload the page.",
                               onMouseOver = "this.style.color='orange'", # old school js
                               onMouseOut = "this.style.color='green'",
                               style = "color: green; font-weight: bold; border: none; background-color: inherit;"),
             
              windowTitle = "nextflow-bcl", 
              collapsible = TRUE,
    
    tabPanel("nextflow-bcl pipeline",
            # attempts to use external progress bar
            includeCSS("css/customProgress.css"),
            useShinyjs(),
            useShinyalert(),
            shiny::uiOutput("mqc_report_button", inline = TRUE),
            shiny::div(id = "commands_pannel",
              shinyDirButton(id = "bcl_folder", 
                             label = "Select BCL run folder", 
                             title = "Please select an Illumina run folder"),
              
              actionButton("run", "Run nextflow-bcl pipeline", 
                         style = "color: green; font-weight: bold;", 
                         onMouseOver = "this.style.color = 'orange' ", 
                         onMouseOut = "this.style.color = 'green' "),
              
              actionButton("reset", "Reset", 
                         style = "color: green; font-weight: bold;",
                         onMouseOver = "this.style.color = 'orange' ",
                         onMouseOut = "this.style.color = 'green' "),
              
              shinyFilesButton(id = "samplesheet", 
                              label = "Select sample sheet", 
                              title = "Please select a sample sheet", multiple = FALSE, 
                              class = "rightAlign")
              
            ),
            
            verbatimTextOutput("stdout") # this element is fixed height in the css file
            
    ),
    tabPanel("Help", 
             includeMarkdown("help.md")
             )
    
  )
 }
 #### server ####
  server <- function(input, output, session) {
    options(shiny.launch.browser = TRUE, shiny.error=recover)
    
    # generate random hash for multiqc report temp file name
    mqc_hash <- sprintf("%s_%s.html", as.integer(Sys.time()), digest::digest(runif(1)) )
    
    # dir choose and file choose management --------------------------------------
    volumes <- c(Home = fs::path_home(), getVolumes()() )
    shinyDirChoose(input, "bcl_folder", 
                   roots = volumes, 
                   session = session, 
                   restrictions = system.file(package = "base"))
    shinyFileChoose(input, "samplesheet", 
                    roots = volumes, 
                    session = session)
    
    #-----------------------------------
    # show currently selected bcl folder and sample sheet file
    # in case no active selection is made, look in the bcl folder
    # else take the selected sample sheet
    
    output$stdout <- renderPrint({
      if (is.integer(input$bcl_folder)) {
        cat("No Illumina run folder selected\n")
      } else {
        if (is.integer(input$samplesheet)) { 
          sh_selected <<- list.files(path = parseDirPath(volumes, input$bcl_folder), pattern = "SampleSheet.csv", full.names = TRUE)
        } else {
          sh_selected <<- parseFilePaths(volumes, input$samplesheet)$datapath  
          }
        cat(
          " Currently selected run folder:\n", 
          parseDirPath(volumes, input$bcl_folder), "\n\n",
          "Currently selected sample sheet:\n", 
          sh_selected
        )
        
       }
    })

    #
    # real call to nextflow-bcl
    #-----------      
    # using processx to better control stdout
    observeEvent(input$run, {
      if(is.integer(input$bcl_folder)) {
        shinyjs::html(id = "stdout", "\nPlease select an Illumina run folder first, then press 'Run'...\n", add = TRUE)
      } else {
        # set run button color to red?
        shinyjs::disable(id = "commands_pannel")
        # change label during run
        shinyjs::html(id = "run", html = "Running... please wait")
        
      # define wd to runfolder
      wd <- parseDirPath(volumes, input$bcl_folder)
        
      #--------------------------------------------  
      # Dean Attali's solution
      # https://stackoverflow.com/a/30490698/8040734
        withCallingHandlers({
          shinyjs::html(id = "stdout", "")
          p <- processx::run("nextflow", 
                      args = c("run" ,
                               "angelovangel/nextflow-bcl", # pulls and puts it in ~/.nextflow
                               #fs::path_abs("../nextflow-bcl/main.nf"), # absolute path to avoid pulling from github
                               "--runfolder", wd, 
                               "--samplesheet", sh_selected),
                      
                      wd = wd,
                      #echo_cmd = TRUE, echo = TRUE,
                      stdout_line_callback = function(line, proc) {message(line)}, 
                      #stdout_callback = cb_count,
                      stderr_to_stdout = TRUE, 
                      error_on_status = FALSE
                      )
          }, 
            message = function(m) {
              shinyjs::html(id = "stdout", html = m$message, add = TRUE); 
              runjs("document.getElementById('stdout').scrollTo(0,1e9);") # scroll the page to bottom with each message, 1e9 is just a big number
            }
        )
      #-------------------------------
        if(p$status == 0) {
          
          # clean work dir in case run finished ok
          work_dir <- paste(wd, "/work", sep = "")
          system2("rm", args = c("-rf", work_dir) )
          cat("deleted", work_dir)
          
          # copy mqc to www/ to be able to open it, also use hash to enable multiple concurrent users
          
          mqc_report <- paste(wd, "/results-bcl/multiqc_report.html", # make sure the nextflow-bcl pipeline writes to "results-bcl"
                           sep = "")
           
          system2("cp", args = c(mqc_report, paste("www/", mqc_hash, sep = "")) )
          
          # OK alert
          #shinyjs::enable(id = "commands_pannel") # make action buttons active again
          shinyjs::hide(id = "commands_pannel")
          shinyjs::show(id = "reset")
          # render the new action button to show mqc report
          output$mqc_report_button <- renderUI({
            actionButton("mqc", label = "MultiQC report", 
                         icon = icon("th"), 
                         onclick = sprintf("window.open('%s', '_blank')", mqc_hash)
                         )
          })
          # the next changes the text and function of the run button
          #shinyjs::html(id = "run", 
          #              html = sprintf("<a href='%s' target='_blank'>Show MultiQC report</a>", mqc_hash) )
          #
          # build js callback string for shinyalert
          js_cb_string <- sprintf("function(x) { if (x == true) {window.open('%s') ;} } ", mqc_hash)
          
          shinyalert("Run finished!", type = "success", 
                   animation = "slide-from-bottom",
                   text = "Pipeline finished, check results folder", 
                   showCancelButton = TRUE, 
                   confirmButtonText = "Open report",
                   callbackJS = js_cb_string, 
                   #callbackR = function(x) { js$openmqc(mqc_url) }
                   )
        } else {
          shinyjs::enable(id = "commands_pannel")
          shinyjs::html(id = "run", html = "Finished with errors, please reset")
          shinyjs::disable(id = "run")
          
          shinyalert("Error!", type = "error", 
                     animation = "slide-from-bottom", 
                     text = "Pipeline finished with errors, press OK to reload the app and try again.", 
                     showCancelButton = TRUE, 
                     callbackJS = "function(x) { if (x == true) {history.go(0);} }"
                     )
        }
      }
      
    })
    
    #------------------------------------------------------------
    session$onSessionEnded(function() {
      # delete own mqc from www, it is meant to be temp only 
      system2("rm", args = c("-rf", paste("www/", mqc_hash, sep = "")) )
      })
  
  #---
  # ask to start over if title or reset clicked
  #----                     
  observeEvent(input$bclButton, {
    shinyalert(title = "",
               type = "warning",
               text = "Restart app?", 
               html = TRUE, 
               confirmButtonText = "Start again", 
               showCancelButton = TRUE, 
               callbackJS = "function(x) { if (x == true) {history.go(0);} }" # restart app by reloading page
               )
  })
  observeEvent(input$reset, {
    shinyalert(title = "",
               type = "warning",
               text = "Restart app?", 
               html = TRUE, 
               confirmButtonText = "Start again", 
               showCancelButton = TRUE, 
               callbackJS = "function(x) { if (x == true) {history.go(0);} }" # restart app by reloading page
      )
    })
   
    
 }
 
 shinyApp(ui, server)
 