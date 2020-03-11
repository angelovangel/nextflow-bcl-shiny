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
 library(yaml) # for generating custom multiqc_config.yaml from modal inputs
 library(loggit)
 
 source("ncct_make_yaml.R") # I should use modules to handle modal inputs here...
 
 loggit::setLogFile(paste0(getwd(), "/loggit.json"))
 users <- reactiveValues(count = 0)

 #### ui ####
 ui <- function(x) {
   
   navbarPage(title = tags$button("nextflow-bcl",
                               id = "bclButton", # events can be listened to as input$bclButton in server
                               class = "action-button", #shiny needs this class def to work
                               title = "If you want to start over, just reload the page.",
                               onMouseOver = "this.style.color='orange'", # old school js
                               onMouseOut = "this.style.color='#3498DB'",
                               style = "color: #3498DB; font-weight: bold; border: none; background-color: inherit;"),
             
              windowTitle = "nextflow-bcl", 
              collapsible = TRUE,
    
    tabPanel("nextflow-bcl pipeline",
            # attempts to use external progress bar
            includeCSS("css/customProgress.css"),
            useShinyjs(),
            useShinyalert(),
            shiny::uiOutput("mqc_report_button", inline = TRUE),
            shiny::uiOutput("nxf_report_button", inline = TRUE),
            shiny::uiOutput("bcl_log_button", inline = TRUE),
            
            shiny::div(id = "commands_pannel", 
              shinyDirButton(id = "bcl_folder", 
                             label = "Select BCL run folder", 
                             title = "Please select an Illumina run folder"),
              
              actionButton("run", "Run nextflow-bcl pipeline", 
                         style = "color: #3498DB; font-weight: bold;", 
                         onMouseOver = "this.style.color = 'orange' ", 
                         onMouseOut = "this.style.color = '#3498DB' "),
              
              actionButton("reset", "Reset", 
                         style = "color: #3498DB; font-weight: bold;",
                         onMouseOver = "this.style.color = 'orange' ",
                         onMouseOut = "this.style.color = '#3498DB' "),
              # more options, like 
              actionButton("more", "More options", class = "rightAlign"),
              absolutePanel(top = 140, right = 20,
                textInput(inputId = "report_title", 
                          label = "Title of MultiQC report", 
                          value = "InterOp and bcl2fastq summary"),
                tags$hr(),
                checkboxInput("tower", "Use Nextflow Tower to monitor run", value = FALSE),
                tags$hr(),
                shinyFilesButton(id = "samplesheet", 
                                 label = "Select sample sheet", 
                                 title = "Please select a sample sheet", 
                                 multiple = FALSE), 
                tags$hr(),
                shinyDirButton(id = "results_dir", 
                             label = "Custom results output folder", 
                             title = "Select a folder to save fastq results"),
                tags$hr(),
                actionButton("ncct", "Enter NCCT project info")
              )
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
    
    # update user counts at each server call
    isolate({
      users$count <- users$count + 1
    })
    
    # observe changes in users$count and write to log
    observe({
      loggit("INFO", log_msg = "current_users", log_detail = paste(users$count))
    })
    
    options(shiny.launch.browser = TRUE, shiny.error=recover)
    #----
    # observer for optional inputs
    hide("report_title")
    hide("samplesheet")
    hide("results_dir")
    hide("ncct")
    hide("tower")
    observeEvent(input$more, {
      shinyjs::toggle("report_title")
      shinyjs::toggle("samplesheet")
      shinyjs::toggle("results_dir")
      shinyjs::toggle("ncct")
      shinyjs::toggle("tower")
    })
    
    #----
    # generate random hashes for multiqc report temp file name etc.
    mqc_hash <- sprintf("%s_%s.html", as.integer(Sys.time()), digest::digest(runif(1)) )
    nxf_hash <- sprintf("%s_%s.html", as.integer(Sys.time()), digest::digest(runif(1)) )
    bcllog_hash <- sprintf("%s_%s.txt", as.integer(Sys.time()), digest::digest(runif(1)) )
    
    # reactives for optional nxf params
    # set TOWER_ACCESS_TOKEN in ~/.Renviron
    optional_params <- reactiveValues(mqc = "", tower = "")
    
    # dir choose and file choose management --------------------------------------
    volumes <- c(Home = fs::path_home(), getVolumes()() )
    shinyDirChoose(input, "bcl_folder", 
                   roots = volumes, 
                   session = session, 
                   restrictions = system.file(package = "base"))
    shinyFileChoose(input, "samplesheet",
                    roots = volumes, 
                    session = session, 
                    filetypes = 'csv')
    shinyDirChoose(input, "results_dir", 
                   roots = volumes, 
                   session = session)
    
    #-----------------------------------
    # SET WORK DIR, SAMPLESHEET, OUTDIR, 
    # show currently selected bcl folder and sample sheet file
    # in case no active selection is made, look in the bcl folder
    # else take the selected sample sheet
    
    output$stdout <- renderPrint({
      if (is.integer(input$bcl_folder)) {
        cat("No Illumina run folder selected\n")
      } else {
        # define wd to runfolder
        wd <<- parseDirPath(volumes, input$bcl_folder) # hard assigns required in render? 
        # auto-fill sample sheet
        if (is.integer(input$samplesheet)) { 
          sh_selected <<- list.files(path = wd, pattern = "SampleSheet.csv", full.names = TRUE)
        } else {
          sh_selected <<- parseFilePaths(volumes, input$samplesheet)$datapath # parseFilePaths returns a df! 
        }
        
        # set outdir
        if(is.integer(input$results_dir)) {
          outdir <<- paste(wd, "/results-bcl", sep = "")
        } else {
          outdir <<- parseDirPath(volumes, input$results_dir)
        }
        
        # set tower optional
        if(input$tower) {
          optional_params$tower <- "-with-tower"
        } else {
          optional_params$tower <- ""
        }
        
        # and write current selection to stdout
        cat(
          " 1. Currently selected Illumina run folder:\n", 
          wd, "\n\n",
          "2. Currently selected pipeline output folder: \n", 
          outdir, "\n\n",
          "3. Currently selected sample sheet:\n", 
          sh_selected, "\n\n",
          "--------------------------------\n",
          "If there are selections for all three above you can start the run. \n",
          "The current mqc config reactive val is:\n", 
          optional_params$mqc, "\n\n",
          
          "--------------------------------\n",
          "Nextflow command to be executed:\n",
          "nextflow run angelovangel/nextflow-bcl", "\\ \n",
          "--runfolder", wd, "\\ \n",
          "--outdir", outdir, "\\ \n",
          "--samplesheet", sh_selected, "\\ \n",
          "--title", input$report_title, "\\ \n",
          "-with-report", paste(outdir, "/nxf_workflow_report.html", sep = ""), "\\ \n",
          optional_params$tower, optional_params$mqc, "\n"
        )
        
       }
    })
    
    #----
    # strategy for ncct modal and multiqc config file handling:
    # if input$ncct_ok is clicked, the modal inputs are fed into the ncct_make_yaml() function, which generates
    # a multiqc_config.yml file and saves it using tempfile()
    # initially, the reactive value mqc_config$rv is set to "", if input$ncct_ok then it is set to
    # c("--multiqc_config", mqc_config_temp) and this reactive is given as param to the nxf pipeline
    
    # observer for ncct modal
    source("ncct_modal.R", local = TRUE)$value
    observeEvent(input$ncct, {
      showModal(ncct_modal())
    })
    
    # generate yml file in case OK of modal was pressed
    # the yml file is generated in the app exec env, using temp()
    observeEvent(input$ncct_ok, {
      mqc_config_temp <- tempfile()
      optional_params$mqc <- c("--multiqc_config", mqc_config_temp) 
      ncct_make_yaml(customer = input$customer, 
                     project_id = input$project_id, 
                     ncct_contact = input$ncct_contact, 
                     project_type = input$project_type, 
                     lib_prep = input$lib_prep, 
                     indexing = input$indexing, 
                     seq_setup = input$seq_setup, 
                     ymlfile = mqc_config_temp)
      shinyalert(text = "Project info saved!", type = "info", timer = 1500, showConfirmButton = FALSE)
      removeModal()
    })
    
    
    #----
    # progress definition
    progress <- shiny::Progress$new(min = 0, max = 1, style = "old")
  
    cb <- function(chunk, process) {
      counts <- str_count(chunk, pattern = "bcl2fastq")
      progress$inc(amount = counts/6)
    }
    
    #
    # real call to nextflow-bcl
    #-----------      
    # using processx to better control stdout
    observeEvent(input$run, {
      if(is.integer(input$bcl_folder)) {
        shinyjs::html(id = "stdout", 
                      "\nPlease make sure that Illumina run folder and sample sheet are selected, then press 'Run'...\n", 
                      add = TRUE)
      } else {
        # set stuff when run starts
        shinyjs::disable(id = "commands_pannel")
        shinyjs::disable(id = "bclButton")
        # change label during run
        shinyjs::html(id = "run", html = "Running...")
        progress$set(message = "Running...  please wait", value = 0)
        on.exit(progress$close())
        
        
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
                               "--outdir", outdir,
                               "--samplesheet", sh_selected, 
                               "--title", input$report_title, 
                               optional_params$mqc,  # here is the mqc_config trick - pass either "" or "--multiqc_config ..."
                               optional_params$tower, 
                               "-with-report", paste(outdir, "/nxf_workflow_report.html", sep = "")
                               ),
                      
                      wd = wd,
                      #echo_cmd = TRUE, echo = TRUE,
                      stdout_line_callback = function(line, proc) {message(line)}, 
                      stdout_callback = cb,
                      stderr_to_stdout = TRUE, 
                      error_on_status = FALSE
                      )
          }, 
            message = function(m) {
              shinyjs::html(id = "stdout", html = m$message, add = TRUE); 
              runjs("document.getElementById('stdout').scrollTo(0,1e9);") 
              # scroll the page to bottom with each message, 1e9 is just a big number
            }
        )
      #-------------------------------
        if(p$status == 0) {
          
          # clean work dir in case run finished ok
          work_dir <- paste(wd, "/work", sep = "")
          system2("rm", args = c("-rf", work_dir) )
          cat("deleted", work_dir)
          
          # copy mqc to www/ to be able to open it, also use hash to enable multiple concurrent users
          # make sure the nextflow-bcl pipeline writes to outdir
          mqc_report <- paste(outdir, "/multiqc_report.html", sep = "")
          nxf_report <- paste(outdir, "/nxf_workflow_report.html", sep = "")
          bcl_log <- paste(outdir, "/bcl_out.log", sep = "")
           
          system2("cp", args = c(mqc_report, paste("www/", mqc_hash, sep = "")) )
          system2("cp", args = c(nxf_report, paste("www/", nxf_hash, sep = "")) )
          system2("cp", args = c(bcl_log, paste("www/", bcllog_hash, sep = "")) )
          
          # OK alert
          shinyjs::hide(id = "commands_pannel")
          shinyjs::enable(id = "bclButton")
          
          # render the new action buttons to show reports
          output$mqc_report_button <- renderUI({
            actionButton("mqc", label = "MultiQC report", 
                         icon = icon("th"), 
                         onclick = sprintf("window.open('%s', '_blank')", mqc_hash)
                         )
          })
          
          output$nxf_report_button <- renderUI({
            actionButton("nxf", label = "Nextflow execution report", 
                         icon = icon("th"), 
                         onclick = sprintf("window.open('%s', '_blank')", nxf_hash)
            )
          })
          
          output$bcl_log_button <- renderUI({
            actionButton("bcl", label = "bcl2fastq log file", 
                         icon = icon("th"), 
                         onclick = sprintf("window.open('%s', '_blank')", bcllog_hash)
            )
          })
          
          
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
          shinyjs::enable(id = "bclButton")
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
      #user management
      isolate({
        users$count <- users$count - 1
      })
      
      loggit::loggit(log_lvl = "INFO", "app_stop", log_detail = "0")
    
      # delete own mqc from www, it is meant to be temp only 
      system2("rm", args = c("-rf", paste("www/", mqc_hash, sep = "")) )
      system2("rm", args = c("-rf", paste("www/", nxf_hash, sep = "")) )
      system2("rm", args = c("-rf", paste("www/", bcllog_hash, sep = "")) )
      #stopApp() reset doesn't work if this is active - reload app stops it
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
 