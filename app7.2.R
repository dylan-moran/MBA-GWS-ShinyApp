
# Dylan Moran
# Removes dropbox capabilities and reads/writes to local device
# dmoran@csumb.edu



source("fin_shiny_fxns7.2_DM.R")                                  # changed to 7.2
appCSS <- ".mandatory_star { color: red; }"






####
#APP
shinyApp(ui = navbarPage(
  id = "form",
  title = "FinID Data Entry",
  
  
  ###############
  ##SURVEY INFO##
  ###############
  
  tabPanel("Survey Info",
           sidebarPanel(
             #Survey inputs
             #file upload
             radioButtons("user", labelMandatory("User"), 
                          choices = flds$users, inline = T, selected=character(0)),
             selectInput("site.phid", labelMandatory("Survey site"),
                         choices = flds$sites),
             dateInput("date.phid", labelMandatory("Survey date"), 
                       value = Sys.Date(), format = "yyyy-mm-dd"),
             hr(),
             checkboxGroupInput("crew", "Vessel Crew", 
                                choices = flds$observers, inline = T),
             textInput("vessel", "Vessel Name", placeholder = "Flatline | RV Outrage")
           ),
           
           mainPanel(
             #outputs for tab 1
             # sliderInput("effort.on", "On Effort?", 
             #             value = 8, min = 4, max = 20, step = 0.5),
             # sliderInput("effort.off", "Off Effort?",
             #             value = 15, min = 4, 
             #             max = 20, step = 0.5),
             sliderInput("effort", "Start-Stop Time of Survey Effort", 
                         min = 6, max = 20, value = c(8, 15)),
             textInput("notes.survey", "Notes from survey day", 
                       placeholder = "breaches? predations? see a mermaid?", width = "100%"),
             textOutput("crew"),
             actionButton("addfins", "Ready to add Fins?", class="btn-primary")
           )
  ),
  
  
  
  ################
  ##FIN ID ENTRY##
  ################
  
  tabPanel("Fin Photo Entry",
           shinyjs::inlineCSS(appCSS),
           
           sidebarPanel(
             
             #PhotoID fields first
             #selectInput("site.phid", labelMandatory("Monitoring Site"), selected = uiOutput("site.survey"),
             #            choices = flds$sites),
             #dateInput("date.phid", labelMandatory("Date"), value = NULL, format = "yyyy-mm-dd"),
             # uiOutput("site.phid"),
             # uiOutput("date.phid"),
             numericInput("sighting.phid", labelMandatory("Sighting #"), value = NULL, 
                          min = 01, max = 99, step =1), #MAKE THIS NUMERIC?
             hr(),
             fileInput("fin.photo", labelMandatory("Upload fin here"), 
                       multiple = F, accept=c("image/jpeg", "image/png", "image/tiff",
                                              ".jpeg", ".jpg", ".png", ".tiff")),
             textInput("time", labelMandatory("Time of Observation"), placeholder = "24HR CLOCK PLS (e.g., 0915 for 9:15)"),
             hr(),
             selectInput("sex", labelMandatory("Sex (U if unknown)"), choices = c("M", "F", "U"), 
                         selectize = F, selected = "U"), 
             numericInput("size", labelMandatory("Size (in ft)"), value = NULL, min = 4, max = 20, step = 0.5),
             hr(), #break in UI
             conditionalPanel(   #collect data once fin is uploaded & photoID is correct       
               condition = "output.finuploaded",
               textInput("notes", "Notes", placeholder = "e.g. pings heard, secondary marks, scars, nicknames, etc", 
                         width = "600px"),
               selectInput("tag.exists", "Tagged Already?", choices = c("U", "Y"), selected = "U"),
               selectInput("tagdeployed", "New Tag?", choices = c("None", "PAT", "Acoustic", "Stomach", "Clamp"), selected = "None"),
               #make ^ check box and conditional panel for each tag selected?
               conditionalPanel(
                 condition = "input.tagdeployed != 'None'",
                 radioButtons("tag.side", "Deployed On? ", choices = c("NA", "L", "R"), 
                              inline = T), 
                 textInput("tag.id", "Tag ID#"),
                 textInput("tag.notes", "Tagging Notes", width = '600px', 
                           placeholder = "e.g., programming params, Ptt/SPOT used, orientation"),
                 selectInput("biopsy", "Biopsy?", choices = c("N", "Y"), selected="N"),
                 conditionalPanel(
                   condition = "input.biospy != 'N'",
                   textInput("biopsy.id", "Vial Number?")
                 )
               )
             )
             
             
           ), 
           
           mainPanel(useShinyjs(),
                     uiOutput("site.phid"),
                     uiOutput("date.phid"),
                     uiOutput("PhotoID"),
                     hr(),
                     conditionalPanel("output.finuploaded",
                                      #DT::dataTableOutput("dataentry"),
                                      fluidRow(
                                                column(width = 5, 
                                                       textInput("match.sugg", 
                                                                 "FinID Suggestion for MatchMaker", 
                                                                 placeholder = "That you Snowflake? Ornament? Burns?", 
                                                                 width = "auto"),
                                                       imageOutput(outputId = "FinShot", 
                                                       width = "auto",
                                                       height = "auto")),
                                                column(width = 5,
                                                       leafletOutput("map", height = 400, width = 400),
                                                       uiOutput("xyloc"),
                                                       actionButton("resetlatlong", "Reset Lat/Long", class = "btn-primary")
                                                       )
                                               ),
                                      hr(),
                                      conditionalPanel("mandatoryFilled",
                                                       actionButton("saveEncounter", "Save Encounter", class="btn-primary"),
                                                       actionButton("r2submit", "Ready To Review Data?", class="btn-primary")
                                      )
                     )
           )
           
  ),
  
  
  
  ###################
  ##DATA SUBMISSION##
  ###################
  
  tabPanel("Data Review",
           # fluidRow(
           #   verbatimTextOutput('x4')
           # ),
           fluidPage(
#             sidebarPanel(
#               selectInput("location.filt", "Location Filter", choices = c("None", "AN", "FAR", "APT", "PR"), selected=" ") #,
#              dateInput("date.filt", "Date Filter", 
#                        value = Sys.Date(), format = "yyyy-mm-dd"),
#               ),
#             useShinyalert(), 
             textOutput("UserEmpty"),                                   # reminds that User must be filled in for submission to work
             tags$head(tags$style("#UserEmpty{color: red; 
                                  font-size: 20px; 
                                  font-style: bold;}")),                # user empty text is red
             sidebarLayout(
                       sidebarPanel(width = 3,
                                   textOutput("NoSelectedFinTxt"),
                                      tags$head(tags$style("#NoSelectedFinTxt{
                                                           color: grey; 
                                                           font-size: 20px; 
                                                           font-style: bold;}")),
                                   imageOutput("SelectedFinImg"),
                                   textOutput("SelectedFinOrigImgName"),
                                     tags$head(tags$style("#SelectedFinOrigImgName{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                                   hr(),
                                   textOutput("SelectedFinImgName"),
                                      tags$head(tags$style("#SelectedFinImgName{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                                   hr(),
                                   uiOutput("btn2")
                                   ),
               mainPanel(width = 8,
                         actionButton("resetedits_btn", "Reset All Edits", 
                                      class = "btn-primary"),
                         # data review table
                         DT::dataTableOutput("finsTable")
                      )
             ),

             # satsfied with data?
             checkboxInput("reviewed", 
                label = "I HAVE VERIFIED THE DATA", 
                value = F),

             #reveal button if data is reviewed
             uiOutput("reviewed")
           )
  )
),

###################################################################################################
################################           SERVER           #######################################
###################################################################################################

server = function(input, output, session) {
  #enlarge maximum upload size 
  options(shiny.maxRequestSize=30*1024^2)
  
  ######################
  ######################
  ##Page 1 server stuff
  #update phids from survey
  # observeEvent(input$survey,{
  #   updateSelectInput(session, "site.phid", selected = input$survey)
  # }) 
  # observeEvent(input$date.survey,{
  #   updateDateInput(session, "date.phid", value = input$date.survey)
  # })
  output$site.phid = renderText({paste("<font color=\"#FF0000\"><b>SURVEY SITE<font color=\"#000000\"></b> assigned as: ", 
                                       HTML(paste0("<font color=\"#FF0000\"><b>", input$site.phid, "<b>")))})
  output$date.phid = renderText({paste(HTML("<font color=\"#FF0000\"><b>SURVEY DATE<font color=\"#000000\"></b> assigned as: "), 
                                       HTML(paste0("<font color=\"#FF0000\"><b>",
                                                   as.Date(input$date.phid, format = "%m-%d-%Y"),
                                                   "</b>")))})
  
  
  #make off effort depend on effort (can't be before on effort)
  # observe(input$effort.off <= input$effort.on,{
  #   updateSliderInput(session, "effort.off", value = input$effort.on + 0.5)
  # })
  
  

  # ######################
  # ##Page 2 server stuff
  # ######################


  # ######################
  # #data gatekeeper, fields can be entered once PhotoID & file exists
  # #data can't be pressed into a csv file until conditions are met
  # ######################
  # #ALL THINGS PHOTO ID STAMP HAPPEN HERE
  # ######################
  
  
   phid <- reactiveValues()
  # phid$exists <- tools::file_path_sans_ext(
  #   getExistingPhids(all = T, paths = F))
  
   finUP <- reactive({
     if(is.null(input$fin.photo)){
       return(NULL)
     }else{
       #make fin photo
       cat("the datapath currently is", file.exists(input$fin.photo$datapath), "\n")
       re1 <- reactive({gsub("\\\\","/", input$fin.photo$datapath)})
       output$FinShot <- renderImage({list(src = re1(), 
                                           width = "auto", 
                                           height = "300")}, 
                                           deleteFile = FALSE)
  
       #make id, check dupes
       #make a dummy
       phid$site <- input$site.phid
       phid$date <- input$date.phid
       phid$dummy <- paste0(toupper(input$site.phid),
                            format(input$date.phid, "%y%m%d"),
                            ifelse(nchar(input$sighting.phid==1),
                                   paste0("0", input$sighting.phid),
                                   input$sighting.phid))
       #check the dummy against dupes, PRESS it
       phid$val <- ifelse(phid$dummy %in% phid$exists, NA, phid$dummy)
  
       #press the phid to an output obj
       output$PhotoID <- renderText({
         #check dupes
         validate(
           need(!is.na(phid$val), "PHOTOID ALREADY EXISTS!!!!!!! MAKE A CHANGE")
         )
         paste(HTML("<font color=\"#FF0000\"><b>PHOTO ID<font color=\"#000000\"></b> assigned as: "),
               HTML(paste0("<font color=\"#FF0000\"><b>",
                           phid$val,
                           "</b>")))
       })


        output$PhotoID <- renderText({paste0(toupper(input$site.phid),                    # text displayed naming photoID
                                             format(input$date.phid, "%y%m%d"),
                                             ifelse(nchar(input$sighting.phid==1),
                                                    paste0("0", input$sighting.phid),
                                                    input$sighting.phid))})
  
  
     #make site map for leaflet input
       ctr <- flds$coords[[input$site.phid]]
       output$map <- renderLeaflet({
         leaflet() %>%
           addTiles() %>%                                                        # use basemap tile
           #addProviderTiles(providers$Stamen.TonerLite,                         # provider tiles acting weird.
           #                options = providerTileOptions(noWrap=T)) %>%
           #addMarkers(data = matrix(c(-123.01, 37.69), nrow=1))
           setView(lng=ctr[1,1], lat = ctr[1,2], zoom = 13)
       })
  
  
       observeEvent(input$map_click,{
         #capture click
         click <- input$map_click
         phid$lat <- click$lat
         phid$long <- click$lng
         leafletProxy('map') %>%
           clearMarkers() %>%
           addPulseMarkers(data = click, lng=~lng, lat=~lat, icon = makePulseIcon(),
                           options = leaflet::markerOptions(draggable = F))
  
         output$xyloc <- renderText({paste("lat: ", round(click$lat, 4),
                                           "| long: ", round(click$lng, 4))})
       })
       
       # observeEvent(input$resetlatlong,{
       #   leafletProxy('map') %>%
       #     clearMarkers()
       #   phid$lat   <- NULL
       #   phid$long  <- NULL
       #   output$xyloc <- NULL
       # })
       #return the photoID to splash around
       return(phid)
     }
   })
  
  
  
   #####################
  
   output$finuploaded <- reactive({
     #hide the data entry tbl until essential info is included
     return(!is.null(finUP()) && !is.na(phid$val) && grepl("^([A-Z]{2,3})([0-9]{8})", phid$val))
   })
   outputOptions(output, 'finuploaded', suspendWhenHidden=F)
  
   
   
   
   output$dataentry <- DT::renderDataTable(
     formData(),
     rownames = FALSE,
     options = list(searching = FALSE, lengthChange = FALSE,
                    columnDefs=list(
                      list(visible = F, targets = c(14:17))))
   )
  
   observeEvent("$('#dataentry').hasClass('recalculating')",{
     shinyalert(title = "Fetching data", text = "Please wait for data to download",
                closeOnEsc = T, closeOnClickOutside = T, timer = 5)
   })
  
  
   #submit buttons only if fields are filled, theres a photo, & proper photoID
   observe({
     mandatoryFilled <- vapply(flds$mandatory,
                               function(x) {
                                 !is.null(input[[x]]) && input[[x]] != "" && !is.null(finUP())
                               },
                               logical(1))
  
     mandatoryFilled <- all(mandatoryFilled)
  
      #shinyjs::toggleState(id = "saveEncounter", condition = mandatoryFilled)
      #shinyjs::toggleState(id = "r2submit", condition = mandatoryFilled)
   })
  
   
   
  ##########################################
  ##########################################
  ##Page 3 server stuff
  ##########################################
   
   
  output$reviewed <- renderUI({
    if(input$reviewed == T){
      actionButton("SAVEDATA", "SUBMIT & STORE", class="btn-primary")}
  })
  

  
  ######################
  ######################
  ##Data making stuff
  
  
  formData <- reactive({
    #save photo
    if(is.null(finUP)){
      return(NULL)
      ##SOME WARNING DATA WILL NOT BE SAVED W/O A PHOTO FILE
    }
    else{
      
      data <- c(refID = "UNMATCHED", name = "NONE_YET", 
                match.sugg = as.character(input$match.sugg), 
                time.obs = as.character(input$time),
                PhotoID = as.character(phid$val),
                site = toupper(as.character(input$site.phid)), 
                date = as.character(input$date.phid), 
                sighting = as.character(input$sighting.phid),
                sex = as.character(input$sex),
                size = as.character(round(input$size/0.5)*0.5),
                tag.exists = as.character(input$tag.exists),
                tag.deployed = as.character(input$tagdeployed),
                tag.id = as.character(input$tag.id),
                tag.side = as.character(input$tag.side),
                biopsy = as.character(input$biopsy),
                biopsy.id = as.character(input$biopsy.id),
                notes = as.character(input$notes),
                tagging.notes = as.character(input$tag.notes),
                user = as.character(input$user),
                lat.approx = as.character(round(as.numeric(phid$lat), 4)),
                long.approx = as.character(round(as.numeric(phid$long), 4)),
                timestamp = epochTime(), 
                #one row, one entry, one photo
                dfN = file.path(paste0("CCA_GWS_PHID_", 
                                       #photoID
                                       # toupper(input$site.phid),
                                       # format(input$date.phid, "%y%m%d"), 
                                       # ifelse(nchar(input$sighting.phid==1), 
                                       #       paste0("0", input$sighting.phid),
                                       #       input$sighting.phid), 
                                       phid$val, "_",
                                       #timestamp
                                       as.integer(Sys.time()),
                                       #extension
                                       ".csv")),
                #photo file
                pFn = file.path(dropfin, paste0(phid$val, 
                                                ".", 
                                                tools::file_ext(input$fin.photo$datapath))),
                orig.fin.file = as.character(input$fin.photo$name),
                survey.vessel = as.character(input$vessel),
                survey.crew = as.character(paste(input$crew, collapse = "|")),
                # survey.effortON = as.character(input$effort.on), #w/o slider range
                # survey.effortOFF = as.character(input$effort.off),
                survey.effortON = as.character(input$effort[1]), #w/ slider range
                survey.effortOFF = as.character(input$effort[2]),
                survey.notes = as.character(input$survey.notes)
      )
      data <- t(data)
      data
    }
  })
  
  output$FinPhotoName <-
    renderText({
      input$fin.photo$name
    })

   ######################
   ######################
   ##Button doing stuff
   #Find surveys available to submit & populate via unique dates/locs
   #produce the selectInput for surveys available
   
   

   
   
   
   survey.avail <- reactive({        # i think this can be deleted
     EncounterData<- loadData2()
     staged <- unique(EncounterData$PhotoID)
     return(staged)
   })
   
   
   
   output$for.review <- renderUI({      # i think this can be deleted?
     selectInput("for.review","Surveys available for review/submission",
                 choices = survey.avail(), selected = NULL)
   })
   
   
   
   
   
   
   
   
   
   #################################################
   ######### Encounter Data Table Review ###########
   #################################################
   
    start_table <- loadData2Table()

    this_table <- reactiveVal(start_table)

    
    observeEvent(input$resetedits_btn, {                    # reloads fintable to default
       refreshed_finsTable <- loadData2Table()
       this_table(refreshed_finsTable)
    })

    observeEvent(input$finsTable_cell_edit, {
      editinput <- input$finsTable_cell_edit                # Dear whoever works on this next... 
      editinput = data.frame(editinput)                     # this chunk made me contemplate  
      #print(editinput)                                     # my entire life as a "biologist"
      proxytable = this_table()                                      # ...
           as.data.frame(proxytable)                                 # this code is written in blood and tears
      proxytable[editinput$row,editinput$col] <- editinput$value
      this_table(proxytable)
    })
    
    output$btn2 <- renderUI({
      if(!is.null(input$finsTable_rows_selected)){
        actionButton("delete_btn", "Delete Selected Row", 
                     class = "btn-primary",
                     style = "color: white; 
                     background-color: #EA5332")
      }
    })
    
    observeEvent(input$delete_btn, {
      proxytable = this_table()
      if (!is.null(input$finsTable_rows_selected)) {
        proxytable <- proxytable[-as.numeric(input$finsTable_rows_selected),]
      }
      this_table(proxytable)
    })
   
   
   
    output$finsTable <- 
      DT::renderDataTable(     # table needs to be refreshed going into the page. 
      this_table(),                              # I made it refresh when the r2submit btn is hit
      editable = TRUE,                           # but it needs a refresh if you go to hit the tab
      selection = 'single',
      rownames = TRUE,
      options = list(
        scrollX = TRUE,
        scrollY = "500px",
        server = F,
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        class = "display")
    )
    
    
    output$UserEmpty <-
      renderText(
        if(is.null(input$user)){
          "User is not defined. See 'Survey Info' tab"
        }
      )
    
    observeEvent(input$SAVEDATA, {
      #view(this_table())
      FINput_filename = paste0(
                        "FINput_", 
                        input$user,                               # Stamps with user's initials
                        "_",
                        format(Sys.time(), "%Y%m%d"),       #yyyymmdd date format saved in system's TZ
                        "_",                                
                        format(Sys.time(), "%H%M%S"),       #hhmmss   time format saved in system's TZ
                        ".csv")                             #           (hours are in 24hr format)
      write.csv(this_table(), 
                file = paste0(
                  encounterfiles_FINAL, "/",FINput_filename
                ))
    })
   
##################################################
   
   
   
   
  observeEvent(input$addfins, {
    updateTabsetPanel(session, "form", selected = "Fin Photo Entry")
  })
  
  
    
  #Observe "saveEncounter" event here
  observeEvent(input$saveEncounter, {
    data <- data.frame(formData(), stringsAsFactors = F)
    showNotification(paste("DataCSV", data$dfN, "is uploaded to", dropsc), 
                     #action = a(href="javascript:location.reload();", "Reload page"),
                     closeButton = F, type = "message", duration=9,
                     id = "datUP")
    saveData2(data)
    savePhoto2(input$fin.photo, phid$val)
    
    showNotification(paste("FinPhoto", phid$val, "uploaded to", dropfin), 
                     #action = a(href="javascript:location.reload();", "Reload page"), 
                     closeButton = F, type = "message", duration=9,
                     id = "phidUP")
    
    #reset fields
    sapply(c("sighting", "sex", "size", "tag.exists", "tagdeployed", "tag.id",
             "tag.side", "biopsy", "biopsy.id", "notes", "tag.notes",
             "fin.photo", "PhotoID", "finuploaded", "match.sugg", "time", "FinShot"), 
           reset)
    output$FinShot <<- NULL
    output$dataentry <<- NULL
    output$PhotoID <<- NULL
    phid$val <<- NULL
    
    reset("data")
    reset("saveEncounter")
    reset("r2submit")
  })
  
  
  
  observeEvent(input$r2submit, {
    #click saveEncounter to save photo/data
    observe({
      shinyjs::click("saveEncounter")})
    #refresh data review table
      start_table <- loadData2Table()
      this_table(start_table)
    #move user to submission page
      updateTabsetPanel(session, "form", selected = "Data Review")
  })
  
  
  
  ######################################################################
  ######### Fin Photo Displays when review table row is selected #######
  ######################################################################
  
  
  
  # prints message if no row is selected in review table
  output$NoSelectedFinTxt <-
      renderText({
        if(is.null(input$finsTable_rows_selected)){
          return("Select an encounter to view Fin ID Photo")
        }else{
          stop()
        }
      })
  
  # Prints name of reformatted finID photo
  output$SelectedFinImgName <- 
    renderText({
        cleanIMGname <- sub(".*FinID_curator/FinIDs_staging/", "", selectedFin())   # selectedfin() is a complete path but we only want to display the file name
        if(!is.null(input$finsTable_rows_selected)){                                #based on 'dropfin' directory. Fix so it paste0's 'dropfin'
          paste0("Now-Standardized File Name: ",cleanIMGname)
        }
     })
  
  # Prints name of uploaded finID photo
  output$SelectedFinOrigImgName <-
    renderText({
      if(!is.null(input$finsTable_rows_selected)){
        paste0("Original File Name: ",selectedFinOrig())
      }
    })

  
  # extracts name of the uploaded finID photo
  # for selected row of review table
  selectedFinOrig <- reactive({          
    if(is.null(input$finsTable_rows_selected)){
      # do nothing
    }else{
      proxytable = this_table()
      as.data.frame(proxytable)
      selectedFinOrig <- proxytable[input$finsTable_rows_selected,c("orig.fin.file")]
      return(selectedFinOrig)
    }
  })
  
  
  # extracts name of the reformatted finID photo
  # for selected row of review table
  
  selectedFin <- reactive({          
    if(is.null(input$finsTable_rows_selected)){     # i could change it to !is.null(..) and put into the if statement but im lazy :)
        # do nothing
    }else{
      proxytable = this_table()
          as.data.frame(proxytable)
      selectedFin <- proxytable[input$finsTable_rows_selected,c("pFn")]
      return(selectedFin)
      }
    })
    
  
  
  ######### renders image of fin based on selected row of review table
  output$SelectedFinImg <- 
    renderImage({
    if(!is.null(input$finsTable_rows_selected)){
       filename <- normalizePath(selectedFin())
       list(src = filename,
          width = "300",
          height = "auto")
          }
    else{
      stop()                                         # removes error message if no row is selected
      }
    }, 
     deleteFile = FALSE           # stops R from deleting file after rendered
    )
  
  
  ######################################################################
  ######################################################################
  
  
  
  
  ######################################################################
  ###############         End of Server Code        ####################
  ######################################################################
}
)

#shinyApp(ui, server)
