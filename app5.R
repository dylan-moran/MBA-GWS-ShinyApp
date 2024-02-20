#Data Entry for GWS Monitoring CentralCA
#New gen, attempt to implement:
#update msg's, less buggy buttons, naming conflicts, & x/y leaflet
#JHMoxley, 9/25/18

source("fin_shiny_fxns2.R")
#set up dropbox
token <- readRDS("droptoken.rds") 
drop_acc(dtoken=token)
appCSS <- ".mandatory_star { color: red; }"
#database for survey entries
dB <- data.frame(NULL)

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
             selectInput("site.phid", labelMandatory("Survey site"),
                         choices = flds$sites),
             dateInput("date.phid", labelMandatory("Survey date"), 
                       value = Sys.Date(), format = "yyyy-mm-dd"),
             hr(),
             checkboxGroupInput("crew", "Crew", 
                                choices = flds$observers, inline = T),
             textInput("vessel", "Vessel", placeholder = "MBA Skiff? Norcal? R/V BS?")
           ),
           mainPanel(
             #outputs for tab 1
             # sliderInput("effort.on", "On Effort?", 
             #             value = 8, min = 4, max = 20, step = 0.5),
             # sliderInput("effort.off", "Off Effort?",
             #             value = 15, min = 4, 
             #             max = 20, step = 0.5),
             sliderInput("effort", "Start/Stop of Survey Effort?", 
                         min = 6, max = 20, value = c(8, 15)),
             textInput("notes.survey", "Notes from survey day", 
                       placeholder = "breaches? predations?", width = "100%"),
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
             radioButtons("user", "User", 
                          choices = flds$observers, inline = T, selected=character(0)),
             # uiOutput("site.phid"),
             # uiOutput("date.phid"),
             numericInput("sighting.phid", labelMandatory("Sighting #"), value = NULL, 
                          min = 01, max = 99, step =1), #MAKE THIS NUMERIC?
             hr(),
             fileInput("fin.photo", labelMandatory("Upload fin here"), 
                       multiple = F, accept=c("image/jpeg", "image/png", "image/tiff",
                                              ".jpeg", ".jpg", ".png", ".tiff")),
             textInput("time", labelMandatory("Time of Observation"), placeholder = "24HR CLOCK PLS (e.g., 0915 for 9:15"),
             hr(),
             selectInput("sex", labelMandatory("Sex (U if unknown)"), choices = c("M", "F", "U"), 
                         selectize = F, selected = "U"), 
             numericInput("size", labelMandatory("Size (in ft)"), value = NULL, min = 4, max = 20, step = 0.5),
             hr(), #break in UI
             conditionalPanel(   #collect data once fin is uploaded & photoID is crorect       
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
                                      textInput("match.sugg", "Suggestions to the MatchMaker?", placeholder = "Zat you, Burnsey?", 
                                                width = "600px"),
                                      imageOutput(outputId = "FinShot", width = "auto", height="auto"),
                                      hr(),
                                      uiOutput("xyloc"),
                                      leafletOutput("map"),
                                      DT::dataTableOutput("dataentry"),
                                      conditionalPanel("mandatoryFilled",
                                                       actionButton("masfins", "Mas Fins?", class="btn-primary"),
                                                       actionButton("r2submit", "Ready To Submit?", class="btn-primary")
                                      )#submit button that tabs to next panel?
                     )
           )
           
  ),
  ################
  ##DATA SUBMISSION##
  ################
  tabPanel("Data Submission",
           # fluidRow(
           #   verbatimTextOutput('x4')
           # ),
           fluidPage(
             verbatimTextOutput('x4'),
             #create list of site/date combos avail for review
             uiOutput("for.review"),
             # selectInput("review", label = "Select data for submission",
             #             choices = drop_dir(dropsc)$name, selected = NULL),
             checkboxInput("reviewed", 
                           label = "I HAVE VERIFIED THE DATA", 
                           value = F),
             
             
             #display data
             DT::dataTableOutput("finsTable"),
             
             #reveal button if data is reviewed
             uiOutput("reviewed")
           )
  )
),
################
##SERVER##
################
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
  
  ######################
  ######################
  ##Page 2 server stuff
  
  #data gatekeeper, fields can be entered once PhotoID & file exists
  #data can't be pressed into a csv file until conditions are met
  ######################
  #ALL THINGS PHOTO ID STAMP HAPPEN HERE
  ######################
  phid <- reactiveValues()
  phid$exists <- tools::file_path_sans_ext(
    getExistingPhids(all = T, paths = F))
  
  finUP <- reactive({
    if(is.null(input$fin.photo)){
      return(NULL)
    }else{
      #make fin photo
      cat("the datapath currently is", file.exists(input$fin.photo$datapath), "\n")
      re1 <- reactive({gsub("\\\\","/", input$fin.photo$datapath)})
      output$FinShot <- renderImage({list(src = re1())}, deleteFile = FALSE)
      
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
      
      # output$PhotoID <- renderText({paste0(toupper(input$site.phid),
      #                                      format(input$date.phid, "%y%m%d"),
      #                                      ifelse(nchar(input$sighting.phid==1),
      #                                             paste0("0", input$sighting.phid),
      #                                             input$sighting.phid))})
      
      
      #make site map for leaflet input
      ctr <- flds$coords[[input$site.phid]]
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap=T)) %>%
          #addMarkers(data = matrix(c(-123.01, 37.69), nrow=1))
          setView(lng=ctr[1,1], lat = ctr[1,2], zoom = 14) 
      })
      observeEvent(input$map_click,{
        #capture click
        click <- input$map_click
        phid$lat <- click$lat
        phid$long <- click$lng
        #print("pos is", clat, clong)
        #add to map
        leafletProxy('map') %>%
          clearMarkers() %>% 
          addPulseMarkers(data = click, lng=~lng, lat=~lat, icon = makePulseIcon(), 
                          options = leaflet::markerOptions(draggable = F))
        #MIGHT NOT UPDATE LOC?? make draggable false
        
        output$xyloc <- renderText({paste("lat: ", round(click$lat, 4),
                                          "| long: ", round(click$lng, 4))})
        
      })
      
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
  
  #submit buttons only if fields are filled, theres a photo, & proper photoID
  observe({
    mandatoryFilled <- vapply(flds$mandatory,
                              function(x) {
                                
                                !is.null(input[[x]]) && input[[x]] != "" && !is.null(finUP())
                              },
                              logical(1))
    
    mandatoryFilled <- all(mandatoryFilled)
    
    # shinyjs::toggleState(id = "masfins", condition = mandatoryFilled)
    # shinyjs::toggleState(id = "r2submit", condition = mandatoryFilled)
  })
  
  ######################
  ######################
  ##Page 3 server stuff
  #build data frame, needs to become reactive
  #output$reviewed <- renderText({input$reviewed})
  #can we make this load sooner?? 
  
  output$reviewed <- renderUI({
    if(input$reviewed == T){
      actionButton("SAVEDATA", "SUBMIT & STORE", class="btn-primary")}
  })
  
  # review <- reactive({
  #   if(output$reviewed==F){return(NULL)}
  #   else{
  #     output$reviewCHK <- renderText({input$reviewed})
  #   }
  # })
  # output$reviewed <- return(review())
  
  ######################
  ######################
  ##Data making stuff
  formData <- reactive({
    #save photo
    if(is.null(finUP)){
      return(NULL)
      ##SOME WARNING DAATA WILL NOT BE SAVED W?O A PHOTO FILE
    }
    else{
      
      
      #can make the fields match zegami here? 
      data <- c(refID = "UNMATCHED", name = "NONE_YET", 
                match.sugg = as.character(input$match.sugg), 
                time.obs = as.character(input$time),
                # PhotoID = paste0(toupper(input$site.phid), 
                #                  format(input$date.phid, "%y%m%d"), 
                #                  sprintf("%02s", input$sighting.phid)), 
                # PhotoID = paste0(toupper(input$site.phid), 
                #                  format(input$date.phid, "%y%m%d"), 
                #                  ifelse(nchar(input$sighting.phid==1), 
                #                         paste0("0", input$sighting.phid),
                #                         input$sighting.phid)), 
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
  
  
  ######################
  ######################
  ##Button doing stuff
  #Find surveys available to submit & populate via unique dates/locs
  #produce the selectInput for surveys available
  output$for.review <- renderUI({
    selectInput("for.review","Surveys available for review/submission",
                choices = survey.avail(), selected = NULL)
  })
  #parse out the answers
  rev.loc <- reactive({
    str_split(input$for.review, " ", simplify = T)[1]
  })
  rev.dt <- reactive({
    str_split(input$for.review, " ", simplify = T)[2]
  })
  
  #produce choices of surveys available for review/submission
  survey.avail <- reactive({
    phid.data <- bind_cols(
      parse_phid(getExistingPhids(all = F, paths = T, data.only = T)),
      #add path
      path = getExistingPhids(all = F, paths = T, data.only = T)
    )
    #get surveys that are staged
    staged <- unique(paste(phid.data$loc, phid.data$date))
    return(staged)
  })
  
  output$finsTable <- DT::renderDataTable(
    loadData2(
      dt.filt = rev.dt(), loc.filt = rev.loc()
    ),
    ##maybe ok to not have phid.only approach??
    rownames = F, server = T, editable = T,
    extensions = "Buttons", 
    options=list(searching=F, lengthChange=F, paging=F,
                 columnDefs = list(list(visible = F, 
                                        #hide refID/name, site/date/#, tag.side/y-n.biopsy, user/lat/lon/timestamp/dfn/pfn
                                        targets = c(1:2,6:8,14:15, 19:24)-1)))
    
  )
  #messaging of rows selected for deletion
  output$x4 = renderPrint({
    s = input$finsTable_rows_selected
    #phids.sel <- input$finsTable[s, 5]
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  observeEvent(input$addfins, {
    updateTabsetPanel(session, "form", selected = "Fin Photo Entry")
  })
  
  #Observe "mas fins" event here
  observeEvent(input$masfins, {
    data <- data.frame(formData(), stringsAsFactors = F)
    showNotification(paste(data$dfN, "being uploaded to", dropsc), 
                     #action = a(href="javascript:location.reload();", "Reload page"),
                     closeButton = F, type = "message", duration=9,
                     id = "datUP")
    saveData2(data)
    
    savePhoto2(input$fin.photo, phid$val)
    showNotification(paste(phid$val, "photo uploaded to", dropfin), 
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
    reset("masfins")
    reset("r2submit")
  })
  
  
  
  ##Somehow do something different 
  observeEvent(input$r2submit, {
    #click masfins to save photo/data
    observe({
      shinyjs::click("masfins")})
    #move user to submission page
    updateTabsetPanel(session, "form", selected = "Data Submission")
    
    
  })
  
  ##actions once data can be stored
  observeEvent(input$SAVEDATA,{
    #needs a way to only grab a single day of data
    #loadData2 has phid.only param
    saveLog(data, site = input$site, date = input$date)
  })
  
}
)
#shinyApp(ui, server)
