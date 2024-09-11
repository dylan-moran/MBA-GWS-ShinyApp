# Updated for AWS integration



require(shiny)          # Shinyapp fxns
#require(rsconnect)      # apps for Shinyapps
require(DT)             # Wrapper for JavaScript Library 'datatables'
require(shinyTime)      # time input widget for Shiny
require(shinyjs)        # improvement of UI for Shinyapps
library(leaflet)        # creates interactive maps
library(leaflet.extras) # supports leaflet
### library(leaflet.providers)    # do I need this for the different basemaps? couldnt get it working
library(tidyverse)      # data manipulation
library(shinyalert)     # creates popup messages for Shiny
library(shinythemes)
library(lubridate)
#library(shinyFiles)




#Directory Shortcuts within 'MBA GWS ShinyApp' folder

# paste0(Localdir, "/", DIR__)   # any time you want to set path to old dropbox dirs

DIR_archive <- "/srv/shiny-server/FinID_curator/archive"          # unused in any code? see fxns line 197

DIR_Enc_prelim <- "/srv/shiny-server/FinID_curator/Encounters_prelim"            # individual encounters CSV save location
# each encounter gets its own csv. Wipes clean after final data submitted


DIR_FinID_Final <- "/srv/shiny-server/FinID_curator/FinIDs_FINAL"     # folder with final fin photos

DIR_Enc_final <- "/srv/shiny-server/FinID_curator/Encounters_FINAL"      # reviewed and submitted data gets saved here (inop)

DIR_Enc_Backup <- "/srv/shiny-server/FinID_curator/archive/Encounters_BACKUP"   # backup archive of all encounter submissions

DIR_ARCHIVE_LOGOS <- "/srv/shiny-server/FinID_curator/archive/Logos"

DIR_ARCHIVE_IMAGES <- "/srv/shiny-server/FinID_curator/archive/Images"









#list of observers available to checkbox
flds <- list(
  observers = c("PK", "SA", "SJ", "JM", "TC", "TW", "EM", "OJ", "DM"),
  users = c("PK", "SA", "SJ", "JM", "TC", "TW", "EM", "OJ", "DM"),
  sites = c("PR", "FAR", "AN", "APT"),
  mandatory = c("user", "sex", "size"),
  new_mand = c("user", "site.phid", "date.phid", "sighting.phid", "time", "sex", "size"),
  coords = list(matrix(c(-123.00, 38.24), nrow=1), #Tomales station
                matrix(c(-123.0058, 37.6979), nrow=1), #FAR
                matrix(c(-122.34, 37.11), nrow=1), #Ano station
                matrix(c(-121.93, 36.97), nrow=1))  # APT
)
names(flds$coords) <- flds$sites





##
#attempt to generalize
entries <- list(
  #list(id = "site.phid", type = "select", title = "Monitoring Site", choices = flds$sites),
  list(id = "date.phid", type = "date", title = "Date", value = Sys.Date(), format = "yyyy-mm-dd"),
  list(id = "sighting", type = "text", title = "Sighting #", placeholder = "0?"),
  list(id = "matchsugg", type = "text", title = "Suggested Match", placeholder = "handle, tag#?"),
  list(id = "time", type = "text", title = "Time of sighting", placeholder = "0915")
  #list(id = "sex", type = "select", title = "Sex (U if unknown", choices = c("U","M","F"), selected="U"),
  #list(id = "size", type = )
)


#add a red star next to mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}







loadData2 <- function(directorypath) {          # output df is called 'EncounterData'  |  dt.filt = NULL, loc.filt = NULL
  # loc and dt filts are disabled
  
  dir <- list.files(directorypath)
  #list files 
  files <- as.character(dir)
  #get metadata
  files <- bind_cols(filename = files, parse_phid(files))
  files$path <- paste0(directorypath, "/", files$filename)
  #read in data, all character classes
  EncounterData <- lapply(files$path, read.csv, stringsAsFactors = F,          # changed drop_read_csv to read.csv
                          as.is = T, colClasses = "character")
  EncounterData <- dplyr::bind_rows(EncounterData)
  
  return(EncounterData)
}


check.existing.phids <- function(new.phid){      # checks for duplicate phids in total master backup
  phid.archive <- loadData2(DIR_Enc_Backup)
  phid.prelims <- loadData2(DIR_Enc_prelim) # new with OR statment below
  view(phid.prelims)
  if((new.phid %in% phid.archive$PhotoID) | (new.phid %in% phid.prelims$PhotoID) ){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

check.existing.phids2 <- function(new.phid){      # checks for duplicate phids in total master backup
  phid.archive <- loadData2(DIR_Enc_Backup)
  if(length(list.files(DIR_Enc_Backup))>0){
    if(length(list.files(DIR_Enc_prelim))>0){
      phid.prelims <- loadData2(DIR_Enc_prelim)
      if( (new.phid %in% phid.archive$PhotoID) | (new.phid %in% phid.prelims$PhotoID) ){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }else{
      if((new.phid %in% phid.archive$PhotoID)){
        return(TRUE)
      }else{
        return(FALSE)
      }}
  }else{
    return(FALSE)
  }
  
  
}




### new loaddata2table fxn ###

loadData2Table <- function(directorypath){
  loadedData<- loadData2(directorypath)
  loadedDT <- data.frame(loadedData)
  return(loadedDT)
}

##############################


loadData3Table <- function(){
  loadedData<- read.csv("/srv/shiny-server/FinID_curator/archive/PlaceholderFile.csv")
  loadedDT <- data.frame(loadedData)
  return(loadedDT)
}










#parsing data from phids
#    takes ID from naming convention and extracts dates and No.
parse_phid <- function(ids){
  require(stringr)
  if(any(nchar(ids)>11, na.rm = T)){
    #if ids are longer than standard id (ie. pathways), extract just the phid
    ids <- str_extract(ids, "([A-Z]{2,3})([0-9]){8}")
  }
  
  id.dat <- data.frame(phid = ids) %>%            #convert to df for stringr tools
    mutate(loc = as.vector(str_match(phid, "^[:alpha:]*")),
           code = str_extract(phid, "[0-9]+"),
           #8 digit #'s are stand eval; YYMMDD w/ 2 digi shark-du-jur#
           yr = ifelse(nchar(code)==8, substr(code, 1,2), NA),
           month = ifelse(nchar(code)==8, substr(code,3,4), NA),
           day = ifelse(nchar(code)==8, substr(code,5,6), NA),
           dujour = ifelse(nchar(code)==8, substr(code,7,8),NA))
  #2yr evals seem common
  id.dat$yr <- ifelse(nchar(id.dat$code)==2, id.dat$code, id.dat$yr)
  id.dat$yr4 <- ifelse(as.numeric(id.dat$yr) > 80,
                       paste0("19", id.dat$yr), paste0("20", id.dat$yr))
  #filter non standard naming
  id.dat$loc <- ifelse(id.dat$loc %in% flds$sites, id.dat$loc, NA)
  
  #make dates
  id.dat<- id.dat %>% mutate(date = as.Date(
    paste(yr4, month, day, sep = "/"), format = "%Y/%m/%d"))
  
  return(id.dat)
}









#################
##STORAGE FXNS ##
#################



#write & save data (if no file); LOCALLY
saveData <- function(dat){
  #fileName <- sprintf("here_lies_data.csv")
  # write.table(x = dat, file=file.path(dd, fN),
  #             row.names = F, quote = T, append=T, sep = ",")
  dat <- as.data.frame(dat)
  if(file.exists(file.path(DIR_archive, dfN))){             # added 'drop' in front of 'dd', added 'd' in front of fN for all instances here
    #append if file exists
    write.table(x = dat, file=file.path(DIR_archive, dfN),
                row.names = F, col.names = F,
                quote = T, append=T, sep = ",")
    # file.create(file.path(dd, 'igothere.csv'))
  }else{
    #write csv if file doesn't
    write.table(dat, file = file.path(DIR_archive, dfN),
                row.names = F, col.names = T,
                quote = T, append=F, sep = ",")
  }
}


#using dropbox, individual files for entries
saveData2 <- function(data) {
  data <- data.frame(data, stringsAsFactors = F)
  dropPath <- file.path(DIR_Enc_prelim, data$dfN)
  print(paste0("dropPath is:", "(DIR_Enc_prelim, data$dFn) which is: ", dropPath))
  tempPath <- file.path(tempdir(),
                        data$dfN)
  print(paste("storing data in this file", dropPath))
  #one entry per file
  write.csv(data, dropPath, row.names = F, quote = TRUE)     # Writes to EncPrelim
}


#################
## SaveLog fxn ##
#################


#save photo uploads w/ photoID naming convention

savePhoto2 <- function(photo, phid){
  
  dropPath <- file.path(paste0(DIR_FinID_Final,"/", phid, ".png"))     # named photo saves to 'staging' folder
  file.copy(from = photo$datapath, to = dropPath)              # copies photo from interface to folder
  print(paste0("FinPhoto is uploaded to ", dropPath))          # Prints path to photo in console (not displayed on UI)
  # drop_upload() used to be here
}



#timestamping
epochTime <- function(){
  as.integer(Sys.time())
}










appCSS <- ".mandatory_star { color: red; }"




#####
#APP#
#####


shinyApp(ui = 
           navbarPage(
             windowTitle = "FinID Data Entry Portal",   # on tab of webbrowser
             title = "FinID Data Entry Portal",
             theme = shinytheme("yeti"),      #  "yeti"
             id = "form",
             
             ###########################
             #### PAGE 0.1: WELCOME ####
             ###########################
             
             
             
             tabPanel("Home",
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     imageOutput(outputId = "JlabLogo",
                                                 width = "auto",
                                                 height = "auto"),
                                     hr(),
                                     textOutput("HomeSideTxt.VersionNo"),
                                     tags$head(tags$style("#HomeSideTxtVersionNo{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                                     hr(),
                                     textOutput("HomeSideTxt.Purpose"),
                                     tags$head(tags$style("#HomeSideTxt1{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                                     hr(),
                                     textOutput("HomeSideTxt.1"),
                                     tags$head(tags$style("#HomeSideTxt1{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                                     hr(),
                                     textOutput("HomeSideTxt.2"),
                                     tags$head(tags$style("#HomeSideTxt2{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                        ),
                        mainPanel(width = 10,
                                  textOutput("Welcome"),
                                  tags$head(tags$style("#Welcome{
                                                           color: grey; 
                                                           font-size: 30px; 
                                                           font-style: italic;
                                                           font-weight: 800;}")),
                                  hr(),
                                  htmlOutput("Directions"),
                                  tags$head(tags$style("#Directions{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                                  actionButton("begin_btn", "Click here to start entering data!", class = "btn-primary"),
                                  htmlOutput("Spacer_logos"),
                                  hr(),
                                  fluidRow(
                                    column(width = 2),
                                    column(width = 2,
                                           imageOutput("JlabSeal")),
                                    column(width = 2,
                                           imageOutput("BigFishLabLogo")),
                                    column(width = 2,
                                           imageOutput("StanfordLogo")),
                                    column(width = 3,
                                           imageOutput("CWSPLogo"))
                                  )
                        ),
                        position = c("right")
                      )  # end of sidebar layout
             ),  # END of Tabpanel 'Home'
             
             ########################
             #### PAGE 0.2: FAQs ####
             ########################
             
             
             tabPanel("FAQs",
                      wellPanel(
                        htmlOutput("FAQs.Txt")
                      )
                      
             ), # End of TabPanel
             
             
             
             
             
             
             
             
             
             #########################
             ## PAGE 1: SURVEY INFO ##
             #########################
             tabPanel("1. Survey Info Metadata",
                      sidebarPanel(
                        radioButtons("user", labelMandatory("User"), 
                                     choices = flds$users, inline = T, selected=character(0)),
                        dateInput("date.phid", labelMandatory("Survey date"), 
                                  value = Sys.Date(), format = "yyyy-mm-dd"),
                        selectInput("site.phid", labelMandatory("Survey site"),
                                    choices = flds$sites),
                        
                        hr(),
                        checkboxGroupInput("crew", "Vessel Crew", 
                                           choices = flds$observers, inline = T),
                        textInput("vessel", "Vessel Name", placeholder = "e.g. Blue Serengeti , Flatline")
                      ),
                      
                      mainPanel(
                        htmlOutput("SurveyInfoDirections"),
                        tags$head(tags$style("#SurveyInfoDirections{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                        hr(),
                        
                        fluidRow(
                          column(width = 4,
                                 textInput("surveystart", label = "Survey Start Time", placeholder = "24HR FORMAT (e.g., 0915 for 9:15a)")
                          ),
                          column(width = 4,
                                 textInput("surveyend", label = "Survey End Time", placeholder = "24HR FORMAT (e.g., 1500 for 3:00p)")
                          )),
                        # sliderInput("effort", "Start-Stop Time of Survey Effort",        # swapped with text inputs above for better time resolution
                        #             min = 6, max = 20, step = 0.25, value = c(8, 15)),
                        textInput("notes.survey", "Notes from survey day", 
                                  placeholder = "breaches? predations? see a mermaid?", width = "100%"),
                        textOutput("crew"),
                        actionButton("addfins", "Ready to add Fins?", class="btn-primary")
                      )
             ),
             
             
             
             ##########################
             ## PAGE 2: FIN ID ENTRY ##
             ##########################
             
             tabPanel("2. Photo ID Entry",
                      shinyjs::inlineCSS(appCSS),
                      
                      sidebarPanel(
                        numericInput("sighting.phid", labelMandatory("Sighting #"), value = NULL, 
                                     min = 01, max = 99, step =1),
                        hr(),
                        fileInput("fin.photo", labelMandatory("Upload fin here"), 
                                  multiple = F, accept=c("image/jpeg", "image/png", "image/tiff",
                                                         ".jpeg", ".jpg", ".png", ".tiff")),
                        textInput("time", labelMandatory("Time of Observation (24HR)"), placeholder = "24HR FORMAT(e.g., 0915 for 9:15a)"),
                        hr(),
                        selectInput("sex", labelMandatory("Sex (U if unknown)"), choices = c("M", "F", "U"), 
                                    selectize = F, selected = "U"), 
                        numericInput("size", labelMandatory("Size (in ft)"), value = NULL, min = 4, max = 20, step = 0.5),
                        checkboxInput("unknownsize", label = "Unknown Size", value = FALSE),
                        uiOutput("checksize"),
                        tags$head(tags$style("#checksize{
                                                      color: red; 
                                                      font-size: 20px; 
                                                      font-style: bold;}")),
                        hr(), #break in UI
                        conditionalPanel(   #collect data once fin is uploaded & photoID is correct       
                          condition = "output.finuploaded",
                          textInput("notes", "Notes", placeholder = "e.g. pings heard, secondary marks, scars, etc", 
                                    width = "600px"),
                          selectInput("biopsy", "Biopsy?", choices = c("N", "Y"), selected="N"),
                          conditionalPanel(
                            condition = "input.biospy != 'N'",
                            textInput("biopsy.id", "Vial Number?"),
                            selectInput("tag.exists", "Tagged Already?", choices = c("U", "Y"), selected = "U"),
                            selectInput("tagdeployed", "New Tag?", choices = c("None", "PAT", "Acoustic", "Stomach", "Clamp"), selected = "None"),
                            #make ^ check box and conditional panel for each tag selected?
                            conditionalPanel(
                              condition = "input.tagdeployed != 'None'",
                              radioButtons("tag.side", "Deployed On? ", choices = c("NA", "L", "R"), 
                                           inline = T), 
                              textInput("tag.id", "Tag ID#"),
                              textInput("tag.notes", "Tagging Notes", width = '600px', 
                                        placeholder = "e.g., programming params, Ptt/SPOT used, orientation")
                            )
                          )
                        )
                        
                        
                      ), 
                      
                      mainPanel(useShinyjs(),
                                uiOutput("PhotoIDtxt"),
                                tags$head(tags$style("#PhotoIDtxt{
                                                      color: grey; 
                                                      font-size: 30px; 
                                                      font-style: bold;}")),
                                hr(),
                                conditionalPanel("output.finuploaded",
                                                 fluidRow(
                                                   column(width = 5, 
                                                          textInput("match.sugg", 
                                                                    "Think you know this shark? Put its name here", 
                                                                    placeholder = "e.g. Sicklefin, Snowflake, Burns, Elvis", 
                                                                    width = "auto"),
                                                          imageOutput(outputId = "FinShot", 
                                                                      width = "auto",
                                                                      height = "auto")),
                                                   column(width = 5,
                                                          textOutput("mapdirections"),
                                                          leafletOutput("map", height = 400, width = 400),
                                                          uiOutput("xyloc"),
                                                          # actionButton("resetlatlong", "Reset Lat/Long", class = "btn-primary")
                                                   )
                                                 ),
                                                 hr(),
                                                 conditionalPanel("mandatoryFilled",
                                                                  uiOutput("SaveBtns")
                                                 )
                                )
                      )
                      
             ),
             
             
             
             #############################
             ## PAGE 3: DATA SUBMISSION ##
             #############################
             
             tabPanel("3. Data Review",
                      fluidPage(
                        textOutput("UserEmpty"),                                   # reminds that User must be filled in for submission to work
                        tags$head(tags$style("#UserEmpty{color: red; 
                                  font-size: 20px; 
                                  font-style: bold;}")),                # user empty text is red
                        textOutput("NoEncounterFiles"),
                        tags$head(tags$style("#NoEncounterFiles{color: red; 
                                  font-size: 20px; 
                                  font-style: bold;}")),
                        uiOutput("refresh_notification"),
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
                                    htmlOutput("datareviewdirections"),
                                    tags$head(tags$style("#datareviewdirections{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}")),
                                    hr(),
                                    
                                    fluidRow(
                                      column(width = 2,
                                             actionButton("resetedits_btn", "Reset Edits", 
                                                          class = "btn-primary")
                                      ),
                                      column(width = 4,
                                             fluidRow(
                                               column(width = 2, 
                                                      checkboxInput("reviewed", 
                                                                    label = NULL, 
                                                                    value = F),
                                                      tags$style("input[type=checkbox] {
                                                      transform: scale(1.5);
                                                      }")),
                                               column(width = 8,
                                                      # satsfied with data?
                                                      htmlOutput("reviewedlabel"),
                                                      tags$head(tags$style("#reviewedlabel{
                                                           color: grey; 
                                                           font-size: 15px; 
                                                           font-style: bold;}"))
                                               )
                                             ) #end of fluidrow
                                             
                                             
                                      ),
                                      column(width = 3,
                                             #reveal button if data is reviewed
                                             uiOutput("reviewed")
                                      )
                                      
                                    ),
                                    
                                    # data review table
                                    DT::dataTableOutput("finsTable")
                          )
                        )
                      )
             ),
             
             
             
             #############################
             ##    PAGE 4: Settings     ##
             #############################
             
             
             tabPanel("Settings",
                      fluidPage(
                        navlistPanel(widths = c(2, 10),
                                     tabPanel("Directories and File Locations",
                                              textOutput("SettingsDirectoriesTitle"),
                                              tags$head(tags$style("#SettingsDirectoriesTitle{
                                                           color: grey;
                                                           font-size: 25px;
                                                           font-style: bold;}")),
                                              hr(),
                                              htmlOutput("SettingsDirectoriesTxt")
                                     ),
                                     tabPanel("Support",
                                              textOutput("SettingsSupportTitle"),
                                              tags$head(tags$style("#SettingsSupportTitle{
                                                           color: grey;
                                                           font-size: 25px;
                                                           font-style: bold;}")),
                                              hr(),
                                              htmlOutput("SettingsSupportTxt")
                                     )
                                     
                        )  # end of navListPane()
                      )  # end of fluidPage()
                      
             ) # end of 'Settings' tabPanel()
             
             
             
             
             
             
             
             
           ), ### end of UI (navbar 1) #######################################################################
         
         ###################################################################################################
         ###################################################################################################
         ###################################################################################################
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         ###################################################################################################
         ################################           SERVER           #######################################
         ###################################################################################################
         
         server = function(input, output, session) {
           #enlarge maximum upload size 
           options(shiny.maxRequestSize=30*1024^2)
           
           
           
           ##########################################
           ######### Home Page Server Stuff #########
           ##########################################
           
           
           #########################
           # Home Page: Main Panel #
           #########################
           
           
           
           
           
           output$Welcome <-
             renderText("Welcome to the White Shark FinID Data Entry Portal!")
           
           
           
           output$Directions <-
             renderUI({
               linebreak <- paste(" ")
               str2 <- paste("3 STEPS FOR SUBMITTING DATA")
               str3 <- paste("1. INPUT SURVEY METADATA - Enter once per field day")
               str4 <- paste("2. INPUT SHARK ENCOUNTERS - Submit Photo ID, Size, Sex, Tags, etc.")
               str5 <- paste("3. REVIEW DATA - Review encounters in table format, make edits, then submit")
               str6 <- paste("Before starting data entry, match your field notes to your edited video / stills")
               HTML(paste(linebreak, linebreak,
                          str2,
                          linebreak,
                          str3,
                          str4,
                          str5,
                          linebreak, linebreak, linebreak, linebreak,
                          str6,
                          linebreak, linebreak, linebreak, linebreak, linebreak, linebreak,
                          sep = '<br/>'
               ))
             })
           
           
           
           output$Spacer_logos <-
             renderUI({
               linebreak <- paste(" ")
               HTML(paste(linebreak, linebreak, linebreak, linebreak, linebreak,
                          sep = '<br/>'))
             })
           
           
           observeEvent(input$begin_btn, {
             updateTabsetPanel(session, "form", selected = "1. Survey Info Metadata")
           })
           
           
           
           #########################
           # Home Page: Side Panel #
           #########################
           
           
           output$JlabLogo <-
             renderImage({list(src = normalizePath(paste0(DIR_ARCHIVE_LOGOS, "/" ,"JorgensenLab_Logo_BLK_Transparent.png")), 
                               width = "200", 
                               height = "auto")}, 
                         deleteFile = FALSE)
           
           output$JlabSeal <-
             renderImage({list(src = normalizePath(paste0(DIR_ARCHIVE_LOGOS, "/" ,"JorgensenLab_OfficialSeal.png")), 
                               width = "auto", 
                               height = "100")}, 
                         deleteFile = FALSE)
           
           output$BigFishLabLogo <-
             renderImage({list(src = normalizePath(paste0(DIR_ARCHIVE_LOGOS, "/" ,"BigFishLabLogo.jpg")), 
                               width = "auto", 
                               height = "100")}, 
                         deleteFile = FALSE)
           
           output$CWSPLogo <-
             renderImage({list(src = normalizePath(paste0(DIR_ARCHIVE_LOGOS, "/" ,"CWSP.png")), 
                               width = "auto", 
                               height = "100")}, 
                         deleteFile = FALSE)
           
           output$StanfordLogo <-
             renderImage({list(src = normalizePath(paste0(DIR_ARCHIVE_LOGOS, "/" ,"StanfordLogo.jpg")), 
                               width = "auto", 
                               height = "100")}, 
                         deleteFile = FALSE)
           
           
           
           
           
           output$HomeSideTxt.VersionNo <-
             renderText("App Version 7.3.2")
           
           output$HomeSideTxt.Purpose <-
             renderText("This app standardizes the data entry process and data output format for White Shark FinIDs.")
           
           output$HomeSideTxt.1 <-
             renderText(paste("Built by Dylan Moran, adapted from Jerry Moxley's original 'MBA Shiny App'."))
           
           output$HomeSideTxt.2 <-
             renderText(paste("For any support or feature requests, contact Dylan Moran (dmoran@csumb.edu)"))
           
           
           
           
           
           
           
           ##########################################
           ######### FAQs Page Server Stuff #########
           ##########################################
           
           
           
           
           output$FAQs.Txt <-
             renderUI({
               linebreak <- paste(" ")
               str1 <- paste("Q. What will my output data look like?")
               str2 <- paste("A. This app will automatically rename your FinID photos (saved to '/FinIDs_FINAL' folder) 
                    and generate a csv table (saved to '/Encounters_FINAL' folder) containing all FinID 
                    data from the session.")
               str3 <- paste("Q. What if my app crashes? Do I lose all of my data from the session?")
               str4 <- paste("A. No! There is a built-in safety backup in case the app crashes and data have not been 
                    submitted. Every individual encounter is saved to its own csv file into 
                    the '/Encounter' folder. The files will be wiped from the temporary 
                    location ONLY after you submit the final datatable. If the app crashes or you need to close out of the app, check the 'Data Review Page' 
                    to determine where you left off and what data has been saved or not.")
               str5 <- paste("Q. What if I have encounters without good dorsal fin shots?")
               str6 <- paste("A. Do not log any encounters that don't have a high-quality dorsal fin photo. 
                    The app will not allow you to submit data without size, sex, and a FinID photo.")
               HTML(paste(str1,
                          linebreak,
                          str2,
                          linebreak,
                          linebreak,
                          str3,
                          linebreak,
                          str4,
                          linebreak,
                          linebreak,
                          str5,
                          linebreak,
                          str6,
                          sep = '<br/>'
               ))
             })
           
           
           
           
           
           
           ######################
           ##Page 1 server stuff
           ######################
           
           
           output$SurveyInfoDirections <-
             renderUI({
               linebreak <- paste(" ")
               str1 <- paste("Notes")
               str2 <- paste("- All values will be retained while on other pages.")
               str3 <- paste("- If you are submitting data for multiple days in one session, make sure to return
                             to this page and update the survey information.")
               HTML(paste(str1, 
                          str2,
                          str3,
                          sep = '<br/>'))
               
             })
           
           
           
           
           
           
           
           
           
           #update phids from survey
           observeEvent(input$survey,{
             updateSelectInput(session, "site.phid", selected = input$survey)
           })
           observeEvent(input$date.survey,{
             updateDateInput(session, "date.phid", value = input$date.survey)
           })
           output$site.phid = renderText({paste("<font color=\"#808080\"><b>SURVEY SITE<font color=\"#808080\"></b> assigned as: ", 
                                                HTML(paste0("<font color=\"#808080\"><b>", input$site.phid, "<b>")))})
           output$date.phid = renderText({paste(HTML("<font color=\"#808080\"><b>SURVEY DATE<font color=\"#808080\"></b> assigned as: "), 
                                                HTML(paste0("<font color=\"#808080\"><b>",
                                                            as.Date(input$date.phid, format = "%m-%d-%Y"),
                                                            "</b>")))})
           
           
           
           # #make off effort depend on effort (can't be before on effort)
           # observe(input$effort.off <= input$effort.on,{
           #   updateSliderInput(session, "effort.off", value = input$effort.on + 0.5)
           # })
           
           
           observeEvent(input$addfins, {
             updateTabsetPanel(session, "form", selected = "2. Photo ID Entry")
           })
           
           
           
           
           
           
           
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
           
           
           ###########
           
           output$checksize <- renderUI({
             req(input$size)
             if(input$size < 5 | input$size > 19){
               renderText(paste0("Just checking...was the shark actually ", input$size , "ft long?"))
             }
           })
           
           
           # finphotoexists <- reactive({
           #   #print(paste0("Re-evaluate: is.null(input$fin.photo$datapath) is ", is.null(input$fin.photo$datapath)))
           # 
           #   #does the photo exist?
           #   if(!is.null(input$fin.photo)){                  # photo is input
           #     if(file.exists(input$fin.photo$datapath)){    # AND file exists
           #       return(TRUE)
           #       view(input$fin.photo)
           #     }else{
           #       return(FALSE)                                     # file has been deleted
           #     }
           #   }else{
           #     return(FALSE)       # fin.photo is NULL (hasnt been uploaded the first time)
           #   }
           # 
           #   })
           
           photoexist.rv <- reactiveValues()
           #   
           #   finphotoexists <- 
           #     observe({
           #        photoexists <- file.exists(input$fin.photo$datapath)
           #       if(photoexists==TRUE){
           #         photoexist.rv$x <- 1
           #         }else{
           #           photoexist.rv$x <- 0
           #         }
           #      })
           
           
           # state <- reactiveValues()
           # 
           # observe({
           #   state$x <- input$a
           #   state$y <- ifelse(state$x < 4, 1, 0)
           # })
           
           does.phid.exist <- reactive({check.existing.phids2(phid$val)})
           
           
           ################
           
           finUP <- reactive({
             if(is.null(input$fin.photo)){    #  is.null(input$fin.photo)   photoexist.rv$state == "FALSE"   finphotoexists() == FALSE       ####doesnt work
               return(NULL)
             }else{
               print("FinUP is TRUE")
               #make fin photo
               re1 <- reactive({gsub("\\\\","/", input$fin.photo$datapath)})
               output$FinShot <- renderImage({list(src = re1(), 
                                                   width = "auto", 
                                                   height = "300")}, 
                                             deleteFile = FALSE)
               
               
               #make id, check dupes
               #make a dummy
               phid$site <- input$site.phid                   # delete?
               phid$date <- input$date.phid                   # delete?
               phid$val <- paste0(toupper(input$site.phid),
                                  format(input$date.phid, "%y%m%d"),
                                  ifelse(input$sighting.phid < 10,
                                         paste0("0", input$sighting.phid),
                                         input$sighting.phid))
               
               
               
               output$PhotoIDtxt <- renderText({
                 if(does.phid.exist() == TRUE){
                   paste0("WARNING: PhotoID (", phid$val, ") already 
                   exists in database!!! Change the PhotoID's sighting number!")
                 }else{
                   paste0(phid$val)}
               })
               
               
               
               output$SaveBtns <- renderUI({
                 if(does.phid.exist()==FALSE){      # phid must not already exist
                   req(input$user,                  # mandatory fields must be filled before buttons appear
                       input$site.phid,
                       input$date.phid,
                       input$sighting.phid,
                       input$fin.photo,
                       input$time,
                       input$sex,
                       phid$lat,
                       phid$long,
                       input$size || input$unknownsize)  # either size or U size ckbox is req'd
                   tagList(
                     actionButton("saveEncounter", "Add New Encounter", class = "btn-primary"),
                     actionButton("r2submit", "Save and Continue: Ready to Review Encounters", class="btn-primary"))
                 }else{
                   #buttons disappear when phid already exists
                 }
                 
               })
               
               output$mapdirections <-
                 renderText("Click the map to drop a pin and save coordinates*")
               
               
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
               #     clearMarkers()%>%
               #     addPulseMarkers(data = click, lng=~lng, lat=~lat, icon = makePulseIcon(),
               #                     options = leaflet::markerOptions(draggable = F))
               #   phid$lat   <- NULL
               #   phid$long  <- NULL
               #   output$xyloc <- renderText({paste("lat: ", phid$lat,
               #                                     "| long: ", phid$long)})
               # })
               
               
               #return the photoID to splash around
               return(phid)
             }
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
               
               data <- c(refID = "UNMATCHED", 
                         name = "NONE_YET", 
                         match.sugg = as.character(input$match.sugg), 
                         time.obs = as.character(input$time),
                         PhotoID = as.character(phid$val),
                         site = toupper(as.character(input$site.phid)), 
                         date = as.character(input$date.phid), 
                         sighting = as.character(input$sighting.phid),
                         sex = as.character(input$sex),
                         size = ifelse(input$unknownsize == TRUE, "U", as.character(round(input$size/0.5)*0.5)),
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
                         # ifelse(is.null(phid$lat), 
                         #                   paste0("flds$coords$", input$site.phid, "[2]"),
                         #                   as.character(round(as.numeric(phid$lat), 4))),
                         long.approx = as.character(round(as.numeric(phid$long), 4)),
                         # ifelse(is.null(phid$long), 
                         #                    paste0("flds$coords$", input$site.phid, "[1]"),
                         #                    as.character(round(as.numeric(phid$long), 4))),
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
                         #photo file final path
                         pFn = file.path(DIR_FinID_Final, paste0(phid$val, 
                                                                 ".", 
                                                                 tools::file_ext(input$fin.photo$datapath))),
                         orig.fin.file = as.character(input$fin.photo$name),
                         survey.vessel = as.character(input$vessel),
                         survey.crew = as.character(paste(input$crew, collapse = "|")),
                         survey.effortON = as.character(input$surveystart), #w/o slider range
                         survey.effortOFF = as.character(input$surveyend),
                         # survey.effortON = as.character(input$effort[1]), #w/ slider range
                         # survey.effortOFF = as.character(input$effort[2]),
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
           
           
           # if 'sizeunknown' ckbox is clicked, reset the size numeric input
           observeEvent(input$unknownsize == TRUE, {     
             reset("size")
           })
           
           # clear checkbox if size value is updated
           observeEvent(input$size, {
             reset("unknownsize")
           })
           
           #####################
           
           output$finuploaded <- reactive({
             #hide the photo and map and buttons until essential info is included
             return(!is.null(finUP()) && !is.na(phid$val) && grepl("^([A-Z]{2,3})([0-9]{8})", phid$val))
           })
           outputOptions(output, 'finuploaded', suspendWhenHidden=F)
           
           
           
           
           
           ##################################################
           
           
           #Observe "saveEncounter" event here
           observeEvent(input$saveEncounter, {
             data <- data.frame(formData(), stringsAsFactors = F)
             showNotification(paste("DataCSV", data$dfN, "is uploaded to", DIR_Enc_prelim), 
                              #action = a(href="javascript:location.reload();", "Reload page"),
                              closeButton = TRUE, type = "message", duration=9,
                              id = "datUP")
             saveData2(data)
             savePhoto2(input$fin.photo, phid$val)
             print(paste0("SaveEncounter-> Phid$val is ", phid$val))
             
             updateNumericInput(inputId = "sighting.phid", value = input$sighting.phid + 1)
             
             #refresh data review table
             fresh_table <- loadData2Table(DIR_Enc_prelim)
             this_table(fresh_table)
             
             showNotification(paste("FinPhoto", phid$val, "uploaded to", DIR_staging), 
                              #action = a(href="javascript:location.reload();", "Reload page"), 
                              closeButton = T, type = "message", duration=9,
                              id = "phidUP")
             
             #reset fields
             sapply(c("sighting", "sex","unknownsize", "size", "tag.exists", "tagdeployed", "tag.id",
                      "tag.side", "biopsy", "biopsy.id", "notes", "tag.notes",
                      "fin.photo", "PhotoID", "finuploaded", "match.sugg", "time", "FinShot"),
                    reset)
             
             # removes temp file so image resets
             unlink(input$fin.photo$datapath)
             
             
             
             print("SaveEncounter")
             
             output$FinShot <- NULL
             output$PhotoID <- NULL
             phid$val <- NULL
             
             reset("data")
             reset("saveEncounter")
             reset("r2submit")
           })
           
           
           
           observeEvent(input$r2submit, {
             #click saveEncounter to save photo/data
             observe({
               shinyjs::click("saveEncounter")})
             
             #move user to submission page
             updateTabsetPanel(session, "form", selected = "3. Data Review")
           })
           
           
           
           
           
           
           
           ##########################################
           ##########################################
           ##Page 3 server stuff
           ##########################################
           
           
           output$datareviewdirections <-
             renderUI({
               linebreak <- paste(" ")
               str1 <- paste("Notes")
               str2 <- paste("- Select a row to view the FinID photo and image names. You can also delete rows if you determine encounters are the same individuals.")
               str3 <- paste("- This review table is editable. Double click a cell to modify it.")
               str4 <- paste("- When you submit final data, any edits made to the review table will be saved.")
               HTML(paste(str1,
                          str3,
                          str4,
                          str2,
                          sep = '<br/>'
               ))
             })
           
           output$reviewedlabel <-
             renderUI({
               str1 <- paste("Data is REVIEWED and CORRECT")
               HTML(paste(str1))
             })
           
           
           #################################################
           ######### Encounter Data Table Review ###########
           #################################################
           
           
           ### Initial Load
           
           if(length(list.files(DIR_Enc_prelim))>0){                             # if directory has contents, load datatable for review
             start_table <- loadData2Table(DIR_Enc_prelim)
             this_table <- reactiveVal(start_table)
             print("Initialization- Encounters_prelim folder has contents")
           }else{                                                                # if directory is empty, load placeholder table
             print("Initialization- No files in Encounters_prelim folder")
             print("Loading placeholder file")
             start_table <- loadData3Table()
             this_table <- reactiveVal(start_table)
           }
           
           
           
           
           ##################################
           
           # Text appears if buttons are pressed and no encounter files exist yet
           
           Folder_contents_txt <- 
             eventReactive(list(input$resetedits_btn,  # looks for multiple buttons to render the text
                                input$saveEncounter,
                                input$r2submit), 
                           {if(length(list.files(DIR_Enc_prelim))==0){
                             "No Encounter Files Exist yet. Submit FinID Data"
                           }})
           
           output$NoEncounterFiles <-
             renderText(
               Folder_contents_txt()
             )
           
           
           ###################################
           
           
           observeEvent(input$resetedits_btn, {                    # reloads fintable to existing prelim encounters
             if(length(list.files(DIR_Enc_prelim))>0){
               refreshed_finsTable <- loadData2Table(DIR_Enc_prelim)
               this_table(refreshed_finsTable)
             }
             else{
               print("still nothing in the directory")
             }
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
             DT::renderDataTable(
               this_table(),
               editable = TRUE,
               selection = 'single',
               rownames = TRUE,
               options = list(
                 scrollX = TRUE,          # fluid width with scroll bar on x axis
                 scrollY = "300px",       # height of table
                 server = F,
                 paging = TRUE,           # create pages if more than pageLength
                 pageLength = 5,          # rows per page
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
               cleanIMGname <- sub(".*FinID_curator/FinIDs_FINAL/", "", selectedFin())   # selectedfin() is a complete path but we only want to display the file name
               if(!is.null(input$finsTable_rows_selected)){                                #based on 'DIR_staging' directory. Fix so it paste0's 'DIR_staging'
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
           
           
           
           # renders image of fin based on selected row of review table
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
           
           
           output$reviewed <- 
             renderUI({
               if(input$reviewed == T){
                 actionButton("r2submitFINAL", "Ready to Submit", class="btn-primary")
               }
             })
           
           
           
           
           ######### Modal Dialogues ###########
           
           observeEvent(input$r2submitFINAL,{
             showModal(modalDialog(
               title = "FINAL DATA SUBMISSION",
               "This is the last step before your data is submitted and complete.
      Keep the checkbox in the default state.",
               checkboxInput("modalckbox1", 
                             "Clear cached preliminary encounter files", 
                             value = TRUE),
               actionButton("SubmitFinalData_btn","Submit Final Data", class="btn-primary"),
               size = c("m"),    # s, m, l, xl
               easyClose = TRUE,
               footer = NULL
             )
             )
           })
           
           observeEvent(input$SubmitFinalData_btn,{
             showModal(modalDialog(
               title = "Thanks for sharking with us!",
               imageOutput("sharksmile"),
               actionButton("r2h", "Home Page", class="btn-primary"),
               easyClose = FALSE,   # must press HOME btn (it refreshes the app)
               footer = NULL))
             FINput_filename = paste0(
               "FINput_", 
               input$user,                               # Stamps with user's initials
               "_",
               format(Sys.time(), "%Y%m%d"),       #yyyymmdd date format saved in system's TZ
               "_",                                
               format(Sys.time(), "%H%M%S"),       #hhmmss   time format saved in system's TZ
               ".csv")                             #           (hours are in 24hr format)
             showNotification(paste0("Final Encounter Data Table- ",  
                                     FINput_filename ,
                                     " from this session is saved to the ", 
                                     DIR_Enc_final, " folder."),
                              duration = 8,
                              type = c("message"),
                              closeButton = TRUE)
             write.csv(this_table(), 
                       file = paste0(
                         DIR_Enc_final, "/",FINput_filename
                       ))
             write.csv(this_table(),                                        # writes table to backup folder
                       file = paste0(DIR_Enc_Backup, "/",FINput_filename 
                       ))
             if(input$modalckbox1 == TRUE){    # delete cached files and refresh review table
               do.call(file.remove, list(list.files(paste0(getwd(), "/", DIR_Enc_prelim, "/"), full.names = TRUE)))
               # refresh fins table
               start_table <- loadData3Table()
               this_table(start_table)
             }
             
             reset("modalckbox1")
             
             reset("reviewed")   # reset 'data is reviewed and correct' checkbox
           })
           
           
           
           output$sharksmile <-
             renderImage({list(src = normalizePath(paste0(DIR_ARCHIVE_IMAGES,"/sharksmile.png")), 
                               width = "auto", 
                               height = "300px")}, 
                         deleteFile = FALSE)
           
           
           observeEvent(input$r2h, {
             removeModal()
             updateTabsetPanel(session, "form", selected = "Home")
             refresh()
           })
           
           
           
           
           
           
           #################################
           ##### Page 4: Settings Stuff ####
           #################################
           
           
           #### Directories Tab
           
           output$SettingsDirectoriesTitle <-
             renderText("Directories and Folders")
           
           
           output$SettingsDirectoriesTxt <-
             renderUI({
               linebreak <- paste(" ")
               str1 <- paste0("Current working directory: ", getwd())
               str2 <- paste0("Data Directory: ", "/FinID_curator")
               str3 <- paste("All data is set, by default, to move through the FinID_curator directory and will be routed to the subfolders listed below.")
               str4 <- paste("**Note- The next version of this app will have the ability to set locations of where data will be saved in 
                    addition to the FinID_curator directory. (If this is a feature anyone would like to be implemented, please let Dylan know.")
               HTML(paste(str1,
                          linebreak,
                          str2,
                          str3,
                          linebreak,
                          str4,
                          sep = '<br/>'
               ))
             })
           
           
           #### Support Tab
           
           output$SettingsSupportTitle <-
             renderText("Support & Help")
           
           
           output$SettingsSupportTxt <-
             renderUI({
               linebreak <- paste(" ")
               str1 <- paste("For feature requests or any support questions, contact Dylan Moran (dmoran@csumb.edu)")
               str3 <- paste("More information is available on this GitHub repository. As the FinID Entry App gets new iterations, the GitHub will be updated and remain current.")
               str4 <- paste("https://github.com/dylan-moran/MBA-GWS-ShinyApp/tree/main")
               HTML(paste(str3,
                          str4,
                          linebreak,
                          linebreak,
                          str1,
                          sep = '<br/>'
               ))
             })
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           ######################################################################
           ###############         End of Server Code        ####################
           ######################################################################
         }
)

shinyApp(ui, server)
