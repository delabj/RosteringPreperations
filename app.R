library(shiny)
library(rhandsontable)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)


###### UI Layout ######
ui <- navbarPage("Rostering Preperation",
                 
                 #### HOME PAGE #####
                 tabPanel("Home",
                          h1("About this App"),
                          fluidRow(
                              column(width = 3, img(src='spartanLogo.png', align = "center")),
                              column(width = 9, 
                                     p("This App is designed to prep ad-hoc exports from Infinite Campus to various other programs that require student Rosters."), 
                                     p("Simply upload the ad hoc from campus on the upload tab. On the Prep tab select the options for the roster you need."),
                                     p(tags$b("It is reccomended that this App be run in a full sized window"))
                              )
                          )
                 ),
                 
                 #### UPLOAD PAGE #####
                 ## right now this has file options, I may move those to the prep page. 
                 tabPanel("Upload", 
                          titlePanel("Uploading Files"),
                          uiOutput("warningTitle"),
                          uiOutput("variableType"),
                          sidebarLayout(
                              sidebarPanel(
                                  fileInput("file1", "Choose CSV File",
                                            multiple = TRUE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  selectInput("rosterType", "Choose a roster to prep",
                                              choices = c("None","IHT", "HMH (Gov/Hist)", "Waterford")),
                                  uiOutput("schoolSelectIHT"),
                                  uiOutput("teacherSelect"),
                                  uiOutput("periodSelect"),
                                  uiOutput("hmhFileOptions"),
                                 
                                  
                                  h6("Options should remain in the default position unless you know what you're doing"),
                                  # Horizontal line 
                                  tags$hr(),
                                  
                                  # Input: Checkbox if file has header
                                  checkboxInput("header", "Header", TRUE),
                                  
                                  # Horizontal line
                                  tags$hr(),
                                  
                                  # Input: Select number of rows to display
                                  radioButtons("disp", "Display",
                                               choices = c(Head = "head",
                                                           All = "all"),
                                               selected = "head")
                              ), 
                              
                              ## this shows the uploaded data set
                              mainPanel(
                                  tableOutput("contents")
                              )
                          )
                 ),
                 
                 #### PREPARED FILE #####
                 tabPanel("Prep",
                          mainPanel( uiOutput("exportTitle"), 
                                     downloadButton("downloadData", "Download"),
                                     tableOutput("prepped")))
)


###### SERVER FUNCTIONS ######
server <- function(input, output) {

    #### Data ####

    ## Extract the CSV
    filedata <- reactive({
        infile <- input$file1
        if(fileReady()==F){return(NULL)}
        
        read.csv(infile$datapath,
                 header = input$header,
                 sep = ",",
                 quote = '"', 
                 stringsAsFactors=FALSE)
    })
    
    ## Show Uploaded Data
    output$contents <- renderTable({
        df <- filedata()
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    ## PREP The FILE
    filePrep <- reactive({
        
        if(fileReady()==F){return(NULL)}
        
        ## if it's IHT Do the IHT PREP
        if( input$rosterType == "IHT"){
            if(input$SchoolDropdown == "Elementary"){
        
        filedata() %>% 
            as_tibble() %>%
            ## Remove duplicates 
            distinct(student_studentNumber, .keep_all = T)%>% 
            #fix the kindergarten output from campus to match what IHT needs
            mutate(student_grade = replace(student_grade, student_grade=="KF", "K"))%>%
            # replace commas in classroom teacher display name to make splitting easier
            mutate(courseSection_teacherDisplay = str_replace_all(courseSection_teacherDisplay, ",","-"))%>%
            #split the classroom teacher from the PE teacher
            separate(courseSection_teacherDisplay, into = c("peTeach", "elemteach"),  sep="-", extra="drop") %>%
            #filter out the teacher
            filter(peTeach == input$teacherDropdown) %>%
            #Select and rename variables
            select(c('grade level'= student_grade,
                     'section' = function_IHTClassName,
                     'student id'= student_studentNumber,
                     'last name*'=student_lastName,
                     'first name'=student_firstName,
                     'secondary email' =contacts_email,
                     'gender*'=student_gender,
                     'birthdate' = student_birthdate ))%>%
            #add the blank columns
            add_column('email'=NA, .before = 'secondary email')%>%
            add_column(height=NA, weight =NA, .before = 'birthdate') %>%
            add_column(rhr=NA, max=NA, .after = 'birthdate')-> ihtELM
            
            
            return(ihtELM)
            }
            else if(input$SchoolDropdown == "Junior High"){
                filedata() %>% 
                    as_tibble() %>%
                    ## Remove duplicates 
                    distinct(student_studentNumber, .keep_all = T)%>% 
                    #fix the kindergarten output from campus to match what IHT needs
                    mutate(student_grade = replace(student_grade, student_grade=="KF", "K"))%>%
                    unite("IHTClassName",function_IHTClassName, function_Schoolyear)%>%
                    #Select and rename variables
                    select(c('grade level'= student_grade,
                             'section' = IHTClassName,
                             'student id'= student_studentNumber,
                             'last name*'=student_lastName,
                             'first name'=student_firstName,
                             'secondary email' =contacts_email,
                             'gender*'=student_gender,
                             'birthdate' = student_birthdate ))%>%
                    #add the blank columns
                    add_column('email'=NA, .before = 'secondary email')%>%
                    add_column(height=NA, weight =NA, .before = 'birthdate') %>%
                    add_column(rhr=NA, max=NA, .after = 'birthdate')-> ihtJH
                return(ihtJH)
                
            }
            else if(input$SchoolDropdown == "High School"){
                filedata() %>% 
                as_tibble() %>%
                    ## Remove duplicates 
                    distinct(student_studentNumber, .keep_all = T)%>% 
                    #fix the kindergarten output from campus to match what IHT needs
                    mutate(student_grade = replace(student_grade, student_grade=="KF", "K"))%>%
                    #filter out the teacher
                    filter(str_detect(function_IHTClassName, pattern = input$periodDropdown)) %>%
                    #Select and rename variables
                    select(c('grade level'= student_grade,
                             'section' = function_IHTClassName,
                             'student id'= student_studentNumber,
                             'last name*'=student_lastName,
                             'first name'=student_firstName,
                             'secondary email' =contacts_email,
                             'gender*'=student_gender,
                             'birthdate' = student_birthdate ))%>%
                    #add the blank columns
                    add_column('email'=NA, .before = 'secondary email')%>%
                    add_column(height=NA, weight =NA, .before = 'birthdate') %>%
                    add_column(rhr=NA, max=NA, .after = 'birthdate')-> ihtHS
                    return(ihtHS)
            }
            else{return(NULL)}
        }
        
        else if(input$rosterType == "HMH (Gov/Hist)"){
            if(input$hmhFileDropdown == "Class" ){
                
                filedata() %>%
                    distinct(courseSection_courseID, courseSection_sectionNumber, .keep_all = T)%>%
                    mutate(courseSection_courseName, COURSESUBJECT = ifelse(courseSection_courseName =="American Government","Government", "History"))%>%
                    mutate(courseSection_courseName, GRADE = ifelse(courseSection_courseName =="American Government","11", "9"))%>%
                    mutate("CLASSLOCALID"=paste(cal_endYear, courseSection_courseID, sectionSchedule_periodStart, sep="_"))%>%
                    mutate("CLASSNAME"=paste(cal_endYear, courseSection_courseName,"Sec", courseSection_sectionNumber,  sep=" "))%>%
                    add_column("TERMID"=NA, "ORGANIZATIONID"=250932,"CLASSDESCRIPTION"=NA, "ORGANIZATIONTYPEID"=NA,HMHAPPLICATIONS="ED")%>%
                    select(c("SCHOOLYEAR"=cal_endYear,
                             CLASSLOCALID,
                             "COURSEID"=courseSection_courseID, 
                             "COURSENAME"=courseSection_courseName,
                             COURSESUBJECT, 
                             CLASSNAME,
                             CLASSDESCRIPTION,
                             "CLASSPERIOD"= sectionSchedule_periodStart,
                             ORGANIZATIONTYPEID,
                             ORGANIZATIONID,
                             GRADE,
                             TERMID,
                             HMHAPPLICATIONS
                    )) -> CLASS
                return(CLASS)
            }
            else if(input$hmhFileDropdown == "Users"){
                filedata() %>%
                    distinct(student_studentNumber, .keep_all = T)%>%
                    mutate(student_studentNumber,"ROLE"=ifelse(student_studentNumber >=500000, "T", "S")) %>%
                    mutate("USERNAME"= gsub(" ","", str_remove_all(tolower(paste0(student_firstName,student_lastName)), "[~!@#$%^&*(){}_+:<>?,./;'-]")))%>%
                    add_column("MIDDLENAME"=NA, "ORGANIZATIONTYPEID"="MDR", "ORGANIZATIONID"=250932, "PRIMARYEMAIL"=NA,HMHAPPLICATIONS="ED" )%>%
                    select("SCHOOLYEAR"=cal_endYear,
                           ROLE,
                           "LASID"=student_studentNumber,
                           "SASID"=student_stateID,
                           "FIRSTNAME"=student_firstName,
                           MIDDLENAME,
                           "LASTNAME"=student_lastName, 
                           "GRADE"=student_grade,
                           USERNAME, 
                           "PASSWORD"=student_studentNumber,
                           ORGANIZATIONTYPEID, 
                           ORGANIZATIONID, 
                           PRIMARYEMAIL,
                           HMHAPPLICATIONS) -> USERS
                return(USERS)
            }
            else if(input$hmhFileDropdown == "Class Assignments"){
                filePrep() %>%
                        mutate("CLASSLOCALID"=paste(cal_endYear, courseSection_courseID, sectionSchedule_periodStart, sep="_"))%>%
                        mutate(student_studentNumber,"ROLE"=ifelse(student_studentNumber >=500000, "T", "S")) %>%
                        mutate(student_studentNumber,"OSITION"=ifelse(student_studentNumber >=500000, "L", NA)) %>%
                        select("SCHOOLYEAR"=cal_endYear,
                               CLASSLOCALID,
                               ROLE, 
                               POSITION)-> CLASSASSIGNMENTS
                return(CLASSASSIGNMENTS)
            }
            else{return(NULL)}
        }
        else if (input$rosterType =="Waterford"){
          df <- filedata() 
          
          df %>%
            filter(str_detect(student_calendarName, pattern="18-19"))%>%
            distinct(student_studentNumber, .keep_all = T)%>%
            mutate(student_grade = replace(student_grade, student_grade=="KF", "K"))%>%
            mutate(student_grade = replace(student_grade, student_grade=="01", "1st Grade")) %>%
            mutate(student_grade = replace(student_grade, student_grade=="02", "2nd Grade"))%>%
            mutate("firstName" = gsub(" ","", str_remove_all(student_firstName, "[~!@#$%^&*(){}_+:<>?,./;'-]")))%>%
            mutate("middleName" = gsub(" ","", str_remove_all(student_middleName, "[~!@#$%^&*(){}_+:<>?,./;'-]")))%>%
            mutate("lastName" = gsub(" ","", str_remove_all(student_lastName, "[~!@#$%^&*(){}_+:<>?,./;'-]")))%>%
            mutate("preferredName" = gsub(" ","",str_remove_all(coalesce(student_alias, firstName), "[~!@#$%^&*(){}_+:<>?,./;'-]")))%>%
            mutate("classGrade"=student_grade)%>%
            mutate("schoolName"=substr(student_calendarName, 6,100000))%>%
            add_column("sisName"=NA,
                       "sisID"=NA,
                       "schoolSISName"=NA,
                       "schoolSISID"=NA,
                       "pLanguage"=NA,
                       "Ethnicity"=NA,
                       "Disabilities"=NA,
                       "hStatus"=NA,
                       "sp"=NA,
                       "Email"=NA,
                       "Username"=NA,
                       "Password"=NA)%>%
            select("First Name"=firstName,
                   "Middle Name"=middleName,
                   "Last Name"=lastName,
                   #"Prefered Name"=preferredName,
                   "Unique Student ID"=student_studentNumber,
                   "Grade"= student_grade,
                   "Class Name"= student_homeroomTeacher,
                   "Class Grade"= classGrade,
                   "Class SIS NAME"=sisName,
                   "Class SIS ID" = sisID,
                   "School Name" = schoolName,
                   "School SIS NAME"= schoolSISName,
                   "School SIS ID" = schoolSISID,
                   "Gender"=student_gender,
                   "Birthday"= student_birthdate,
                   "Primary Language"= pLanguage,
                   Ethnicity,
                   Disabilities,
                   "Household Status"= hStatus,
                   "Special Programs"=sp,
                   Email,
                   Username,
                   Password
            ) -> waterford
          
          return(waterford)
        }
            
        else{return(NULL)}
        
    })
    
    ## Render the prepped data
    output$prepped <- renderTable({
        df <- filePrep()
        
            return(df)
        
    })
    
    
    #### EXPORT DATA ####
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$rosterType, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(filePrep(), file, row.names = FALSE, na="")
        }
    )
    
    #### UI ####
    
    ## select the level of institution for IHT
    output$schoolSelectIHT <- renderUI({
        
        if(fileReady()==F){return(NULL)}
        if(FileCorrect()==F){return(NULL)}
        
        
        if(input$rosterType == "IHT"){ 
            
            if (is.null(df)) return(NULL)

            items=c("Elementary", "Junior High", "High School")
            selectInput("SchoolDropdown", "School",items)}
        else{return(NULL)}
    })
    
    ## select the teacher for elementary teachers in IHT
    output$teacherSelect <- renderUI({
        
        if(fileReady()==F){return(NULL)}
        if(FileCorrect()==F){return(NULL)}
        
        
        
        ##This gets the list of teachers
        
        
        if(input$rosterType == "IHT"){ 
          df <-filedata() %>% 
            as_tibble() %>%
            distinct(student_studentNumber, .keep_all = T)%>%
            mutate(student_grade = replace(student_grade, student_grade=="KF", "K"))%>%
            mutate(courseSection_teacherDisplay = str_replace_all(courseSection_teacherDisplay, ",","-")) %>%
            separate(courseSection_teacherDisplay, into = c("peTeach", "elemteach"),  sep="-", extra="drop")
            
            if (is.null(df)){return(NULL)} 
            if (input$SchoolDropdown != "Elementary")  return(NULL)  
            
            
            items=unique(df$peTeach)
            selectInput("teacherDropdown", "Select the teacher",items)
            
        }
        else{return(NULL)}
    })
    
    ## Period select for high School IHT
    output$periodSelect <- renderUI({
        
        if(fileReady()==F){return(NULL)}
        if(FileCorrect()==F){return(NULL)}
        
        
        
      
        
        if(input$rosterType == "IHT"){ 
            ##This gets the list of periods
            df <-filedata() %>% 
                as_tibble() %>%
                distinct(student_studentNumber, .keep_all = T)%>%
                mutate(student_grade = replace(student_grade, student_grade=="KF", "K"))%>%
                mutate(courseSection_teacherDisplay = str_replace_all(courseSection_teacherDisplay, ",","-")) %>%
                separate(function_IHTClassName, into = c("peTeach", "elemteach"),  sep=9, extra="drop")
            
            if (is.null(df)){return(NULL)} 
            if (input$SchoolDropdown != "High School")  return(NULL)  
            
            
            items=unique(df$peTeach)
            items = items[order(nchar(items), items)]
            selectInput("periodDropdown", "Select the period",items)
            
        }
        else{return(NULL)}
    })
    
    output$warningTitle <- renderUI({
        if(fileReady()==F){return(NULL)}
        if(input$rosterType=="None" ){
            
        }
        else{
            if(FileCorrect()==F){
                titlePanel("MISSING ROWS")
            }  
            else{
                titlePanel("All Rows Present")
            }
        }
        
    })
    
    ## renders the title for output page
    output$exportTitle <- renderUI({
        
        if(fileReady()==F){return(NULL)}
            if (is.null(df)) return(NULL)
        
        if(input$rosterType == "IHT"){ 
            if(input$SchoolDropdown != "Junior High"){
                if(input$SchoolDropdown=="Elementary"){
                    titlePanel(paste0(input$SchoolDropdown, " ", input$rosterType, ": ", input$teacherDropdown))   
                }
                else{titlePanel(paste0(input$SchoolDropdown, " ", input$rosterType, ": ", input$periodDropdown))}
                
            }
            else{titlePanel(paste0(input$SchoolDropdown, " ", input$rosterType))}
        }
        return(NULL)
           
    })
    
    ## File options for HMH
    output$hmhFileOptions <- renderUI({
        
        if(fileReady()==F){return(NULL)}
        if(FileCorrect()==F){return(NULL)}
        
        
        if(input$rosterType == "HMH (Gov/Hist)"){
            items=c("Class", "Users", "Class Assignments")
            radioButtons("hmhFileDropdown", "Select Output File", items)
            
        }
        
    })
    
    

   #### ERROR CHECKING ####
    
    ## is there a file ready and waiting?
    fileReady <- reactive({
        infile <- input$file1
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(FALSE)
        }
        return(TRUE)
    })
    
    FileCorrect <- reactive ({
        if(fileReady()==F){return(NULL)}
        
        
        if(input$rosterType == "IHT"){
            check <- c("student_grade", #
                       "student_homeroomTeacher", #
                       "student_studentNumber", #       
                       "student_lastName", #
                       "student_firstName", #
                       "student_gender",# 
                       "student_birthdate", #
                       "student_calendarName", #
                       "courseSection_teacherDisplay", #
                       "roster_endDate",
                       "sectionSchedule_periodStart", #
                       "sectionSchedule_scheduleStart", #
                       "contacts_email", #
                       "function_IHTClassName"#   
                       )
            
            
            test <- check %in% names(filedata())
            
            if(all(test)==T){return(T)}
            else{return(F)}
        }
        else if(input$rosterType== "HMH (Gov/Hist)"){
            check <- c("student_firstName",
                       "student_lastName",
                       "student_grade",
                       "student_stateID",
                       "courseSection_courseID",
                       "student_endDate",
                       "courseSection_courseNumber",
                       "roster_endDate",
                       "courseSection_teacherDisplay",
                       "student_studentNumber",
                       "courseSection_sectionNumber",
                       "courseSection_courseName",
                       "sectionSchedule_periodStart",
                       "cal_endYear")
            
            test <- check %in% names(filedata())
            
            if(all(test)==T){return(T)}
            else{return(F)}
            
        }
        else if(input$rosterType=="Waterford"){
          check <- c("student_firstName",
                     "student_middleName", 
                     "student_lastName", 
                     "student_alias", 
                     "student_studentNumber", 
                     "student_grade",
                     "student_gender",
                     "student_birthdate",
                     "student_calendarName",
                     "student_homeroomTeacher")
          test <- check %in% names(filedata())
          
          if(all(test)==T){return(T)}
          else{return(F)}
        }
        else{
            return(F)
            }
      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
