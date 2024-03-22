library(shiny)
library(ggplot2)
library(DT)
library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)


library(dqshiny)

mimic_icu_cohort<-readRDS("mimiciv_shiny/mimic_icu_cohort.rds")

satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)

# connect to the BigQuery database `biostat-203b-2024-winter.mimic4_v2_2`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)



# UI
ui <- fluidPage(
  titlePanel("ICU Cohort Shinny APP"),
  tabsetPanel(
    tabPanel("Paient characteristics",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "VOI",
                             label = "Variable of interest:(may require some time)",
                             choices = c("Race", 
                                         "Age", 
                                         "Gender", 
                                         "LabMeasurements", 
                                         "Vitals","LastCareUnit")),
                 checkboxInput(inputId = "RMO",
                             label = "Remove Outliers in IQR method for measurement?",
                             value = TRUE),
                 ),
               
               
                
               mainPanel(
                 plotOutput("plot1")
               )
              )
    ),
    tabPanel("Patient's ADT and ICU stay Information",
             sidebarLayout(
               sidebarPanel(
                 # textInput(inputId = "PID",
                 #           label = "Select a patient:",
                 #           value = "10013310"),
                 autocomplete_input("PID", 
                                    "Select a patient:(the plot loading may require some time)",
                                    as.character(mimic_icu_cohort$subject_id),
                                    "10013310", 
                                    max_options = 1000)
               ),
               mainPanel(
                 plotOutput("plot2")
               )
             )
    )
  )
)

# Server
server <- function(input, output) {
  output$plot1 <- renderPlot({
    if (input$VOI == "Race") {
      ggplot(mimic_icu_cohort, aes(x = race)) + 
        geom_bar(stat="count",fill="blue") +
        labs(title = "Race distribution", x = "race", y = "count") +
        coord_flip()
    }
    else if (input$VOI == "Age") {
      ggplot(mimic_icu_cohort, aes(x = age_intime)) + 
        geom_histogram(binwidth = 5,fill="red") +
        labs(title = "Age distribution", x = "age", y = "count") +
        coord_flip()
    }
    else if (input$VOI =="Gender") {
      ggplot(mimic_icu_cohort, aes(x = gender)) + 
        geom_bar(stat="count",fill="skyblue") +
        labs(title = "Gender distribution", x = "gender", y = "count") +
        coord_flip()
    }
    
    
    else if (input$VOI == "LabMeasurements") {
      cohort_long1 <- mimic_icu_cohort %>%
        pivot_longer(cols = 
                       c("White Blood Cells",
                         "Creatinine","Potassium",
                         "Glucose","Sodium","Chloride",
                         "Bicarbonate","Hematocrit"), 
                     names_to = "Variable", values_to = "Value")
      if(input$RMO==TRUE){
        ggplot(cohort_long1,aes(x = Variable, y = Value)) +
          geom_boxplot(outlier.shape = NA) +
          scale_y_continuous(limits = c(0, 
                                        max(quantile(
                                          cohort_long1$Value, 0.75,na.rm=TRUE)
                                            + 1.5 * 
                                            IQR(cohort_long1$Value,na.rm=TRUE))
                                        )) +
          labs(title = "Boxplot of Lab Measurements", 
               x = "Variable", 
               y = "Value")+
          coord_flip()
      }
      else{
        ggplot(cohort_long1,aes(x = Variable, y = Value)) +
          geom_boxplot() +
          labs(title = "Boxplot of Lab Measurements", 
               x = "Variable", 
               y = "Value") +
          coord_flip()
      }
    }
    
    else if (input$VOI == "Vitals") {
      cohort_long2 <- mimic_icu_cohort %>%
        pivot_longer(cols = c("Heart Rate",
                              "Non Invasive Blood Pressure systolic",
                              "Temperature Fahrenheit","Respiratory Rate", 
                              "Non Invasive Blood Pressure diastolic"), 
                                names_to = "Variable", values_to = "Value")
      if(input$RMO==TRUE){
        ggplot(cohort_long2,aes(x = Variable, y = Value)) +
          geom_boxplot(outlier.shape = NA) +
          scale_y_continuous(limits = c(0, 
                                        max(quantile(
                                          cohort_long2$Value, 0.75,na.rm=TRUE) + 
                                              1.5 * 
                                            IQR(cohort_long2$Value,na.rm=TRUE))
                                        )) +
          labs(title = "Boxplot of Lab Measurements", 
               x = "Variable", 
               y = "Value")+
          coord_flip()
      }
      else{
        ggplot(cohort_long2,aes(x = Variable, y = Value)) +
          geom_boxplot() +
          labs(title = "Boxplot of Lab Measurements", 
               x = "Variable", 
               y = "Value") +
          coord_flip()
      }
    }
    else if (input$VOI == "LastCareUnit") {
      ggplot(mimic_icu_cohort, aes(x=last_careunit)) + 
        geom_bar(stat="count", fill="blue") +
        labs(title = "Last Care Unit distribution",
             x = "last_careunit", 
             y = "count") +
        coord_flip()
    } 
  })
  
  output$plot2 <- renderPlot({
    # subject id
    sid<-as.integer(input$PID)
    # sid<-10013310
    
    # obtain the ADT data
    sid_adt <- tbl(con_bq, "transfers") |>
      filter(subject_id == sid)
    
    # obtain the labevents data
    sid_lab<-tbl(con_bq, "labevents") |>
      filter(subject_id == sid)
    
    # obtain the procedures data
    sid_proc<-tbl(con_bq, "procedures_icd") |>
      filter(subject_id == sid)
    
    sid_proc_d<-tbl(con_bq, "d_icd_procedures") |>
      semi_join(sid_proc, by="icd_version") |>
      semi_join(sid_proc, by="icd_code")
    
    sid_proc <- sid_proc |>
      left_join(sid_proc_d, by = "icd_code") |>
      mutate(type = long_title,
             chartdate = TIMESTAMP(chartdate),
             )
    
    sid_diag<-tbl(con_bq,"diagnoses_icd") |>
      filter(subject_id == sid)
    
    sid_diag_d<-tbl(con_bq,"d_icd_diagnoses") |>
      semi_join(sid_diag, by = "icd_version") |>
      semi_join(sid_diag, by = "icd_code")
      
    sid_pat<-tbl(con_bq,"patients") |>
      filter(subject_id == sid)
    
    sid_adm<-tbl(con_bq,"admissions") |>
      filter(subject_id == sid)
    
    ggplot() +
      geom_segment(
        data=sid_adt %>% filter(eventtype != "discharge"),
        aes(
          x = intime, 
          xend = outtime, 
          y = "ADT", 
          yend = "ADT", 
          color = careunit,
          linewidth = str_detect(careunit, "(ICU|CCU)")
        ),
        
      ) +
      geom_point(
        data = sid_lab,
        aes(
          x = charttime, 
          y = "Lab"
        ),
        shape = "+",
        size=4
        
      ) +
      geom_point(
        data = sid_proc,
        aes(
          x = chartdate,
          y = "Procedure",
          shape = type
        ),
        size=3
      ) +
      scale_shape_manual(values = 15:24)+
      labs(
        x = "", 
        y = "",
        title = str_c("Patient ", sid,
                      ", ", pull(sid_pat,gender), 
                      ", ",pull(sid_pat,anchor_age), 
                      " years old ",unique(pull(sid_adm,race))),
        shape="Procedure",
        color="Care Unit"
      )+
      guides(linewidth = FALSE,
             color=guide_legend(ncol=1),
             shape=guide_legend(ncol=1)) +
      theme(legend.position = "bottom")
  })

}


# Run the app
shinyApp(ui = ui, server = server)




