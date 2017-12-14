#Author: Michael McCormack
library(shiny)
library(dplyr)
library(ggplot2)
library(ggmap)

data <- read.csv("clean.csv")

job_data <- data %>%
  select(company, gender, race, job_category, counts) %>% 
  filter(!is.na(counts), !is.na(gender)) %>%
  group_by(job_category, gender, company, race) %>%
  summarise(counts = sum(counts))


ui <- fluidPage(
                titlePanel("Diversity in Silicon Valley"),
                 sidebarLayout(
                   sidebarPanel(selectInput("companyInput","Companies:",
                                            choices = unique(job_data[["company"]])),
                                hr(),
                                helpText("List of the companies we examined."),
                                br()
                              ),
                    mainPanel(
                     tabsetPanel(type = "tabs",
                      tabPanel("Gender", plotOutput("cool")),
                      tabPanel("Company",plotOutput("job")),
                      tabPanel("Race",plotOutput("race")),
                      tabPanel("Job",plotOutput("jobs")),
                      tabPanel("Static", plotOutput("stat"))
                 ))
))

server <- function(input, output) {
  output$stat <- renderPlot({
    
    ggplot(job_data, aes(x = job_category, y = counts, fill=gender)) +
      geom_bar(stat = 'identity',position=position_dodge()) + ggtitle("Breakdown of Job Category in Silicon Valley") +
      theme(axis.text.x = element_text(angle = 30, size = 15))
    
    ggplot(job_data, aes(x = gender, y = counts, fill=gender)) +
      geom_bar(stat = 'identity') + ggtitle("Breakdown of Gender Employment in Silicon Valley") +
      theme(axis.text.x = element_text(angle = 30, size = 15))
    
    ggplot(job_data, aes(x = company, y = counts, fill = gender)) +
      geom_bar(stat = 'identity', position=position_dodge()) + 
      ggtitle("Breakdown of Gender by Company") + ylab("Employee Count") + xlab("Company") +
      theme(axis.text.x = element_text(angle = 30, size = 15))
    
    ggplot(job_data, aes(x = race, y = counts, fill = gender)) +
      geom_bar(stat = 'identity',position=position_dodge()) + 
      ggtitle("Breakdown of Race by Gender") + ylab("Counts") + xlab("Race")+
      theme(axis.text.x = element_text(angle = 30, size = 15))
  })
  output$jobs <- renderPlot({
    
    filtered <- job_data %>%
      select(gender, counts, company, job_category) %>%
      filter(!(counts == 0)) %>%
      filter(company == input$companyInput)
    
    ggplot(filtered, aes(x = job_category, y = counts, fill=gender)) +
      geom_bar(stat = 'identity',position=position_dodge()) + ggtitle("Breakdown of Job Category in Silicon Valley")+
      theme(axis.text.x = element_text(angle = 30, size = 15))
    
  })
  output$cool <- renderPlot({
    filtered <- job_data %>%
      select(gender, counts, company, job_category) %>%
      filter(!(counts == 0)) %>%
      filter(company == input$companyInput)
    
    ggplot(filtered, aes(x = gender, y = counts, fill=gender)) +
      geom_bar(stat = 'identity') + ggtitle("Breakdown of Gender Employment in Silicon Valley")+
      theme(axis.text.x = element_text(angle = 30, size = 15, face = "bold"), plot.title= element_text(size=30))
    
  })
  
  output$job <- renderPlot({
    
    filtered <- job_data %>%
      select(gender, counts, company, job_category) %>%
      filter(!(counts == 0)) %>%
      filter(company == input$companyInput)
    
    ggplot(filtered, aes(x = company, y = counts, fill = gender)) +
      geom_bar(stat = 'identity', position=position_dodge()) + 
      ggtitle("Breakdown of Gender by Company") + ylab("Employee Count") + xlab("Company")+
      theme(axis.text.x = element_text(face="bold", angle = 15, size = 20))
  })
  
  output$race <- renderPlot({
    
    filtered <- job_data %>%
      select(gender, counts, company, job_category, race) %>%
      filter(!(counts == 0)) %>%
      filter(company == input$companyInput)
    
    ggplot(filtered, aes(x = race, y = counts, fill = gender)) +
      geom_bar(stat = 'identity',position=position_dodge()) + 
      ggtitle("Breakdown of Race by Gender") + ylab("Counts") + xlab("Race")+
      theme(axis.text.x = element_text(angle = 30, size = 15, face="bold"))
  })

}
shinyApp(ui = ui, server = server)

