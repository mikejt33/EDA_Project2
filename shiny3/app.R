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
  titlePanel("Our Amazing App Part "),
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
                  tabPanel("Job",plotOutput("jobs"))
      ))
  ))

server <- function(input, output) {
  output$jobs <- renderPlot({
    
    filtered <- job_data %>%
      select(gender, counts, company, job_category) %>%
      filter(!(counts == 0)) %>%
      filter(company == input$companyInput)
    
    # ggplot(filtered, aes(x = job_category, y = counts, fill = gender)) +
    #   geom_violin() + geom_jitter()
    

    ggplot(filtered, aes(x = job_category, y = counts, fill = gender)) +
      geom_boxplot()
    
  })
  output$cool <- renderPlot({
    filtered <- job_data %>%
      select(gender, counts, company, job_category) %>%
      filter(!(counts == 0)) %>%
      filter(company == input$companyInput)
    
    # ggplot(filtered, aes(x = gender, y = counts, fill = gender)) +
    #   geom_violin() + geom_jitter()
    
    ggplot(filtered, aes(x = gender, y = counts, fill = gender)) +
      geom_boxplot()
  
  })
  
  output$job <- renderPlot({
    
    filtered <- job_data %>%
      select(gender, counts, company, job_category) %>%
      filter(!(counts == 0)) %>%
      filter(company == input$companyInput)
    
    
     ggplot(filtered, aes(x = company, y = counts, fill = gender)) +
       geom_boxplot()
       
      # geom_boxplot(stat = 'identity', position=position_dodge()) + 
      #  ggtitle("Breakdown of Gender by Company") + ylab("Employee Count") + xlab("Company")
  })
  
  output$race <- renderPlot({
    
    filtered <- job_data %>%
      select(gender, counts, company, job_category, race) %>%
      filter(!(counts == 0)) %>%
      filter(company == input$companyInput)

    ggplot(filtered, aes(x = race, y = counts, fill = gender)) +
      geom_boxplot()
    # ggplot(filtered, aes(x = race, y = counts, fill = gender)) +
    #   geom_bar(stat = 'identity',position=position_dodge()) + 
    #   ggtitle("Breakdown of Race by Race") + ylab("Counts") + xlab("Race")
  })
  
}
shinyApp(ui = ui, server = server)

