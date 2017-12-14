library(shiny)
library(dplyr)
library(ggplot2)
library(ggmap)

data <- read.csv("clean.csv")

gender_data <- data %>%
  select(company, gender, job_category, counts) %>% 
  filter(job_category=="Totals", !is.na(counts), !is.na(gender)) %>%
  group_by(company, gender) %>%
  summarise(counts = sum(counts))

job_data <- data %>%
  select(company, gender, job_category, counts) %>% 
  filter(!is.na(counts), !is.na(gender)) %>%
  group_by(job_category, gender) %>%
  summarise(counts = sum(counts))

race_data <- data %>%
  select(race, gender, job_category, counts) %>% 
  filter(!is.na(counts), !is.na(gender)) %>%
  group_by(race, gender) %>%
  summarise(counts = sum(counts))

# 
# ui <- fluidPage(  h1("Our App"),
#                   br(),
#                   h5("Lets look at Gender distribution for 23 tech firms in Silicon Valley"),
#                   br(),
#                   "Store",
#                   strong("prices"))
ui <- fluidPage(
  titlePanel("Our Amazing App"),
  sidebarLayout(
    sidebarPanel(selectInput("companyInput","Companies:",
                             choices = unique(gender_data[["company"]])),
                 hr(),
                 helpText("List of the companies we examined."),
                 br(),
                 
                 selectInput("jobInput", "Jobs:",
                             choices = unique(job_data[["job_category"]])),
                 helpText("List of all the jobs"),
                 br(),
                 
                 selectInput("raceInput","Race:",
                             choices = unique(race_data[["race"]])),
                 helpText("List of all the races")),
    
    mainPanel((
      tabsetPanel(type = "tabs",
        tabPanel("Gender", plotOutput("cool")),
        tabPanel("Job",plotOutput("job")),
        tabPanel("Race",plotOutput("race")),
        tabPanel("Map",plotOutput("map")))
      )
    )
  )
)

server <- function(input, output) {



  output$cool <- renderPlot({
    
    filtered <- gender_data %>%
      select(gender, counts, company) %>%
      filter(company == input$companyInput)
    
    #filtered$gender <- factor(filtered$gender, levels = filtered$gender)
    #ggplot(filtered, aes(x= gender)) + geom_bar()
    #barplot(filtered$counts)
    #ggplot(filtered, aes(x = gender, y = counts, fill = gender)) + geom_bar()
    #ggplot(filtered, aes(x = gender, y = counts)) + geom_bar(stat = 'identity')
    
    ggplot(filtered, aes(x = gender, y = counts, fill = gender)) +
      geom_bar(stat = 'identity') + ggtitle("Breakdown of Employees Gender")
      
    #plot(rnorm(100))
  })
  
  output$job <- renderPlot({
    filtered <- job_data %>%
      select(gender, counts, job_category) %>%
      filter(job_category == input$jobInput)
    
    ggplot(filtered, aes(x = job_category, y = ((counts)/sum(counts)*100), fill = gender)) +
      geom_bar(stat = 'identity') + 
      ggtitle("Breakdown of Jobs by Job") + ylab("Percantage") + xlab("Job")
    
    
  })
  
  output$race <- renderPlot({
    
    filtered <- race_data %>%
      select(gender, counts, race) %>%
      filter(race == input$raceInput)
    
    ggplot(filtered, aes(x = race, y = ((counts)/sum(counts)*100), fill = gender)) +
      geom_bar(stat = 'identity') + 
      ggtitle("Breakdown of Race by Race") + ylab("Percantage") + xlab("Race")
    
    
  })
  
  output$map <- renderPlot({
  
  lat <- c(37.395354,37.330784,37.771885,37.331863,37.405568,37.383049,37.484685,37.422153,37.412433,37.410456,37.387813, 37.430749,37.392210,37.776723,37.396403,37.371929,37.775649,37.790103,37.783026, 37.776991,37.775350,37.428527)
  lng <- c(-122.079070, -121.893790,  -122.405098, -122.031159, -121.926991, -121.925703, -122.147073, -122.083832, -122.147917,-122.036386, -121.963198, -122.095890, -122.031879, -122.391670, -122.056683, -121.965571, -122.399843, -122.396814,-122.397916, -122.416272, -122.417474,-121.897528)
  name <- c("23andMe","Adobe","Airbnb","Apple","Cisco","Ebay","Facebook","Google","HP","HPE","Intel","Intuit","LinkedIn","Lyft","MobileIron","Nvidia","Pinterest", "Salesforce","Square","Twitter","Uber","View")
  df <- data.frame(lat,lng,name)
  
  fitered <- df %>%
    select(lat,lng,name) %>%
    filter(name == input$companyInput)
  
  map <- get_map(location = "San Francisco", zoom =7)
   # 
   # mapPoint <- ggmap(map) + ggplot(fitered,aes(x = lng, y = lat))
   #   geom_point(size = 2, shape = 19)
   # mapPoint
   # 
  
  map <- get_map(location= c(lon = fitered$lng, lat = fitered$lat), zoom = 10)
  
  ggmap(map) + geom_point(data = fitered, aes(x = 122.079070, y= 37.395354))
   ggmap(map,
         ggplot(data = fitered, aes(x = 122.079070, y = 37.395354, label = name))+
           geom_point(fill = "blue", alpha = 0.8, size =5, shape = 21) + geom_text())
  
  })
}
shinyApp(ui = ui, server = server)

