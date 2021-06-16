library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
# Lab_shiny_crime_data.G - Using Web API data in a Shiny app. (6 TODOs, 30pts) -solutions
#
#
# For reference on reactive code: https://mastering-shiny.org/basic-reactivity.html

get.data <- function(url) {
  #TODO 1 (5pts): Add the 4 statements that will execute a GET request using the 
  #        url passed in. This function returns the dataframe.
  #        Note: you can omit or comment out the status check.
####################################################################
  resp <- GET(url)
  resp_text <- content(resp, "text", encoding = "UTF-8")
  data_json <- fromJSON(resp_text, flatten = TRUE)
  data.df <- as.data.frame(data_json)
  
  
####################################################################
  return(data.df)
}

#TODO 2 (5pts): Write two statements: 1- Call the get.data function with the url to get all crime categories
#               and assign the return to cat.df, 2- Create a variable called "categories"
#               and assign to it the "url" column in cat.df.
####################################################################
cat.df <- get.data("https://data.police.uk/api/crime-categories")
categories <- cat.df$url
####################################################################

#TODO 3 (5pts): Write two statements: 1- Call the get.data function with the url to get all forces
#               and assign the return to force.df, 2- Create a variable called "forces"
#               and assign to it the "id" column in force.df.
####################################################################
force.df <- get.data("https://data.police.uk/api/forces")
forces <- force.df$id
####################################################################

ui <- fluidPage(
  titlePanel("Lab: Crime Data"),
  h4("Request url"),
  # this component is just to see the request url.
  verbatimTextOutput("url.txt"),
  sidebarLayout(
  sidebarPanel(
     uiOutput("categories"),
     
#TODO 4 (5pts): Add the statements to add the uiOutput for "forces" and "date".
####################################################################
  uiOutput("forces"),
  uiOutput("date"),
####################################################################
   ),
   
   mainPanel(
     plotOutput("outcome.cat.plot"),
     tableOutput("data.table")
   )
 )
)

server <- function(input, output, session) {
  # Renders (and re-renders) the selectInput "sel.cat".
  output$categories <- renderUI({
    selectInput('sel.cat', 'Crime Category', categories, selectize=FALSE)
  })
  
#TODO 5 (5pts): Add the selectInput for forces.
####################################################################
  output$forces <- renderUI({
    selectInput('sel.force', 'Police Forces', forces, selectize=FALSE)
  })
####################################################################

  # Renders (and re-renders) the textInput "date".
  output$date <- renderUI({
    textInput("txt.date", "Date: yyyy-mm", "")
  })
  
  # This call to renderPrint assembles the request url based on user inputs. It prints it to the verbatimTextOutput called "url.txt".
  output$url.txt <- renderPrint(
    if(input$txt.date=="")
      paste("https://data.police.uk/api/crimes-no-location?category=",input$sel.cat,"&force=",input$sel.force, sep="")
    else
      paste("https://data.police.uk/api/crimes-no-location?category=",input$sel.cat,"&force=",input$sel.force,"&date=",input$txt.date, sep="")
  )
  
  # This code block assembles the request url with parameters based on user inputs: crime category,
  # force, and (optionally) a date in the form yyyy-mm. If the resulting dataframe is empty, the string "no results"
  # is returned.
  results <- reactive({
    if(input$txt.date=="")
      request<-paste("https://data.police.uk/api/crimes-no-location?category=",input$sel.cat,"&force=",input$sel.force, sep="")
    else
      request<-paste("https://data.police.uk/api/crimes-no-location?category=",input$sel.cat,"&force=",input$sel.force,"&date=",input$txt.date, sep="")
    data<- get.data(request)
    if(nrow(data)>0)
      return(subset(data, select=c("category", "month", "outcome_status.category",	"outcome_status.date")))
    else
      return("no results")
  })
  
  # This code gets the dataframe returned by executing the request url determined by the user selections: crime category,
  # force, and (optionally) a date in the form yyyy-mm. If the dataframe contains data, a bar chart will be displayed.
  # The bars represent the frequencies of outcome categories in the dataframe. Note that only categories with frequency 
  # counts of 10% or greater are included.
  output$outcome.cat.plot<-renderPlot({
    data<-results()
    if(data!="no results") {
      count.df<-count(data, vars = outcome_status.category)
      count.top.df<-subset(count.df, ((count.df$n/nrow(data))*100) >= 10 )
      ggplot(count.top.df, aes(vars, n)) + geom_bar(stat = "identity")+ xlab("outcome category") + ylab("N") 
    }
    else
      "no results"
  })
  
#TODO 6 (5pts): Add the statement to render the table. You will call on the reactive block "results".
####################################################################
  output$data.table <- renderTable(results())
####################################################################
}

shinyApp(ui, server)