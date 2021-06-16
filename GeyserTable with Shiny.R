#
# Activity_geyserTable.G - Using Web API data in a Shiny app. (4 TODOs, 20pts)
#
# This web app retrieves a list of geysers from the Yellowstone Park web API.
# It generates a table that lists the following fields for all geysers:
# The geyser's ID, its name, its latitude', its longitude.
#
# The tableOutput object will be used to create the table, then renderTable will
# be called on the dataframe to display the table.
#
#
# The API documentation:
#  https://geysertimes.org/api/v5/docs/index.php
# An interactive GUI for the API:
#  https://geysertimes.org/index.php
#

# The libraries below will be used in this lab. You may need to install them.
# You may answer "no" to this question: "Do you want to install from sources the package which needs compilation?"
# If you need to install, Rstudio should prompt you to do so. If not, execute the install.packages function in the R console- NOT in the R code chunks in the Rmd file. You can call intall.packages with a vecotr of packages, such as:
#  install.packages(c("formattable", "tidyverse")).

# After installing, remember to execute the call to library so that the package will be loaded into the R session.

library(shiny)
library(httr)
library(jsonlite)
library(DT)

# This function takes a web API query url, carries out
# the steps of parsing and transforming the data into a dataframe.
get.data <- function(url) {
  #TODO 1 (5pts): Add the 4 statements that will execute a GET request using the 
  #        url passed in. This function returns the dataframe.
  #        Note: you can omit or comment out the status check.
  resp <- GET(url)
  resp_text <- content(resp, "text", encoding = "UTF-8")
  geyser_json <- fromJSON(resp_text, flatten = TRUE)
  geysers.df <- as.data.frame(geyser_json)
  
  return(geysers.df)
}

# Define UI for application that renders a table of geyser data.
ui <- fluidPage(
  
  # Application title
  titlePanel("Yellowstone Geyser List"),
  
  column(12, dataTableOutput('geyser.table'))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #TODO 2 (5pts): Create output table dataframe. Do this by calling the get.data function and pass in the url that 
  #     returns the list of all geysers (see the "Geysers" part under "GET Requests" on the api doc site). 
  #     Assign this call to the variable geyser.table.data. This calls the function that you wrote in TODO 1.
  geyser.table.data <- get.data("https://www.geysertimes.org/api/v5/geysers")
  
  #TODO 3 (5pts): Generate a subset of the dataframe from the step above that contains only these columns:
  #        'geysers.id','geysers.name','geysers.latitude','geysers.longitude'. 
  #        Assign this to a variable called geyser.table.data.sub
  geyser.table.data.sub <- subset(geyser.table.data, select= c("geysers.id","geysers.name","geysers.latitude", "geysers.longitude"))
  
  
  #TODO 4 (5pts): Assign to the variable output$geyser.table a call to the renderDataTable function and pass in 
  #        the dataframe you created from TODO 3 above.  
  #        Also, sort the dataframe in ascending order on the geysers.name column. How to do that?
  #        https://www.r-bloggers.com/r-sorting-a-data-frame-by-the-contents-of-a-column/
  output$geyser.table <- renderDataTable(geyser.table.data.sub)
  output$geyser.table[order(output$geyser.table[geysers.name]),]
}

shinyApp(ui = ui, server = server)

