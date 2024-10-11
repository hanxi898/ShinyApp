install.packages(c("shiny", "httr", "jsonlite","lab5"))
library(shiny)
library(httr)
library(jsonlite)
library(lab5)


get_kpi_data <- function(year) {
  url <- paste0("https://api.kolada.se/v2/data/kpi/N00945/year/", year)
  print(paste("Fetching data from URL:", url)) 
  response <- GET(url)  
  
 
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8") 
    json_data <- fromJSON(data)  
    
 
    print("JSON Data Structure:")
    print(str(json_data))  
    
    
    if (!is.null(json_data$values) && nrow(json_data$values) > 0) {
      
      df <- do.call(rbind, lapply(1:nrow(json_data$values), function(i) {
        values_row <- json_data$values[i, ]
        if (length(values_row$values) > 0) {
          value_df <- values_row$values[[1]]  
          return(data.frame(
            municipality = values_row$municipality,
            value = value_df$value,
            period = values_row$period
          ))
        }
        return(NULL)
      }))
   
      print("Data Frame Structure:")
      print(str(df))  
      return(df)
    } else {
      stop("No values found in the JSON data.")
    }
  } else {
    stop("Failed to fetch data. Status code: ", status_code(response))  
  }
}

ui <- fluidPage(
  titlePanel("Kolada KPI Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("period", "KPI:", choices = as.character(2000:2023)), 
      actionButton("load", "Load Data") 
    ),
    mainPanel(
      tableOutput("kpiTable") 
    )
  )
)

server <- function(input, output, session) {

  data_list <- reactiveVal(data.frame())  

  observeEvent(input$load, {
    data <- get_kpi_data(input$period)
    
    if (!is.null(data)) {
      data_list(data)  
      print(data_list())  
      
      output$kpiTable <- renderTable({
        req(data_list())  
        data_list()  
      })
    } else {
      showNotification("Data loading failed or no data available", type = "error")
    }
  })
}


shinyApp(ui = ui, server = server)
