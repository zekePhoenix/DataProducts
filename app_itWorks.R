# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(dplyr)
library(readr)


# Load data



# Load data
trend_data <- read_csv("data.csv")
names(trend_data) <- c("date","type","count")



# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Atlanta Police Crime Trends"),
                sidebarLayout(
                    sidebarPanel(
                        
                        # Select type of trend to plot
                        selectInput(inputId = "type", label = strong("Crime Trend Type"),
                                    choices = unique(trend_data$type),
                                    selected = "Larceny-From Vehicle"),
                        
                        # Select date range to be plotted
                        dateRangeInput("date", strong("Date range"), start = "2016-01-01", end = "2020-03-12",
                                       min = "2009-01-01", max = "2020-03-12"),
                        
                        # Select whether to overlay smooth trend line
                        checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                        
                        # Display only if the smoother is checked
                        conditionalPanel(condition = "input.smoother == true",
                                         sliderInput(inputId = "f", label = "Smoother span:",
                                                     min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                     animate = animationOptions(interval = 100)),
                                         HTML("Higher values give more smoothness.")
                        )
                    ),
                    
                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "lineplot", height = "300px"),
                        tags$a(href = "https://www.atlantapd.org/i-want-to/crime-data-downloads", "Source: Atlanta Police Department", target = "_blank")
                    )
                )
)

# Define server function
server <- function(input, output) {
    
    # Subset data
    selected_trends <- reactive({
        req(input$date)
        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
        trend_data %>%
            filter(
                type == input$type,
                date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
                ))
    })
    
    
    # Create scatterplot object the plotOutput function is expecting
    output$lineplot <- renderPlot({
        color = "#000088"
        color1 = "#BBAA55"
        color2 = "#0011EE"
        par(mar = c(4, 4, 1, 1))
        plot(x = selected_trends()$date, y = selected_trends()$count, type = "l",
             xlab = "Date", ylab = "Trend index", col = color1, fg = color, col.lab = color, col.axis = color2)
        # Display only if smoother is checked
        if(input$smoother){
            smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$count, f = input$f)
            lines(smooth_curve, col = "#882211", lwd = 3)
        }
    })
    
}

# Create Shiny object
shinyApp(ui = ui, server = server)