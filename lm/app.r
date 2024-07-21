#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data Visualization in Shiny"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
           
            tags$hr(),
            actionButton("go", "Plot Linear Model"),
           
            tags$hr(),
            actionButton("Slope", "Display Slope"),
           
            tags$hr(),
            actionButton("Intercept", "Display y-intercept"),
           
            tags$hr(),
            actionButton("r_squared", "Display R-squared")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("origPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    lmdata <- reactiveValues()
        
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })

    observeEvent(input$go, {
        update_lm()
    })

    observeEvent(input$Slope, {
        update_slope()
    })

    observeEvent(input$Intercept, {
        update_intercept()
    })

    observeEvent(input$r_squared, {
        update_r_squared()
    })
    
    update_lm <- function() {
        lmdata$model <- lm(y ~ x, data = dataInput())
    }

    update_slope <- function() {
        update_lm()
        lmdata$slope <- coef(lmdata$model)[2]
    }

    update_intercept <- function() {
        update_lm()
        lmdata$intercept <- coef(lmdata$model)[1]
    }

    update_r_squared <- function() {
        update_lm()
        lmdata$r_squared <- summary(lmdata$model)$r.squared
    }
    
    output$origPlot <- renderPlot( {
      plot <- ggplot(dataInput(), aes(x = x, y = y)) +
        geom_point() +
        labs(x = "X", y = "Y") +  # Change axis labels
        ggtitle("Scatterplot") +  # Add plot title
        scale_x_continuous(limits = c(0, NA)) +  # Set minimum x-axis value to 0
        theme_bw()  # Apply minimal theme
      
      plot
    })

    output$lmPlot <- renderPlot({
      plot <- ggplot(dataInput(), aes(x = x, y = y)) +
        geom_point() +
        labs(x = "X", y = "Y") +  # Change axis labels
        ggtitle("Scatterplot") +  # Add plot title
        scale_x_continuous(limits = c(0, NA)) +  # Set minimum x-axis value to 0
        theme_bw()  # Apply minimal theme
      
      if (!is.null(lmdata$model)) {
          plot <- plot +
            geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Change axis labels
            ggtitle("Scatterplot with Linear Regression Line")  # Add plot title
      }

      # Add slope annotation if slope is calculated
      if (!is.null(lmdata$slope)) {
        plot <- plot + annotate("text", x = 0, y = Inf, 
                                label = paste("Slope:", round(lmdata$slope, 2)), 
                                hjust = 0, vjust = 2, size = 5, color = "red")
      }

      # Add y-intercept annotation if intercept is calculated
      if (!is.null(lmdata$intercept)) {
        plot <- plot + annotate("text", x = 0, y = Inf, 
                                label = paste("Y-Intercept:", round(lmdata$intercept, 2)), 
                                hjust = 0, vjust = 4, size = 5, color = "red")
        
      }

      # Add r-squared annotation if intercept is calculated
      if (!is.null(lmdata$r_squared)) {
        plot <- plot + annotate("text", x = 0, y = Inf, 
                                label = paste("r_squared:", round(lmdata$r_squared, 2)), 
                                hjust = 0, vjust = 6, size = 5, color = "red")
        
      }
      
      plot
   })

    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
