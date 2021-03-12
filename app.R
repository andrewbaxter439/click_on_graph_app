library(ggplot2)
library(shiny)

ui <- fluidPage(
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(sidebarPanel(
        sliderInput(
            "bins",
            "Number of bins:",
            min = 1,
            max = 50,
            value = 30
        )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("distPlot", click = "click_graph"),
        
        textOutput("coords")
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    clicks <- reactiveValues(clickno = 1)
    
    observeEvent(input$click_graph$x, {
        clicks[[paste0("c", clicks$clickno)]] <- c(input$click_graph$x, input$click_graph$y)
        clicks$clickno <- 3 - clicks$clickno
    })
    
    lineDraw <- reactiveValues()
    
    observeEvent(clicks$c2[1],{
        grad <- (clicks$c1[2]-clicks$c2[2])/(clicks$c1[1] - clicks$c2[1])
        int <- clicks$c1[2]- clicks$c1[1] * grad
        lineDraw$grad <- grad
        lineDraw$int <- int
    }
    )
    
    output$distPlot <- renderPlot({
        
        df <- data.frame(x = as.numeric(c(clicks$c1[1], clicks$c2[1])), y = as.numeric(c(clicks$c1[2], clicks$c2[2])))
        
        # draw the histogram with the specified number of bins
        ggplot(faithful, aes(x = eruptions, y = waiting)) +
            geom_point() +
            geom_point(data = df,
                       aes(x, y),
                       colour = "blue", 
                       size = 5,
                       shape = 3)+ 
            geom_abline(intercept = lineDraw$int, slope = lineDraw$grad)
    })
    
    output$coords <- renderText({
        paste(clicks$c1, clicks$c2, lineDraw$grad, lineDraw$int)
        # glue::glue("{clicks$c1}, clicks$c2, lineDraw$grad, lineDraw$int")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
