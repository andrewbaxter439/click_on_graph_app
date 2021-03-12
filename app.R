library(ggplot2)
library(shiny)
library(glue)

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
        
        verbatimTextOutput("coords")
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    clicks <- reactiveValues(clickno = 1)
    
    observeEvent(input$click_graph$x, {
        clicks[[paste0("c", clicks$clickno)]] <- list(x = input$click_graph$x, y = input$click_graph$y)
        clicks$clickno <- 3 - clicks$clickno
    })
    
    lineDraw <- reactiveValues()
    
    observeEvent(clicks$c2$x,{
        grad <- (clicks$c1$y-clicks$c2$y)/(clicks$c1$x - clicks$c2$x)
        int <- clicks$c1$y- clicks$c1$x * grad
        lineDraw$grad <- grad
        lineDraw$int <- int
    }
    )
    
    output$distPlot <- renderPlot({
        
        df <- data.frame(x = as.numeric(c(clicks$c1$x, clicks$c2$x)), y = as.numeric(c(clicks$c1$y, clicks$c2$y)))
        
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
        # paste(clicks$c1, clicks$c2, lineDraw$grad, lineDraw$int)
        glue("x1 = {clicks$c1$x}, y1 = {clicks$c1$y}
             x2 = {clicks$c2$x}, y2 = {clicks$c2$y}
             Slope = {lineDraw$grad}, Intercept = {lineDraw$int}", 
                   .transformer = function(text, envir) {
                       # return(round(as.numeric(text), 2))
                       round(as.numeric(identity_transformer(text, envir)), 2)
                   })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
