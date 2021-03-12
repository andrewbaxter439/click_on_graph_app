library(ggplot2)
library(shiny)
library(glue)

ui <- fluidPage(titlePanel("Old Faithful Geyser Data"),
                
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                    sidebarPanel(fluidRow(
                        column(6,
                               numericInput("slope",
                                            "Line slope",
                                            value = NULL)),
                        column(6,
                               numericInput("intercept",
                                            "Line intercept",
                                            value = NULL))
                        
                    )),
                    
                    
                    mainPanel(
                        plotOutput("distPlot", click = "click_graph"),
                        
                        verbatimTextOutput("coords")
                    )
                ))


server <- function(input, output, session) {
    clicks <- reactiveValues(clickno = 1)
    
    observe({
        updateNumericInput(session,
                           "slope",
                           value = round(as.numeric(lineDraw$grad), 2))
        
        updateNumericInput(session,
                           "intercept",
                           value = round(as.numeric(lineDraw$int), 2))

    })
    
    observeEvent(input$click_graph$x, {
        clicks[[paste0("c", clicks$clickno)]] <-
            list(x = input$click_graph$x,
                 y = input$click_graph$y)
        clicks$clickno <- 3 - clicks$clickno
    })
    
    lineDraw <- reactiveValues()
    
    observeEvent(clicks$c2$x, {
        grad <- (clicks$c1$y - clicks$c2$y) / (clicks$c1$x - clicks$c2$x)
        int <- clicks$c1$y - clicks$c1$x * grad
        lineDraw$grad <- grad
        lineDraw$int <- int
    })
    
    output$distPlot <- renderPlot({
        df <-
            data.frame(x = as.numeric(c(clicks$c1$x, clicks$c2$x)), y = as.numeric(c(clicks$c1$y, clicks$c2$y)))
        
        # draw the histogram with the specified number of bins
        ggplot(faithful, aes(x = eruptions, y = waiting)) +
            geom_point() +
            geom_point(
                data = df,
                aes(x, y),
                colour = "blue",
                size = 5,
                shape = 3
            ) +
            geom_abline(intercept = lineDraw$int, slope = lineDraw$grad)
    })
    
    output$coords <- renderText({
        # paste(clicks$c1, clicks$c2, lineDraw$grad, lineDraw$int)
        glue(
            "x1 = {clicks$c1$x}, y1 = {clicks$c1$y}
             x2 = {clicks$c2$x}, y2 = {clicks$c2$y}
             Slope = {lineDraw$grad}, Intercept = {lineDraw$int}",
            .transformer = function(text, envir) {
                round(as.numeric(identity_transformer(text, envir)), 2)
            }
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
