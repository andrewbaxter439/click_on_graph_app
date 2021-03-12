library(ggplot2)
library(shiny)
library(glue)

ui <- fluidPage(titlePanel("Old Faithful Geyser Data"),
                
                
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
                        plotOutput("distPlot",
                                   click = "click_graph",
                                   dblclick  = "clear_graph"),
                        p(
                            "Click on two points on the graph to estimate a best fitted straight line.
                            Double-click to clear."
                        ),
                        checkboxInput("lm", "Show linear model fit"),
                        fluidRow(
                            column(6,
                                   verbatimTextOutput("coords")
                                   ),
                            column(6,
                                   conditionalPanel("input.lm",
                                   verbatimTextOutput("model")
                                                    )
                                   )
                        )
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
    
    df_points <- reactive({
        data.frame(x = as.numeric(c(clicks$c1$x, clicks$c2$x)),
                   y = as.numeric(c(clicks$c1$y, clicks$c2$y)))
    })
    
    output$distPlot <- renderPlot({
        # draw the histogram with the specified number of bins
        plt <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
            geom_point() +
            geom_point(
                data = df_points(),
                aes(x, y),
                colour = "red",
                size = 5,
                stroke = 1,
                shape = 3
            ) +
            geom_abline(
                intercept = inputLine$int,
                slope = inputLine$grad,
                aes(alpha = "Your line"),
                colour = "red"
            ) +
            geom_line(
                data = data.frame(
                    x = mean(faithful$eruptions),
                    y = mean(faithful$waiting)
                ),
                aes(x, y, alpha = "Your line"),
                inherit.aes = FALSE
            ) +
            scale_alpha_manual(
                name = NULL,
                values = c(
                    "Your line" = 1,
                    "Calculated best fit" = 1
                ),
                guide = guide_legend(
                    override.aes = list(
                        linetype = 1,
                        colour = "red",
                        size = 1,
                        fill = NULL,
                        intercept = NULL,
                        slope = NULL
                    )
                )
            ) +
            theme(
                legend.position = "bottom",
                legend.text = element_text(size = 12),
                legend.key = element_blank()
            )
        
        if (input$lm) {
            plt <- plt + geom_smooth(
                method = "lm",
                se = FALSE,
                aes(alpha = "Calculated best fit"),
                colour = "blue"
            ) +
                scale_alpha_manual(
                    name = NULL,
                    values = c(
                        "Your line" = 1,
                        "Calculated best fit" = 1
                    ),
                    guide = guide_legend(
                        override.aes = list(
                            linetype = 1,
                            colour = c("blue", "red"),
                            size = 1,
                            fill = NULL,
                            intercept = NULL,
                            slope = NULL
                        )
                    )
                )
        }
        
        plt
        
    })
    
    inputLine <- reactiveValues()
    
    observe({
        inputLine$grad <- input$slope
        inputLine$int <- input$intercept
    })
    
    observeEvent(input$clear_graph, {
        clicks$c1 <- NULL
        clicks$c2 <- NULL
        inputLine$grad <- NULL
        inputLine$int <- NULL
        clicks$clickno <- 1
    })
    
    output$coords <- renderText({
        glue(
            "Your clicks and estimated line:
             Slope = {lineDraw$grad}
             Intercept = {lineDraw$int}
             x1 = {clicks$c1$x}, y1 = {clicks$c1$y}
             x2 = {clicks$c2$x}, y2 = {clicks$c2$y}",
            .transformer = function(text, envir) {
                round(as.numeric(identity_transformer(text, envir)), 2)
            }
        )
    })
    
    output$model <- renderText({
        mod <- lm(waiting ~ eruptions, data = faithful)
        
        glue(
            "Calculated model:
             Slope = {round(mod$coefficients[2], 2)}
             Intercept = {round(mod$coefficients[1])}"
        )
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
