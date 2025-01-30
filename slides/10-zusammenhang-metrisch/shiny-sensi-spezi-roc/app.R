# Visualize connections between raw data and 
# threshold-dependent true & false positive rates, i.e.: ROC curves
#
# Author: Fabian Scheipl (fabian.scheipl@googlemail.com)
# License: CC BY 4.0-

library(shiny)
source("utils.R")

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Sensitivität, Spezifität & ROC-Kurve"),
    
    # Sidebar with (slider) inputs
    sidebarLayout(
        sidebarPanel(
            h3("Testvorschrift:"),
            sliderInput("cutoff",
                        "Schwellenwert für diagnostischen Test",
                        min = -2,
                        max = 5,
                        step = 0.1,
                        value = 1),
            p("Personen, deren Score diesen Wert überschreitet, werden als <krank> diagnostiziert."),
            br(),
            h3("Eigenschaften der Daten:"),
            sliderInput("n_pos",
                        "Anzahl Kranke:",
                        min = 1,
                        max = 300,
                        value = 50),
            sliderInput("n_neg",
                        "Anzahl Gesunde:",
                        min = 1,
                        max = 300,
                        value = 200),
            sliderInput("mean_diff",
                        "Mittelwertsunterschied Score Kranke - Gesunde",
                        min = 0,
                        max = 5,
                        value = 2),
            p("Wie weit liegt der Mittelwert der Scores der Kranken über dem der Gesunden?"),
            sliderInput("sd_pos",
                        "Standardabweichung Score Kranke",
                        min = .1,
                        max = 2,
                        value = 1),
            p("Wie groß ist die Standardabweichung der Scores der Kranken?"),
            sliderInput("sd_neg",
                        "Standardabweichung Score Gesunde",
                        min = .1,
                        max = 2,
                        value = 1),
            p("Wie groß ist die Standardabweichung der Scores der Gesunden?"),
            numericInput("seed", 
                         "Startwert Zufallsgenerator",
                         min = 1L,
                         max = .Machine$integer.max,
                         value = 1337,
                         step = 1L),
            p("(Änderung würfelt neue Daten aus.)"),
            ),
        mainPanel(
            plotOutput("roc_plot")
        )
    )
)

server <- function(input, output, session) {
    output$roc_plot <- renderPlot({
        data <- make_roc_data(n_pos = input$n_pos,
                              n_neg = input$n_neg,
                              mean_diff = input$mean_diff,
                              sd_pos = input$sd_pos,
                              sd_neg = input$sd_neg,
                              cutoff = input$cutoff,
                              seed = input$seed)
        roc_plot(data = data,
                 cutoff = input$cutoff,
                 roclabels = 3.5)
    },
    # adapts figure size to fill more vertical space, 
    #   see https://github.com/rstudio/shiny/issues/650
    height = function() 0.7 * session$clientData$output_roc_plot_width)
}

# Run the application 
shinyApp(ui = ui, server = server)
