library(shiny)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            actionButton("draw", "Draw"),
            br(),
            br(),
            actionButton("new_game", "New Game"),
            
        ),
        mainPanel(
            uiOutput("display")
        )
    )
)

server <- function(input, output) {
    v <- reactiveValues(
        current = NULL, 
        previous = NULL,
        counter = 0,
        master = list(
            b = paste0("B-", sample(1:15)),
            i = paste0("I-", sample(16:30)),
            n = paste0("N-", sample(31:45)),
            g = paste0("G-", sample(46:60)),
            o = paste0("O-", sample(61:75))
        ) %>% 
            unlist() %>% 
            `[`(sample(1:75))
    )
    
    observeEvent(input$new_game, {
        v$current <- NULL
        v$previous <- NULL
        v$counter <- 0
    })
    
    observeEvent(input$draw, {
        
        if (v$counter > 74) {
            NULL
        } else {
            v$counter <- v$counter + 1
            v$current <- v$master[v$counter]
            v$previous <- c(v$current, v$previous)
        }
    })
    
    output$display <- renderUI({
        tagList(
            p(v$current, style = "font-size:50px"),
            br(),
            p(paste(v$previous, collapse = ", "))
        )
    })
}

shinyApp(ui = ui, server = server)
