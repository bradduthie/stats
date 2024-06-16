#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(""),
    
    column(4,
           fluidRow(
               column(4, offset = 1,
                      actionButton("go", "Randomise"), 
                      
               ),
               column(1, offset = 0),
               column(4, offset = 1,
                      actionButton("reset", "Reset")
               ),
           ),
           DT::dataTableOutput("tt")
    ),
    
    column(8, 
           plotOutput("rplot")
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    sp   <- c(rep("SO1", 17), rep("SO2", 15))
    eg   <- c(3.256, 3.133, 3.071, 2.299, 2.995, 2.929, 3.291, 2.658, 3.406, 
              2.976, 2.817, 3.133, 3.000, 3.027, 3.178, 3.133, 3.210, 3.014, 
              2.790, 2.985, 2.911, 2.914, 2.724, 2.967, 2.745, 2.973, 2.560, 
              2.837, 2.883, 2.668, 3.063, 2.639);
    wp   <- as.data.frame(cbind(sp, eg));
    difs <- tapply(X = eg, INDEX = sp, FUN = mean);
    df1  <- difs[1] - difs[2];
    
    M  <- reactiveValues(data = df1)
    N  <- reactiveValues(data = 1:length(sp))
    W  <- reactiveValues(data = as.data.frame(cbind(sp, eg)))
    P  <- reactiveValues(data = df1);
    
    observeEvent(input$go, {
        theord  <- sample(1:length(sp), length(sp));
        Species <- sp[theord];
        Ovi_len <- eg;
        N$data  <- theord;
        W$data  <- as.data.frame(cbind(Species, Ovi_len));
        difs    <- tapply(X = eg, INDEX = sp[theord], FUN = mean);
        dval    <- difs[1] - difs[2];
        M$data  <- c(M$data, dval);
        P$data  <- dval;
    })
    
    observeEvent(input$reset, {
        N$data  <- 1:24;
        M$data  <- df1;
        W$data  <- as.data.frame(cbind(sp, eg))
    })
    
    output$rplot <- renderPlot({
        breaks <- seq(from = -0.28, to = 0.28, by = 0.01)
        par(mar = c(5, 5, 1, 1));
        hist(M$data, main = "", xlab = "Random mean difference (mm)", cex.lab = 1.5, 
             cex.axis = 1.5, xlim = c(-0.28, 0.28), ylim = c(0, 20), 
             breaks = breaks, col = "grey");
        arrows(x0 = df1, x1 = df1 , y0 = 6, y1 = 1.5, 
               length = 0.15, lwd = 4, col = "red")
        text(x = df1 , y = 6.5, labels = "Observed", cex = 1.5, col = "red")
        # bquote("(" * bar(x) * " = " * .(x) * ", " *  bar(y) * " = " * .(y) * ")")
        answer <- paste("Mean(SO1) - Mean(SO2) = ", round(P$data, digits = 3), "mm")
        text(x = 0, y = 19.5, labels = answer, cex = 2)
        hist(df1, main = "", xlab = "Random mean difference", cex.lab = 1.5, 
             cex.axis = 1.5, xlim = c(-8, 8), ylim = c(0, 10), breaks = breaks,
             add = TRUE, col = "red");
    }, height = 700)
    
    output$tt <- DT::renderDataTable({
        options(DT.options = list(pageLength = 32));
        wp   <- data.frame(Species = W$data[,1], Ovipositor = W$data[,2])
        ddd  <- datatable(wp)
        formatStyle(ddd, "Species", 
                    backgroundColor = styleEqual( c("SO1", "SO2"), 
                                                  c("#E69F00", "#56B4E9")
                    ),
                    fontWeight = 'bold',
                    pageLength = 50, 
        )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)




