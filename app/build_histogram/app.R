library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Building a histogram"),
    
    hr(),
    h4("Use the arrows at the bottom to increase the sample size of sparrows. As you increaes the sample size, you will see a new sparrow added in the right panel with its measured length at the top right. Notice how this adds a measurement to the histogram on the left, and think about the distribution of measured lengths that is being built."),
    hr(),
    
    plotOutput("distPlot"),
    
    fluidRow(
        column(4, offset = 1,
               numericInput("N1", label = "Sample size of birds", width="100%",
                            min = 0, max = 500, value = 0, step = 1), 
        ),
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # set.seed(100);
    
    mbox <- function(x0, x1, y0, y1){
        xx <- seq(from=x0, to=x1, length.out = 100);
        yy <- seq(from=y0, to=y1, length.out = 100);
        xd <- c(rep(x0, 100), xx, rep(x1,100), rev(xx));
        yd <- c(yy, rep(y1,100), rev(yy), rep(y0, 100));
        return(list(x=xd, y=yd));
    }
    
    totlen <- c(154, 160, 155, 154, 156, 161, 157, 159, 158, 158, 160, 162, 161,
                160, 159, 158, 159, 166, 159, 160, 161, 163, 156, 165, 160, 158,
                160, 157, 159, 160, 158, 161, 160, 160, 153, 165, 160, 161, 162,
                163, 162, 163, 161, 160, 162, 160, 161, 162, 165, 161, 161, 162,
                164, 158, 162, 156, 166, 165, 166, 156, 156, 163, 163, 160, 156,
                162, 163, 164, 163, 160, 160, 158, 158, 158, 155, 160, 156, 158,
                166, 165, 157, 164, 166, 167, 161, 166, 161, 156, 154, 153, 153,
                155, 163, 157, 155, 164, 158, 158, 160, 161, 157, 157, 156, 158,
                153, 155, 163, 159, 155, 156, 160, 152, 160, 155, 157, 165, 153,
                162, 162, 159, 159, 155, 162, 152, 159, 155, 163, 163, 156, 159,
                161, 155, 162, 153, 162, 164);
    totwei <- c(24.5, 26.9, 26.9, 24.3, 24.1, 26.5, 24.6, 24.2, 23.6, 26.2, 
                26.2, 24.8, 25.4, 23.7, 25.7, 25.7, 26.5, 26.7, 23.9, 24.7, 28,
                27.9, 25.9, 25.7, 26.6, 23.2, 25.7, 26.3, 24.3, 26.7, 24.9, 
                23.8, 25.6, 27, 24.7, 26.5, 26.1, 25.6, 25.9, 25.5, 27.6, 25.8,
                24.9, 26, 26.5, 26, 27.1, 25.1, 26, 25.6, 25, 24.6, 25, 26, 
                28.3, 24.6, 27.5, 31, 28.3, 24.6, 25.5, 24.8, 26.3, 24.4, 23.3,
                26.7, 26.4, 26.9, 24.3, 27, 26.8, 24.9, 26.1, 26.6, 23.3, 24.2,
                26.8, 23.5, 26.9, 28.6, 24.7, 27.3, 25.7, 29, 25, 27.5, 26, 
                25.3, 22.6, 25.1, 23.2, 24.4, 25.1, 24.6, 24, 24.2, 24.9, 24.1,
                24, 26, 24.9, 25.5, 23.4, 25.9, 24.2, 24.2, 27.4, 24, 26.3, 
                25.8, 26, 23.2, 26.5, 24.2, 26.9, 27.7, 23.9, 26.1, 24.6, 23.6, 
                26, 25, 24.8, 22.8, 24.8, 24.6, 30.5, 24.8, 23.9, 24.7, 26.9, 
                22.6, 26.1, 24.8, 26.2, 26.1);
    totbir <- length(totlen);
    
    xloc  <- runif(totbir);
    yloc  <- runif(totbir);
    
    output$distPlot <- renderPlot({
        N1     <- input$N1;
        minlen <- min(totlen);
        maxlen <- max(totlen);
        par(mar = c(5, 5, 1, 1), lwd = 3, mfrow = c(1, 2));
        hist(x = totlen, main = "", ylab = "Number of sparrows",
             xlab = "Sparrow total length (mm)", cex.lab = 1.5, lwd = 4,
             xaxt = "n", col = "white", xlim = c(minlen, maxlen), 
             ylim = c(0, 19), breaks = minlen:maxlen,
             border = "white", add = FALSE, cex.axis = 1.5);
        if(N1 > 0){
            hist(x = totlen[1:N1], main = "", ylab = "Number of sparrows",
                 xlab = "Sparrow total length (mm)", cex.lab = 1.5, lwd = 4,
                 xaxt = "n", col = "blue", xlim = c(minlen, maxlen), 
                 cex.axis = 1.5, ylim = c(0, 20), breaks = minlen:maxlen, 
                 add = TRUE);
            axis(side = 1, at = minlen:maxlen, cex.axis = 1.5, lwd = 3, 
                 line = -0.75);
            size  <- 0.02 * (totlen + 10*(totlen - mean(totlen)));
            par(mar = c(0, 0, 0, 0), lwd = 1, xaxt = "n", yaxt = "n");
            plot(x = xloc[1:N1], yloc[1:N1], pch = "-", xlim = c(0, 1), 
                 ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", 
                 xlab = "", cex = 4*size[1:N1], bg = "grey", col = "black");
            points(x = xloc[1:N1], yloc[1:N1]-0.02, pch = "^", xlim = c(0, 1), 
                   ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", 
                   xlab = "", cex = 1.5*size[1:N1], col = "orange"); 
            points(x = xloc[1:N1], yloc[1:N1], pch = 25, xlim = c(0, 1), 
                 ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", 
                 xlab = "", cex = size[1:N1], bg = "blue");
            lenmeas <- paste("Measured Length = ", totlen[N1], " (mm)");
            text(x = 0.6, y = 0.99, labels = lenmeas, col = "blue", cex = 1.8,
                 bg = "grey");
        }else{
            axis(side = 1, at = minlen:maxlen, cex.axis = 1.5, lwd = 3, 
                 line = -0.75);
            par(mar = c(0, 0, 0, 0), lwd = 1, xaxt = "n", yaxt = "n");
            plot(x = xloc[1:N1], yloc[1:N1], pch = 25, xlim = c(0, 1), 
                 ylim = c(0, 1), xaxt = "n", yaxt = "n", ylab = "", 
                 xlab = "", type = "n", bg = "blue");
        }
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
