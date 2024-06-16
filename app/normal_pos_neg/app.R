library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualising mean and standard deviation on a histogram"),
    
    hr(),
    h4("Use the sliders at the bottom to change the mean and standard deviation or a randomly generated data set and visualise the data in a histogram. The solid black vertical line indicates the position of the mean, while the red dashed lines show two standard deviations above and below the mean."),
    hr(),
    
    plotOutput("distPlot"),
    
    fluidRow(
        column(4, offset = 1,
               sliderInput("Mean", label = "Mean", width="100%", min = -4, 
                           max = 4, value = 0, step = 0.25),
        ),
        column(1, offset = 0),
        column(4, offset = 1,
            sliderInput("SD", label = "Standard Deviation (SD)", width="100%",
                        min = 1, max = 2, value = 1, step = 0.1)
        )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    mbox <- function(x0, x1, y0, y1){
        xx <- seq(from=x0, to=x1, length.out = 100);
        yy <- seq(from=y0, to=y1, length.out = 100);
        xd <- c(rep(x0, 100), xx, rep(x1,100), rev(xx));
        yd <- c(yy, rep(y1,100), rev(yy), rep(y0, 100));
        return(list(x=xd, y=yd));
    }

    output$distPlot <- renderPlot({
        Mn  <- as.numeric(input$Mean);
        Sd  <- as.numeric(input$SD);
        rand_dat <- rnorm(n = 150, mean = Mn, sd = Sd);
        real_mn  <- mean(rand_dat);
        real_sd  <- sd(rand_dat);
        par(mar = c(5, 5, 1, 1), lwd = 3);
        hist(x = rand_dat, main = "", ylab = "Frequency",
             xlab = "Measured value (e.g., tree height in m)", cex.lab = 1.5, 
             lwd = 4, xaxt = "n", col = "blue", xlim = c(-12, 12), 
             cex.axis = 1.5,  ylim = c(0, 100), breaks = -12:12, add = FALSE);
        axis(side = 1, at = -12:12, cex.axis = 1.5, lwd = 3, line = -0.75);
        arrows(x0 = real_mn, x1 = real_mn, y0 = 90, y1 = 80, lwd = 4);
        tbox <- mbox(x0 = real_mn - 4, x1 = real_mn + 4, y0 = 90, y1 = 100);
        polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="white");
        text(x = real_mn, y = 95, cex = 2, labels = "Mean value");
        lines(x = c(real_mn, real_mn), y = c(0, 75), lwd = 4);
        lines(x = c(real_mn - 2*real_sd, real_mn - 2*real_sd), y = c(0, 75), 
              lwd = 2, lty = "dashed", col = "red");
        lines(x = c(real_mn + 2*real_sd, real_mn + 2*real_sd), y = c(0, 75), 
              lwd = 2, lty = "dashed", col = "red");
        text(x = real_mn + 3*real_sd + 1.1, y = 50, labels = "+2 * SD", cex = 2,
             col = "red");
        text(x = real_mn - 3*real_sd - 1.1, y = 50, labels = "-2 * SD", cex = 2,
             col = "red");
        hist(x = rand_dat, main = "", ylab = "Frequency",
             xlab = "Measured value (e.g., tree height in m)", cex.lab = 1.5, 
             lwd = 4, xaxt = "n", col = "blue", xlim = c(-12, 12), 
             cex.axis = 1.5,  ylim = c(0, 100), breaks = -12:12, add = TRUE);
    })
}

# Run the application 
shinyApp(ui = ui, server = server)




