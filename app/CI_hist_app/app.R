library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Visualising confidence intervals on a histogram"),
    
    hr(),
    h4("Use the sliders at the bottom to change the mean, standard deviation, and sample size of a randomly generate data set. The solid black vertical line indicates the position of the mean, while the red dashed lines show confidence intervals above and below the mean. Think about what happens to the confidence intervals as you increase and decrease the mean, standard deviation, and sample size. Try to link the changes that you see in the confidence intervals to the mathematical expressions in red that define them."),
    hr(),
    
    plotOutput("distPlot"),
    
    inputPanel(
        sliderInput("N_val2", label = "Sample size", width = "100%", 
                    min = 10, max = 100, value = 20, step = 1),
        sliderInput("mn_val2", label = "Mean", width = "100%", 
                    min = 10, max = 20, value = 15, step = 0.2),
        sliderInput("sd_val2", label = "Standard Deviation", width = "125%",
                    min = 0, max = 3.4, value = 1, step = 0.2),
        selectInput("CI", "Confidence Interval (CI)",
                    c("80%" = "1.28", 
                      "90%" = "1.64",
                      "95%" = "1.96",
                      "99%" = "2.58"),
                    selected = "1.96"
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
        rand_dat <- rnorm(n = input$N_val2, mean = input$mn_val2, sd = input$sd_val2);
        real_mn  <- mean(rand_dat);
        mn_diff  <- real_mn - input$mn_val2;
        rand_dat <- rand_dat - mn_diff;
        real_sd  <- sd(rand_dat);
        lci      <- as.numeric(input$mn_val2) - 
            (as.numeric(input$CI) * (as.numeric(input$sd_val2))) / sqrt(as.numeric(input$N_val2));
        uci      <- as.numeric(input$mn_val2) + 
            (as.numeric(input$CI) * (as.numeric(input$sd_val2))) / sqrt(as.numeric(input$N_val2));
        par(mar = c(5, 5, 1, 1), lwd = 3);
        hist(x = rand_dat, main = "", ylab = "Frequency",
             xlab = "Random value", cex.lab = 1.5, lwd = 4,
             xaxt = "n", col = "blue", xlim = c(8, 22), cex.axis = 1.5, 
             ylim = c(0, 100), breaks = 0:30, add = FALSE);
        axis(side = 1, at = 0:30, cex.axis = 1.5, lwd = 3, line = -0.75);
        arrows(x0 = input$mn_val2, x1 = input$mn_val2, y0 = 90, y1 = 80, lwd = 4);
        tbox <- mbox(x0 = input$mn_val2 - 2, x1 = input$mn_val2 + 2, y0 = 90, y1 = 100);
        polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="white");
        text(x = input$mn_val2, y = 95, cex = 2, labels = "Mean value");
        lines(x = c(input$mn_val2, input$mn_val2), y = c(0, 75), lwd = 4);
        lines(x = c(lci, lci), y = c(0, 75), lwd = 1, lty = "dashed", col = "red");
        lines(x = c(uci, uci), y = c(0, 75), lwd = 1, lty = "dashed", col = "red");
        hist(x = rand_dat, main = "", ylab = "Frequency",
             xlab = "Random value", cex.lab = 1.5, lwd = 4,
             xaxt = "n", col = "blue", xlim = c(8, 22), cex.axis = 1.5, 
             ylim = c(0, 100), breaks = 0:30, add = TRUE);
        text(x = input$mn_val2 + 2.5, y = 50, cex = 1.75, col = "red",
             labels = bquote(paste(.(input$mn_val2) + .(input$CI) * frac(.(input$sd_val2), sqrt(.(input$N_val2)))))
        );
        text(x = input$mn_val2 - 2.5, y = 50, cex = 1.75, col = "red",
             labels = bquote(paste(.(input$mn_val2) - .(input$CI) * frac(.(input$sd_val2), sqrt(.(input$N_val2)))))
        );
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


