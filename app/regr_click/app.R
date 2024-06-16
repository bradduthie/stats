library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Thinking about linear regression"),
    
    hr(),
    h4("Add some measurements to regress Temperature against Elevation by hitting the 'Add new measurements' button below. Change the intercept and the slope of the regression line using the sliders, and visualise the residuals of your data by clicking the 'Show residuals' button. To see the equation of your regression line, click 'Show equation'. Try to move the slope and intercept to find the line of best fit by minimising the sum of squared residuals (the plot will turn blue when you succeed)."),
    hr(),
    
    plotOutput("distPlot", height = "600px"),
    
    fluidRow(
        sliderInput("b0", label = "Regression Intercept", width = "100%", 
                    min = -50, max = 50, value = 10, step = 1),
        sliderInput("b1", label = "Regression slope", width = "100%", 
                    min = -10, max = 10, value = 2, step = 0.1),
        actionButton("rands", "Add new measurements"),
        actionButton("rmnds", "Remove measurements"),
        actionButton("sreds", "Show residuals"),
        actionButton("hreds", "Hide residuals"),
        actionButton("sheqn", "Show equation"),
        actionButton("rmeqn", "Hide equation")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values           <- reactiveValues()
    values$dat       <- NA;
    values$B0        <- NA;
    values$B1        <- NA;
    values$residuals <- 0;
    values$eqn       <- 0;
    values$eqn_val   <- NA;
    
    observeEvent(input$rmnds, {
        values$dat <- NA;
        values$B0  <- NA;
        values$B1  <- NA;
    })
    
    observeEvent(input$sheqn, {
      values$eqn <- 1;
    })
    
    observeEvent(input$rmeqn, {
      values$eqn <- 0;
    })
    
    observeEvent(input$sreds, {
      values$residuals <- 1;
    })
    
    observeEvent(input$hreds, {
      values$residuals <- 0;
    })
  
    observeEvent(input$rands, {
        xvals      <- round(runif(n = 10, min = 1, max = 5), digits = 2);
        yvals      <- round(31 + (-6 * xvals) + 
                                rnorm(n = 10, mean = 0, sd = 6),
                            digits = 2);
        values$dat <- data.frame(x = xvals, y = yvals);
        values$B0  <- round(as.numeric(lm(yvals~xvals)$coefficients[1]),
                            digits = 0);
        values$B1  <- round(as.numeric(lm(yvals~xvals)$coefficients[2]),
                            digits = 1);
        values$eqn_val <- paste("y = ", values$B0, " + ", values$B1, "x");
    })
    

    
    output$distPlot <- renderPlot({
        if(!is.na(values$dat[[1]][1])){
            values$ypred <- input$b0 + values$dat$x * input$b1;
            if(values$B0 == input$b0 & values$B1 == input$b1){
              plot(x = values$dat$x, y = values$dat$y, xlim = c(0, 6), 
                   ylim = c(-5, 35), ylab = "Temperature (\u00B0C)",
                   xlab = "Elevation (km)", cex.lab = 1.5, cex.axis = 1.5,
                   pch = 20, cex = 2, col = "blue");
              abline(a = input$b0, b = input$b1, col = "blue", lwd = 5);
              if(values$eqn > 0 & input$b1 > 0){
                eqvl <- paste("y = ", input$b0, " + ", input$b1, "x", 
                              sep = "");
                text(x = 0.5, y = -3, labels = eqvl, 
                     cex = 2, col = "blue", hjust = 0);
              }
              if(values$eqn > 0 & input$b1 <= 0){
                eqvl <- paste("y = ", input$b0, " - ", -1 * input$b1, "x",
                              sep = "");
                text(x = 0.5, y = -3, labels = eqvl, 
                     cex = 2, col = "blue", hjust = 0);
              }
              if(values$residuals > 0){
                SSR <- 0;
                for(i in 1:length(values$dat$x)){
                  x0 <- values$dat$x[i];
                  x1 <- values$dat$x[i];
                  y0 <- values$ypred[i];
                  y1 <- values$dat$y[i];
                  points(x = c(x0, x1), y = c(y0, y1), type = "b", 
                         lwd = 1, lty = "dotted", col = "blue", pch = 20);
                  points(x = x0, y = y1, type = "b", lwd = 1, 
                         lty = "dotted", col = "blue", pch = 20, 
                         cex = 2);
                  resi <- round(y1 - y0, digits = 2);
                  SSR  <- SSR + (resi * resi);
                  rpt  <- mean(c(y0, y1));
                  text(x = x0 + 0.15, y = rpt, labels = resi, col = "blue",
                       hjust = 0);
                } 
                text(x = 4, y = 33, cex = 2, col = "blue",
                     labels = paste("Sum of squared residuals = ", 
                                    round(SSR, digits = 2)));
              }
            }else{
                plot(x = values$dat$x, y = values$dat$y, xlim = c(0, 6), 
                     ylim = c(-5, 35), ylab = "Temperature (\u00B0C)",
                     xlab = "Elevation (km)", cex.lab = 1.5, cex.axis = 1.5,
                     pch = 20, cex = 2);
                abline(a = input$b0, b = input$b1, col = "black", lwd = 3);
                if(values$eqn[[1]] > 0 & input$b1[[1]] > 0){
                    eqvl <- paste("y = ", input$b0, " + ", input$b1, "x", 
                                  sep = "");
                    text(x = 0.5, y = -3, labels = eqvl, 
                        cex = 2, col = "black", hjust = 0);
                }
                if(values$eqn > 0 & input$b1 <= 0){
                    eqvl <- paste("y = ", input$b0, " - ", -1 * input$b1, "x",
                                  sep = "");
                    text(x = 0.5, y = -3, labels = eqvl, 
                         cex = 2, col = "black", hjust = 0);
                }
                if(values$residuals > 0){
                    SSR <- 0;
                    for(i in 1:length(values$dat$x)){
                        x0 <- values$dat$x[i];
                        x1 <- values$dat$x[i];
                        y0 <- values$ypred[i];
                        y1 <- values$dat$y[i];
                        points(x = c(x0, x1), y = c(y0, y1), type = "b", 
                               lwd = 1, lty = "dotted", col = "red", pch = 20);
                        points(x = x0, y = y1, type = "b", lwd = 1, 
                               lty = "dotted", col = "black", pch = 20, 
                               cex = 2);
                        resi <- round(y1 - y0, digits = 2);
                        SSR  <- SSR + (resi * resi);
                        rpt  <- mean(c(y0, y1));
                        text(x = x0 + 0.15, y = rpt, labels = resi, col = "red",
                             hjust = 0);
                    } 
                    text(x = 4, y = 33, cex = 2,
                         labels = paste("Sum of squared residuals = ", 
                                        round(SSR, digits = 2)));
                }
            }
        }else{
            plot(x = 0, y = 0, type = "n", xlim = c(0, 6), ylim = c(-5, 35),
                 xlab = "Elevation (km)", ylab = "Temperature (\u00B0C)",
                 cex.lab = 1.5, cex.axis = 1.5);
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


