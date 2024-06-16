library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualising the F-statistic and p-values"),

    hr(),
    h4("The plot below shows the F-distribution for looking at the ratio of variances. Use the slider at the bottom to change the F ratio, then see how the probability of sampling this F ratio or higher changes. Use the numeric input to change the degrees of freedom."),
    hr(),

    plotOutput("distPlot"),

    fluidRow(
        sliderInput("V1", label = "Variance 1", width="100%", min = 0,
                    max = 10, value = 1, step = 0.01),
       sliderInput("V2", label = "Variance 2", width="100%", min = 0,
                max = 10, value = 1, step = 0.01)
    ),
    fluidRow(
        column(1, offset = 0),
        column(4, offset = 1,
               numericInput("df1", label = "Variance 1 df", width="100%",
                           min = 1, max = Inf, value = 4, step = 1),
               numericInput("df2", label = "Variance 2 df", width="100%",
                            min = 1, max = Inf, value = 4, step = 1)
        )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output){

    mbox <- function(x0, x1, y0, y1){
        xx <- seq(from=x0, to=x1, length.out = 100);
        yy <- seq(from=y0, to=y1, length.out = 100);
        xd <- c(rep(x0, 100), xx, rep(x1,100), rev(xx));
        yd <- c(yy, rep(y1,100), rev(yy), rep(y0, 100));
        return(list(x=xd, y=yd));
    }

    output$distPlot <- renderPlot({
        v1  <- input$V1;
        v2  <- input$V2;
        df1 <- input$df1;
        df2 <- input$df2;
        zz  <- pf(q = v1/v2, df1 = df1, df2 = df2, lower.tail = FALSE)
        xx  <- seq(from = 0, to = 10, by = 0.0001);
        yy  <- df(x = xx, df1 = df1, df2 = df2);
        o1  <- which(xx >= v1/v2);
        b1  <- which(xx <= v1/v2);
        par(mar = c(5, 5, 1, 1), lwd = 3);
        mxy <- max(yy[2:length(xx)]);
        ymx <- 1;
        if(mxy > 1 & df1 > 3 & df2 > 3){
          ymx <- mxy;
        }
        plot(x = xx, y = yy, type = "l", lwd = 4, cex.lab = 2, cex.lab = 2,
             cex.axis = 2, ylab = "Probability density", ylim = c(0, ymx),
             xlab = "F value", xlim = c(0, 5.5));
        polygon(c(xx[o1], max(xx), rev(xx[o1])),
                c(yy[xx==max(xx)], yy[o1], rep(0, length(xx[o1]))),
                col="#E69F00", lwd = 4);
        mxx <- which(xx == max(xx[b1]));
        polygon(c(xx[b1], max(xx[b1]), rev(xx[b1])),
                c(yy[b1], yy[mxx], rep(0, length(b1))),
                col="#56B4E9", lwd = 4);
        bx1y <- ymx;
        bx2y <- 0.91 * ymx;
        bx3y <- 0.81 * ymx;
        bx4y <- 0.90 * ymx;
        t1y  <- 0.955 * ymx;
        t2y  <- 0.855 * ymx;
        points(x = xx, y = yy, type = "l", lwd = 4);
        tbox <- mbox(x0 = 4, x1 = 5.5, y0 = bx2y, y1 = bx1y);
        text(x = 4.75, y = t1y, labels = "", cex = 1.5);
        polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="#56B4E9");
        tbox <- mbox(x0 = 4 , x1 = 5.5, y0 = bx3y, y1 = bx4y);
        text(x = 4.75, y = t2y, labels = "", cex = 1.5);
        polygon(x=tbox$x, y=tbox$y, lwd = 3, border="black", col="#E69F00");
        pbrown   <- round(pf(q = v1/v2, df1 = df1, df2 = df2,
                          lower.tail = FALSE), digits = 3);
        pblue    <- round(1 - pbrown, digits = 3);
        text(x = 4.75, y = t1y,
             labels = paste("P =", pblue), cex = 2);
        text(x = 4.75, y = t2y,
             labels = paste("P =", pbrown), cex = 2);
    })
}

# Run the application
shinyApp(ui = ui, server = server)


