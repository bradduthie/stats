library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("An intuitive understanding of mean and standard deviation"),
    
    hr(),
    h4("Use the sliders at the bottom to change the mean and standard deviation of tree heights in the forest"),
    hr(),
    
    plotOutput("distPlot"),
    
    fluidRow(
        column(4, offset = 1,
               sliderInput("Mean", label = "Mean", width="100%", min = 10, 
                           max = 25, value = 15, step = 1), 
        ),
        column(1, offset = 0),
        column(4, offset = 1,
            sliderInput("SD", label = "Standard Deviation (SD)", width="100%",
                        min = 0, max = 4, value = 0, step = 0.25)
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
    
    build_land <- function(top = 30, ground = 2){
        par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0));
        plot(x = 0, y = 0, type = "n", xlim = c(0, 100), ylim = c(0, top),
             xlab = "", ylab = "", xaxt = "n", yaxt = "n");
        grass <- mbox(x0 = 0, x1 = 100, y0 = 0, y1 = ground);
        sky   <- mbox(x0 = 0, x1 = 100, y0 = ground, y1 = 300);
        polygon(x = grass$x, y = grass$y, col = "lightgreen", lwd = 0.01);
        polygon(x = sky$x, y = sky$y, col = "lightblue", lwd = 0.01);
    }

    
    tree <- function(loc, height){
        radiu <- height * 0.05;
        trunk <- mbox(x0 = loc - radiu, x1 = loc + radiu, y0 = 0, y1 = height);
        polygon(x = trunk$x, y = trunk$y, lwd = 1, col = "brown");
        points(x = loc, y = height, pch = 21, cex = radiu*8, bg = "darkgreen");
    }
    
    trees <- function(n, mean, sd){
        tr  <- seq(from = 10, to = 90, length = n);
        hgt <- rnorm(n = n, mean = mean, sd = sd);
        hgt[hgt < 0.25] <- 0.25;
        for(i in 1:n){
            tree(loc = tr[i], height = hgt[i]);
        } 
    }
    
    forest <- function(n = 15, mean, sd){
        build_land();
        trees(n = n, mean = mean, sd = sd);
    }
    
    output$distPlot <- renderPlot({
        Mn  <- as.numeric(input$Mean);
        Sd  <- as.numeric(input$SD);
        forest(mean = Mn, sd = Sd);
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
