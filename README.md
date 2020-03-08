# :yellow_heart: shinyAssemble
<hr>


shinyAssemble reads Shiny applications code ( one file `app.R` with `ui`, `server` ) <br>
to Visualize shiny application's structure ( as **Assembly Instruction Style** :stuck_out_tongue_winking_eye: ) <br>
note that, hovering node will show element's type ! <br>


### :wrench: Install
use this code

```R 

remotes::install_github('jhk0530/shinyAssemble')

library(shinyAssemble)

shinyAssemble()

```

## [Example, Hello Shiny](https://shiny.rstudio.com/articles/basics.html) 
 
![image](https://user-images.githubusercontent.com/6457691/76163323-6b305e80-6188-11ea-8a5e-acab60e8d788.png)
 
 
### Result
![image](https://user-images.githubusercontent.com/6457691/76163068-6e2a4f80-6186-11ea-8382-951f5ac82ab9.png)

 
### Codes
 
 ```R
 # Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(
        inputId = "bins",
        label = "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x,
      breaks = bins, col = "#75AADB", border = "white",
      xlab = "Waiting time to next eruption (in mins)",
      main = "Histogram of waiting times"
    )
  })
}

 ```

------

<details>
 
 <summary>

 ### [Example 2](https://shiny.rstudio.com/articles/basics.html) 

</summary>
 
 ### Codes
 
 ``` R
 # Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Shiny Text"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing dataset ----
      selectInput(
        inputId = "dataset",
        label = "Choose a dataset:",
        choices = c("rock", "pressure", "cars")
      ),

      # Input: Numeric entry for number of obs to view ----
      numericInput(
        inputId = "obs",
        label = "Number of observations to view:",
        value = 10
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
      "rock" = rock,
      "pressure" = pressure,
      "cars" = cars
    )
  })

  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })

  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
}

 ```
 
 ### Result
 
 ![image](https://user-images.githubusercontent.com/6457691/76163091-ab8edd00-6186-11ea-8411-7b97b168ef3d.png)

 
</details>
 
 
------


<details>
 <summary> 
 
 ### [Example 3](https://shiny.rstudio.com/articles/build.html) 
 
 </summary>
 
 ### Codes
 
 ``` R
 # Define UI for miles per gallon app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Miles Per Gallon"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for variable to plot against mpg ----
      selectInput(
        "variable", "Variable:",
        c(
          "Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear"
        )
      ),

      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption")),

      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
    )
  )
)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })

  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })

  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
      data = mpgData,
      outline = input$outliers,
      col = "#75AADB", pch = 19
    )
  })
}

 ```
 
 ### Result
 
 ![image](https://user-images.githubusercontent.com/6457691/76163108-cb260580-6186-11ea-8580-9d0e377f153b.png)

 
 </details>
 
 
 
### :paperclip: Dependency

* [shiny](https://github.com/rstudio/shiny) - 1.4.0
* [shinyCyJS](https://github.com/jhk0530/shinyCyJS) - 0.0.11
* [shinymaterial](https://github.com/ericrayanderson/shinymaterial) - 1.0.1

### :blush: Authors

* :octocat: jhk0530 [@jhk0530](https://github.com/jhk0530)

### :cloud: ShinyApps

https://jhkim.shinyapps.io/shinyAssemble/

### :memo: License
Copyright :copyright: 2020 Jinhwan Kim

This project is [MIT](https://opensource.org/licenses/MIT) licensed

*This README was generated with :two_hearts: by [shinyReadme](http://github.com/jhk0530/shinyReadme)*
