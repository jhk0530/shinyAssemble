if (!require("colourpicker")) install.packages("colourpicker")
if (!require("shiny")) install.packages("shiny")
if (!require("shinyCyJS")) remotes::install_github("jhk0530/shinyCyJS")
if (!require("shinymaterial")) install.packages("shinymaterial")

library(shinyCyJS)
library(shiny)
library(colourpicker)
library(shinymaterial)

exampleCode <- function() {
  '# Define UI for dataset viewer app ----
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
}'
}

solvedcard <- function(title, ..., depth = NULL, color = NULL, divider = FALSE, height = '90%') {
  shiny::tags$div(
    class = paste("card", ifelse(is.null(depth), "", paste0("z-depth-", depth)), ifelse(is.null(color), "", color)),
    style = paste0("height:",height),
    shiny::tags$div(
      class = "card-content",
      style = "height:90%;",
      shiny::tags$span(
        class = "card-title",
        title
      ),
      shiny::tags$div(
        class = paste0("", ifelse(divider, "divider", ""))
      ),
      ...
    )
  )
}

ui <- function() {
  material_page(
    title = "shinyAssemble <i style = 'margin-left:1em; font-size:2.5rem;' class = 'material-icons'>build</i>",
    shinyjs::useShinyjs(),
    background_color = "mybodystyle",
    nav_bar_color = "mynavstyle",
    tags$head(tags$style(type = "text/css", "html, body {height:100% !important; width:100% !important;}")),
    tags$head(tags$style(type = "text/css", ".form-group.shiny-input-container label { font-weight : bold;}")),
    tags$head(tags$style(type = "text/css", ".mybodystyle { background-color : #f7f1e3 !important;}")),
    tags$head(tags$style(type = "text/css", "#btn { float:right;}")),
    #tags$head(tags$style(type = "text/css", "#txt { font-size:1.3em;}")),
    tags$head(tags$style(type = "text/css", ".mynavstyle { background-color : #2c2c54 !important;}")),
    tags$head(tags$style(type = "text/css", ".shiny-input-container:not(.shiny-input-container-inline) {height:100% !important; width:100% !important;  margin-bottom : 1em !important;}")),
    div(style = 'height:1em;'),
    material_row(
      style = "height : 90%;",
      material_column(
        style = "height:100%;",
        solvedcard(
          title = "Code",
          height = '90%',
          textAreaInput(
            inputId = "txt",
            label = NULL,
            height = "100%",
            width = "100%",
            value = exampleCode()
          ),
          material_button("btn", "Investigate", icon = 'dashboard', color = 'mynavstyle'),
          depth = 3
        ),
        width = 6
      ),
      material_column(
        style = "height:90%;",
        solvedcard(
          title = '',
          height = '13.8%',
          material_column(colourInput(inputId = 'col', label = 'Input', value ='#9b59b6'), width = 2),
          material_column(colourInput(inputId = 'col2', label = 'Output', value ='#2ecc71'), width = 2),
          material_column(colourInput(inputId = 'col3', label = 'Render', value ='#FECA57'), width = 2),
          material_column(colourInput(inputId = 'col4', label = 'Reactive', value ='#FECA57'), width = 2),
          material_column(colourInput(inputId = 'col5', label = 'Data', value ='#48dbfb'), width = 2),
          material_column(colourInput(inputId = 'col6', label = 'Module', value ='#2c2c54'), width = 2),
          depth = 3
        ),
        solvedcard(
          title = "Assembly Information",
          height = '85%',
          div(
            ShinyCyJSOutput("cy", height = "100%"),
            style= 'background-color: #f7f1e3; height : 100%;'
          ),
          material_button(input_id = "btn2", label = "Module", icon = 'check_box_outline_blank', color = 'mynavstyle'),
          depth = 3
        ),
        width = 6
      )
    )
  )
}

server <- function(input, output, session) {
  myf <- function(codes, verbose = FALSE) {
    widgets <- list()
    splitted <- function(text) {
      strsplit(text, "")[[1]]
    }
    found <- function(pattern, text) {
      text <- tolower(text)
      grepl(pattern, text) && grepl("\\(", text)
    }

    detectWidget <- function(codes, i) {
      first <- splitted(codes[i])

      blocks <- i

      count <- unname(table(first)["("]) # count number of ( , it expected as 1 but not for inline cases

      v <- unname(table(first)[")"])
      if (!is.na(v)) count <- count - v
      if (count <= 0) {
        return(blocks)
      }

      # detect Widget Block
      for (j in (i + 1):length(codes)) {
        text <- splitted(codes[j])

        countOpen <- table(text)["("]
        countClose <- table(text)[")"]

        if (!is.na(countOpen)) count <- count + countOpen
        if (!is.na(countClose)) count <- count - countClose

        blocks <- c(blocks, j)
        if (count <= 0) {
          return(blocks)
        }
      }
    }
    # for addin
    # codes = rstudioapi::getSourceEditorContext()$contents

    # remove white spaces
    codes <- sapply(codes, function(i) {
      trimws(i)
    }, USE.NAMES = FALSE)

    # remove empty lines
    codes <- codes[which(codes != "")]

    # remove comments
    codes <- codes[which(substr(codes, 1, 1) != "#")]
    bookmark <- 1

    for (i in 1:length(codes)) {
      if (i <= bookmark) {
        next
      }
      fI <- found("input", codes[i])
      fO <- found("output", codes[i])
      fR <- found("reactive", codes[i])

      # used xor to avoid server.
      if (xor(fI, fO)) {
        blocks <- detectWidget(codes, i)
        widget <- paste0(codes[blocks], collapse = "\n")
        # widget = gsub('\\)', '\\) \n', widget)
        widgets[[length(widgets) + 1]] <- widget
        if (verbose) cat(widget, "\n\n")
        bookmark <- blocks[length(blocks)]
      }

      if (fR) {
        blocks <- detectWidget(codes, i)
        widget <- paste0(codes[blocks], collapse = "\n")
        # widget = gsub('\\)', '\\) \n', widget)
        widgets[[length(widgets) + 1]] <- widget
        if (verbose) cat(widget, "\n\n")
        bookmark <- blocks[length(blocks)]
      }

    }
    return(unique(widgets))
  }

  myf2 <- function(widgets, verbose = FALSE) {
    found <- function(pattern, text) {
      text <- tolower(text)
      grepl(pattern, text)
    }

    detect <- function(thisWidget, word) {
      text <- strsplit(thisWidget, "\\(")[[1]]

      if (word == "outputid") {
        idx <- which(grepl("output", tolower(text)))[1]
        if (length(idx)) {
          text <- text[idx:length(text)]
        }
      }
      if (word == "inputid") {
        idx <- which(grepl("input", tolower(text)))[1]
        if (length(idx)) {
          text <- text[idx:length(text)]
        }
      }

      type <- text[1]

      text[2] <- strsplit(text[2], ",")[[1]][1]
      if (found(word, tolower(text[2]))) { # inputid declared explictly
        text <- strsplit(text[2], ",")[[1]]
        for (j in 1:length(text)) {
          if (found(word, tolower(text[j]))) {
            id <- strsplit(text[j], "=")[[1]][2]
          }
        }
      }
      else {
        id <- strsplit(text[2], ",")[[1]]
      }
      type <- trimws(type)
      id <- trimws(id)
      id <- gsub("\\)", "", id)
      id <- gsub('"', "", id)
      if (verbose) cat("type :", type, "/ id :", id, "\n")
      return(list(type = type, id = id))
    }

    res <- list()

    # identify widget types
    for (i in 1:length(widgets)) {
      thisWidget <- widgets[[i]]

      # must render first, else render will detected as input or output
      source <- target <- method <- c()
      if (found("input\\$", thisWidget) || found("output\\$", thisWidget)) {
        if (found("reactive", thisWidget)) { # reactive
          if (verbose) cat("reactive value\n")
          text <- strsplit(thisWidget, "\n")[[1]]
          for (j in 1:length(text)) {
            thisText <- text[j]

            if (found("<-", thisText)) { # reactive
              thisText <- strsplit(thisText, "<-")[[1]][1] # remove reactive({
              target <- trimws(thisText[1])
              method <- "reactive"
              thisText <- text[j]
            }

            if (found("input\\$", thisText)) { # input$
              thisText <- strsplit(thisText, " ")[[1]]
              thisText <- thisText[which(grepl("input\\$", thisText))]
              thisText <- strsplit(thisText, "\\$")[[1]][2]
              thisText <- gsub("\\(", "", thisText)
              thisText <- gsub("\\)", "", thisText)
              thisText <- gsub(",", "", thisText) # remove ,
              source <- c(source, thisText)
            }
          }
          if (verbose) cat("target :", target, "method :", method, "source :", source, "\n")
          res[[length(res) + 1]] <- list(widget = "reactive", target = target, method = method, source = source)
          next
        }

        else { # render

          if (verbose) cat("rendering widget\n")
          text <- strsplit(thisWidget, "\n")[[1]]
          for (j in 1:length(text)) {
            thisText <- text[j]

            if (found("output\\$", thisText)) { # output$ and render
              thisText <- strsplit(thisText, "\\$")[[1]][-1] # remove output$
              thisText <- strsplit(thisText, "<-")[[1]]
              target <- trimws(thisText[1])
              method <- strsplit(trimws(thisText[-1]), "\\(")[[1]][1]
              thisText <- text[j]
            }

            if (found("input\\$", thisText)) { # input$
              thisText <- strsplit(thisText, " ")[[1]]

              thisText <- thisText[which(grepl("input\\$", thisText))]
              thisText <- strsplit(thisText, "\\$")[[1]][-1]
              thisText <- gsub("\\(", "", thisText)
              thisText <- gsub("\\)", "", thisText)
              thisText <- gsub(",", "", thisText)
              source <- c(source, thisText) # remove output$
            }
          }

          if (length(source) == 0) { # reactive data render
            for (j in 1:length(text)) {
              thisText <- text[j]
              if (found("<-", thisText) && found("\\(\\)", thisText)) {
                thisText <- trimws(strsplit(thisText, "<-")[[1]][-1])
                thisText <- gsub("\\(", "", thisText)
                thisText <- gsub("\\)", "", thisText)
                thisText <- gsub(",", "", thisText)
                source <- c(source, thisText)
              }
            }
          }
          if (length(source) == 0) { # exception 2
            for (j in 1:length(text)) {
              thisText <- text[j]
              if (found("\\(\\)", thisText)) {
                thisText <- trimws(strsplit(thisText, "\\(\\)")[[1]][1])
                thisText <- gsub("\\(", "", thisText)
                thisText <- gsub("\\)", "", thisText)
                thisText <- gsub(",", "", thisText)
                source <- c(source, thisText)
              }
            }
          }

          if(length(source) ==0 ) { # exception 3
            # output$Fun <- renderText({ asd(3, 4) })
            for(j in 1:length(text)){
              thisText <- text[j]
              if(found('\\(', thisText) && found('\\)', thisText)){
                source <- c(source, thisText)
              }
            }

          }

          if (verbose) cat("target :", target, "method :", method, "source :", source, "\n")

          res[[length(res) + 1]] <- list(widget = "render", target = target, method = method, source = source)
          next
        }
      } # render & reactive

      if (found("input", thisWidget)) {
        if (verbose) cat("input widget\n")
        w <- detect(thisWidget, "inputid")
        res[[length(res) + 1]] <- list(widget = "input", type = w$type, id = w$id)
        next
      } # input

      if (found("output", thisWidget)) {
        if (verbose) cat("output widget\n")
        w <- detect(thisWidget, "outputid")
        res[[length(res) + 1]] <- list(widget = "output", type = w$type, id = w$id)
        next
      } # output

      cat("unknown type error please create issue on github\n")
    }
    if(length(source)==0) cat("unknown type error please create issue on github\n")
    res = unique(res)
    return(res)
  }

  myf3 <- function(myf2res) {
    res <- list()

    for (i in 1:length(myf2res)) {

      thiswidget <- myf2res[[i]]

      if (thiswidget$widget == "input") {

        res[[length(res) + 1]] <- buildNode(
          id = thiswidget$id,
          shape = "rectangle",
          borderColor = input$col,
          borderWidth = 5,
          bgColor = "#FFFFFF",
          labelColor = "#000000",
          height = 75,
          width = 75,
          textbgOpacity = 1,
          textBorderWidth = 2,
          tooltip = thiswidget$type
        )
      }
      if (thiswidget$widget == "output") {

        res[[length(res) + 1]] <- buildNode(
          id = thiswidget$id,
          shape = "ellipse",
          borderColor = input$col2,
          borderWidth = 5,
          bgColor = "#FFFFFF",
          labelColor = "#000000",
          height = 75,
          width = 75,
          textbgOpacity = 1,
          textBorderWidth = 2,
          tooltip = thiswidget$type
        )
      }

      if (thiswidget$widget == "render") {
        res[[length(res) + 1]] <- buildEdge(
          source = thiswidget$source,
          target = thiswidget$target,
          label = thiswidget$method,
          lineColor = input$col3,
          curveStyle = "taxi",
          targetArrowShape = "triangle",
          targetArrowColor = input$col3
        )
      }

      if (thiswidget$widget == "reactive") {

        targets <- thiswidget$target
        sources <- thiswidget$source

        # handled multiple target and source

        for(i in 1:length(targets)){
          targetItem <- targets[i]
          res[[length(res) + 1]] <- buildNode(
            id = targetItem,
            shape = "diamond",
            borderColor = input$col5,
            borderWidth = 5,
            bgColor = "#FFFFFF",
            labelColor = "#000000",
            height = 75,
            width = 75,
            textbgOpacity = 1,
            textBorderWidth = 2,
            tooltip = thiswidget$type
          )
        }

        for(i in 1:length(sources)){
          sourceItem = sources[i]
          for(j in 1:length(targets)){
            targetItem <- targets[j]
            res[[length(res) + 1]] <- buildEdge(
              source = sourceItem,
              target = targetItem,
              label = thiswidget$method,
              curveStyle = "taxi",
              lineColor = input$col4,
              targetArrowShape = "triangle",
              targetArrowColor = input$col4
            )
          }
        }

      }
    }

    CheckDefined <- function(element){
      for(i in 1:length(res)){
        if(res[[i]]$group=='nodes'){
          if(res[[i]]$data$id == element) return(TRUE)
        }
      }
      return(FALSE)
    }

    for(k in 1:length(res)){ # check all source are defined.
      thisWidget <- res[[k]]
      if(thisWidget$group =='edges'){
        sources <- thisWidget$data$source
        targets <- thisWidget$data$target
        for(i in 1:length(sources)){
          if(!CheckDefined(sources[i])){ # undetected element
            res[[length(res) + 1]] <- buildNode(
              id = sources[i],
              shape = "star",
              borderColor = '#e84118',
              borderWidth = 5,
              bgColor = "#FFFFFF",
              labelColor = "#000000",
              height = 75,
              width = 75,
              textbgOpacity = 1,
              textBorderWidth = 2,
              tooltip = sources[i]
            )
          }
        }
        for(i in 1:length(targets)){
          if(!CheckDefined(targets[i])){ # undetected element
            res[[length(res) + 1]] <- buildNode(
              id = targets[i],
              shape = "star",
              borderColor = '#e84118',
              borderWidth = 5,
              bgColor = "#FFFFFF",
              labelColor = "#000000",
              height = 75,
              width = 75,
              textbgOpacity = 1,
              textBorderWidth = 2,
              tooltip = targets[i]
            )
          }
        }


      }

    }


    return(res)
  }

  observeEvent(input$btn2, {
    if(input$btn2==0){return(NULL)}

    shinyjs::runjs("
    var nodes = cy.nodes();
    var moduleCnt = 0;
    while(nodes.length>0){

      var familly = nodes[0].component();

      moduleCnt = moduleCnt + 1;
      var newModule = {
        group:'nodes',
        data : {
          backgroundOpacity:1,
          shape:'rectangle',
          borderWidth:5,
          borderColor: $('#col6')[0].value,
          borderOpacity: 1,
          textOpacity: 1,
          id : 'module' + moduleCnt,
          label : 'module' + moduleCnt,
          labelColor:'#8395a7',
          width : 15,height:15,
          bgColor:'#48d8f8',
          bgOpacity:0, bgFill : 'solid',
          bgBlacken : 0, borderStyle : 'solid',
          textOpacity:0,
          fontSize:16,
          textOutlineColor:'#222f3e',
          textOutlineOpacity:1,
          textOutlineWidth:0,
          textbgColor:'#fff',
          textbgOpacity:0,
          textBorderColor:'#222f3e',
          textBorderOpacity:0,
          textBorderWidth:0,
          parent:null,
          opacity:1,
          pieSize1 : '0%', pieSize2 : '0%', pieSize3 : '0%', pieSize4 : '0%',
          pieSize5 : '0%', pieSize6 : '0%', pieSize7 : '0%', pieSize8 : '0%',
          pieSize9 : '0%', pieSize10 : '0%', pieSize11 : '0%', pieSize12 : '0%',
          pieSize13 : '0%', pieSize14 : '0%', pieSize15 : '0%', pieSize16 : '0%',
          pieColor1 : '#000', pieColor2 : '#000', pieColor3 : '#000', pieColor4 : '#000',
          pieColor5 : '#000', pieColor6 : '#000', pieColor7 : '#000', pieColor8 : '#000',
          pieColor9 : '#000', pieColor10 : '#000', pieColor11 : '#000', pieColor12 : '#000',
          pieColor13 : '#000', pieColor14 : '#000', pieColor15 : '#000', pieColor16 : '#000',
          tooltip:''
        }
      }
      cy.add( newModule )
      familly.move({ parent : 'module' + moduleCnt })

      nodes = nodes.difference(familly)
    }

")
  })

  observeEvent(input$btn, {
    if (input$btn == 0) {
      return(NULL)
    }
    code <- strsplit(input$txt, split = "\n")[[1]]
    shinyjs::runjs('var moduleCnt = 0;')
    widgets <- myf(code)
    myfres <- myf2(widgets)
    shinyobj <- myf3(myfres)
    output$cy <- renderShinyCyJS(shinyCyJS(shinyobj))
  })
}

shinyApp(ui = ui(), server = server, options = list(launch.browser = TRUE))
