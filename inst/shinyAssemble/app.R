library(shinyCyJS)
library(shiny)
library(shinymaterial)
library(styler)

exampleCode = function(){'# Define UI for app that draws a histogram ----
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
            }'}

solvedcard = function(title, ..., depth = NULL, color = NULL, divider = FALSE){
  shiny::tags$div(
    class = paste("card", ifelse(is.null(depth), "", paste0("z-depth-", depth)), ifelse(is.null(color), "", color)),
    style = 'height:90%;',
    shiny::tags$div(
      class = "card-content",
      style = 'height:90%;',
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

ui = function(){
  material_page(
    title = 'shinyAssemble',
    tags$head(tags$style(type = "text/css", "html, body {height:100%; width:100%; !important;}")),
    tags$head(tags$style(type = "text/css", ".shiny-input-container:not(.shiny-input-container-inline) {height:100% !important; width:100% !important;  margin-bottom : 1em !important;}")),

    material_row(style = 'height : 90%;',
      material_column(
        style = 'height:100%;',
        solvedcard(
          title = 'code',
          textAreaInput(
            'txt',
            label = 'code here',
            height = '100%',
            width = '100%',
            value = exampleCode()
          ),
          material_button('btn', 'push')
        ),
        width = 6
      ),
      material_column(
        style = 'height:100%;',
        solvedcard(
          title = 'assemble information',
          ShinyCyJSOutput('cy',height = '100%')
        ),
        width = 6
      )
    )
  )
}

server = function(input, output, session){
  myf = function(codes, verbose = FALSE){
    widgets = list()
    splitted = function(text){
      strsplit(text,'')[[1]]
    }
    found = function(pattern, text){
      text = tolower(text)
      grepl(pattern, text) && grepl('\\(', text)
    }

    detectWidget = function(codes, i){
      first = splitted(codes[i])

      blocks = i

      count = unname(table(first)['('])   # count number of ( , it expected as 1 but not for inline cases

      v = unname(table(first)[')'])
      if(!is.na(v)) count = count - v
      if(count <= 0){return(blocks)}

      # detect Widget Block
      for(j in (i+1):length(codes)){

        text = splitted(codes[j])

        countOpen = table(text)['(']
        countClose = table(text)[')']

        if(!is.na(countOpen)) count = count + countOpen
        if(!is.na(countClose)) count = count - countClose

        blocks = c(blocks, j)
        if(count <= 0){return(blocks)}

      }

    }
    # for addin
    # codes = rstudioapi::getSourceEditorContext()$contents

    # remove white spaces
    codes = sapply(codes, function(i){trimws(i)}, USE.NAMES = FALSE)

    # remove empty lines
    codes = codes[which(codes != '')]

    # remove comments
    codes = codes[which(substr(codes,1,1)!='#')]
    bookmark = 1

    for(i in 1:length(codes)){
      if(i <= bookmark){next}
      fI = found('input', codes[i])
      fO = found('output', codes[i])
      fR = found('reactive', codes[i])
      # used xor to avoid server.
      if(xor(fI,fO)){
        blocks = detectWidget(codes, i)
        widget = paste0(codes[blocks], collapse = '\n')
        #widget = gsub('\\)', '\\) \n', widget)
        widgets[[length(widgets)+1]] = widget
        if(verbose) cat(widget ,'\n\n')
        bookmark = blocks[length(blocks)]
      }

      if(fR){
        blocks = detectWidget(codes, i)
        widget = paste0(codes[blocks], collapse = '\n')
        #widget = gsub('\\)', '\\) \n', widget)
        widgets[[length(widgets)+1]] = widget
        if(verbose) cat(widget ,'\n\n')
        bookmark = blocks[length(blocks)]
      }
    }
    return(widgets)
  }
  myf2 = function(widgets, verbose = FALSE){

    found = function(pattern, text){
      text = tolower(text)
      grepl(pattern, text)
    }

    detect = function(thisWidget, word){

      text = strsplit(thisWidget,'\\(')[[1]]

      if(word == 'outputid'){
        idx = which(grepl('output', tolower(text)))[1]
        if(length(idx)){
          text = text[idx:length(text)]
        }
      }
      if(word == 'inputid'){
        idx = which(grepl('input', tolower(text)))[1]
        if(length(idx)){
          text = text[idx:length(text)]
        }
      }


      type = text[1]

      text[2] = strsplit(text[2],',')[[1]][1]
      if(found(word,tolower(text[2]) )) { # inputid declared explictly
        text = strsplit(text[2],',')[[1]]
        for(j in 1:length(text)){
          if( found(word, tolower(text[j]))){
            id = strsplit(text[j],'=')[[1]][2]
          }
        }
      }
      else{
        id = strsplit(text[2],',')[[1]]
      }
      type = trimws(type)
      id = trimws(id); id = gsub('\\)', '', id); id = gsub('"','',id)
      if(verbose) cat('type :',type, '/ id :', id, '\n')
      return(list (type = type, id = id))
    }

    res = list()

    # identify widget types
    for(i in 1:length(widgets)){
      thisWidget = widgets[[i]]

      # must render first, else render will detected as input or output
      source = target = method = c()
      if(found('input\\$', thisWidget) || found('output\\$', thisWidget)) {

        if(found('reactive', thisWidget)){ # reactive
          if(verbose) cat('reactive value\n')
          text = strsplit(thisWidget,'\n')[[1]]
          for(j in 1:length(text)){
            thisText = text[j]

            if(found('<-', thisText)){ # reactive
              thisText = strsplit(thisText, '<-')[[1]][1] # remove reactive({
              target = trimws(thisText[1])
              method = 'reactive'
              thisText = text[j]
            }

            if(found('input\\$', thisText)){ # input$
              thisText = strsplit(thisText, ' ')[[1]]
              thisText = thisText[which(grepl('input\\$',thisText))]
              thisText = strsplit(thisText, '\\$')[[1]][2]
              thisText = gsub('\\(','',thisText)
              thisText = gsub('\\)','',thisText)
              thisText = gsub(',','',thisText) # remove ,
              source = c(source, thisText)
            }
          }
          if(verbose) cat('target :', target, 'method :', method, 'source :', source, '\n')
          res[[length(res)+1]] = list(widget = 'reactive', target = target, method = method, source = source)
          next
        }

        else{ # render

          if(verbose) cat('rendering widget\n')
          text = strsplit(thisWidget,'\n')[[1]]
          for(j in 1:length(text)){
            thisText = text[j]

            if(found('output\\$', thisText)){ # output$ and render
              thisText = strsplit(thisText,'\\$')[[1]][-1] # remove output$
              thisText = strsplit(thisText, '<-')[[1]]
              target = trimws(thisText[1])
              method = strsplit(trimws(thisText[-1]), '\\(')[[1]][1]
              thisText = text[j]
            }

            if(found('input\\$', thisText)){ # input$
              thisText = strsplit(thisText, ' ')[[1]]

              thisText = thisText[which(grepl('input\\$',thisText))]
              thisText = strsplit(thisText,'\\$')[[1]][-1]
              thisText = gsub('\\(','',thisText)
              thisText = gsub('\\)','',thisText)
              thisText = gsub(',','',thisText)
              source = c(source, thisText ) # remove output$
            }

          }

          if(length(source)==0){ # reactive data render
            for(j in 1:length(text)){
              thisText = text[j]
              if(found('<-', thisText) && found('\\(\\)', thisText)){
                thisText = trimws(strsplit(thisText, '<-')[[1]][-1])
                thisText = gsub('\\(','',thisText)
                thisText = gsub('\\)','',thisText)
                thisText = gsub(',','',thisText)
                source = c(source, thisText)
              }
            }
          }
          if(length(source)==0){ # exception 2
            for(j in 1:length(text)){
              thisText = text[j]
              if(found('\\(\\)', thisText)){
                thisText = trimws(strsplit(thisText, '\\(\\)')[[1]][1])
                thisText = gsub('\\(','',thisText)
                thisText = gsub('\\)','',thisText)
                thisText = gsub(',','',thisText)
                source = c(source, thisText)
              }
            }
          }

          if(verbose) cat('target :', target, 'method :', method, 'source :', source, '\n')

          res[[length(res)+1]] = list(widget = 'render', target = target, method = method, source = source)
          next
        }


      } # render & reactive

      if(found('input', thisWidget)) {
        if(verbose) cat('input widget\n')
        w = detect(thisWidget, 'inputid')
        res[[length(res)+1]] = list(widget = 'input', type = w$type, id = w$id)
        next
      } # input

      if(found('output', thisWidget)){
        if(verbose) cat('output widget\n')
        w = detect(thisWidget, 'outputid')
        res[[length(res)+1]] = list(widget = 'output', type = w$type, id = w$id)
        next
      } # output

      cat('unknown type error please create issue on github\n')
    }
    return(res)
  }
  myf3 = function(myf2res){
    res = list()
    for(i in 1:length(myf2res)){
      thiswidget = myf2res[[i]]
      if(thiswidget$widget == 'input'){
        res[[length(res)+1]] = buildNode(
          id = thiswidget$id,
          shape = 'rectangle',
          borderColor = '#9b59b6',
          borderWidth = 5,
          bgColor = '#FFFFFF',
          labelColor = '#000000',
          height = 75,
          width = 75,
          textbgOpacity = 1,
          textBorderWidth = 2,
          tooltip = thiswidget$type
        )
      }
      if(thiswidget$widget == 'output'){
        res[[length(res)+1]] = buildNode(
          id = thiswidget$id,
          shape = 'ellipse',
          borderColor = '#2ecc71',
          borderWidth = 5,
          bgColor = '#FFFFFF',
          labelColor = '#000000',
          height = 75,
          width = 75,
          textbgOpacity = 1,
          textBorderWidth = 2,
          tooltip = thiswidget$type
        )
      }
      if(thiswidget$widget == 'render'){
        res[[length(res)+1]] = buildEdge(
          source = thiswidget$source,
          target = thiswidget$target,
          label = thiswidget$method,
          curveStyle = 'taxi',
          targetArrowShape = 'triangle',
          targetArrowColor = '#FECA57'
        )
      }

      if(thiswidget$widget == 'reactive'){

        res[[length(res)+1]] = buildNode(
          id = thiswidget$target,
          shape = 'diamond',
          borderColor = '#48dbfb',
          borderWidth = 5,
          bgColor = '#FFFFFF',
          labelColor = '#000000',
          height = 75,
          width = 75,
          textbgOpacity = 1,
          textBorderWidth = 2,
          tooltip = thiswidget$type
        )

        res[[length(res)+1]] = buildEdge(
          source = thiswidget$source,
          target = thiswidget$target,
          label = thiswidget$method,
          curveStyle = 'taxi',
          targetArrowShape = 'triangle',
          targetArrowColor = '#FECA57'
        )
      }
    }
    return(res)
  }

  observeEvent(input$btn, {
    if(input$btn == 0){ return(NULL) }
    code = styler::style_text(input$txt)
    code = strsplit(code,split = '\n')[[1]]

    widgets = myf(code)
    myfres = myf2(widgets)
    shinyobj = myf3(myfres)
    output$cy = renderShinyCyJS(shinyCyJS(shinyobj))
  })
}

# shinyApp(ui = ui(), server = server, options = list(launch.browser = TRUE))
