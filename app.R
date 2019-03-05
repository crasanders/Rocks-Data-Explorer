source("header.R")

server = function(input, output, session) {
  ########## MDS PLOTTING TOOL ##########
  
  selectall = function(button, id, choices, session){
    if (button > 0) {
      if (button %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId=id,
                                 choices = choices,
                                 selected = choices)
        
      } else {
        updateCheckboxGroupInput(session=session, 
                                 inputId=id,
                                 choices = choices,
                                 selected = c())
        
      }}
  }
  
  makePlot = function(plotopts){
    data = plotopts$data
    selected = is.element(data$Subtype, plotopts$igChoices) | 
      is.element(data$Subtype, plotopts$metChoices) |
      is.element(data$Subtype, plotopts$sedChoices)
    x = data[selected,plotopts$x]
    y = data[selected,plotopts$y]
    p = ggplot(data[selected,], 
               aes(x, y, image=image)) + 
      geom_rock() +
      theme_classic() +
      coord_cartesian(xlim = plotopts$ranges$x, ylim = plotopts$ranges$y) +
      xlab(colnames(data)[plotopts$x]) + 
      ylab(colnames(data)[plotopts$y])
    
    if (plotopts$correlation){
      fit = lm(y ~ x)
      r = cor(x, y)
      
      theta <- seq(pi/8, 2*pi, length.out=16)
      xo <- diff(range(x))/200
      yo <- diff(range(y))/200
      for(i in theta) {
        p = p + annotate("text", y = min(y)+(sin(i)*yo), x = max(x)+(cos(i)*xo),label=paste("r =", round(r, 3)),hjust=1, size = 10)
      }
      
      p = p + geom_abline(slope = fit$coefficients[[2]], intercept = fit$coefficients[[1]], size = 2, linetype = 2) +
        annotate("text", y = min(y), x = max(x),label=paste("r =", round(r, 3)),hjust=1, size = 10, colour = 'white')
      
    }
    return(p)
  }
  
  ##### REACTIVE VALUES #####
  
  plotopts = reactiveValues(x = 1,
                            y = 2,
                            igChoices = rocks[1:10],
                            metChoices = rocks[11:20],
                            sedChoices = rocks[21:30], 
                            data = mds_30,
                            info = empty,
                            ranges = list(x = NULL, y = NULL),
                            correlation = FALSE)
  
  ##### OBSERVATIONS #####
  
  observeEvent(input[["plotButton"]],
               {
                 plotopts$x = as.integer(input[['x']])
                 plotopts$y = as.integer(input[['y']])
                 plotopts$igChoices = input[['igneous']]
                 plotopts$metChoices = input[['metamorphic']]
                 plotopts$sedChoices = input[['sedimentary']]
                 if (input[['dataToggle']] == 30)
                   plotopts$data = mds_30
                 if (input[['dataToggle']] == 360)
                   plotopts$data = mds_360
                 plotopts$ranges$x = NULL
                 plotopts$ranges$y = NULL
                 plotopts$info = empty
                 plotopts$correlation = input[["correlation"]]
               })
  
  observeEvent(input[['plot_click']],
               {
                 click = input[['plot_click']]
                 clickx = click$x
                 clicky = click$y
                 mds = plotopts$data
                 r = range(cbind(mds[,plotopts$x], mds[,plotopts$y]))
                 dist = (r[2] - r[1]) / 10
                 if (knnx.dist(mds[,c(plotopts$x, plotopts$y)], cbind(clickx, clicky), 1) < dist){
                   nearest = knnx.index(mds[,c(plotopts$x, plotopts$y)], cbind(clickx, clicky), 1)
                   subtype = mds[nearest, 'Subtype']
                   if (is.element(subtype, plotopts$igChoices) || 
                       is.element(subtype, plotopts$metChoices) ||
                       is.element(subtype, plotopts$sedChoices)){
                     plotopts$info = t(mds[nearest,c(13, 34, cols)])
                   }
                 }
               })
  
  observeEvent(input[['plot_dblclick']], {
    brush = input[['plot_brush']]
    if (!is.null(brush)) {
      plotopts$ranges$x = c(brush$xmin, brush$xmax)
      plotopts$ranges$y = c(brush$ymin, brush$ymax)
      
    } else {
      plotopts$ranges$x = NULL
      plotopts$ranges$y = NULL
    }
  })
  
  observe({selectall(input[['igselectall']], "igneous", rocks[1:10], session)})
  observe({selectall(input[['metselectall']], "metamorphic", rocks[11:20], session)})
  observe({selectall(input[['sedselectall']], "sedimentary", rocks[21:30], session)})
  
  
  ##### OUTPUT #####
  
  output[['downloadPlot']] = downloadHandler(
    filename = "Rocks Plot.jpeg",
    content = function(file) {
      ggsave(file, plot = makePlot(plotopts), device = "jpg", height = 12, width = 12, dpi = 400)
    }
  )
  
  output[['plot']] = renderPlot({
    makePlot(plotopts)
  })
  
  output[['click_info']] = renderDataTable({
    datatable(plotopts$info, escape = FALSE, colnames = c(""), options = list(paging = FALSE, searching = FALSE, ordering = FALSE))
  })
  
  ########## MDS SOLUTION ##########
  
  ##### OUTPUT #####
  
  output$downloadmds = downloadHandler(
    filename = "Rocks Table.csv",
    content = function(file){
      if (input$mdsdata == 30)
        data = mds_30
      if (input$mdsdata == 360)
        data = mds_360
      write.csv(data[,cols], file)
    }
  )
  
  output$mdstable = renderDataTable({
    if (input$mdsdata == 30)
      data = mds_30
    if (input$mdsdata == 360)
      data = mds_360
    datatable(data[,c(13, 34, cols)], escape = FALSE, extensions = 'Buttons', options = list(paging = FALSE, dom = 'Bfrtip', buttons = I('colvis')), callback = JS(hovertext))
  })
  
  ########## SIMILARITY MATRIX ##########
  output$downloadsimilaritymatrix = downloadHandler(
    filename = "Rocks Similarity Matrix.csv",
    content = function(file){
      if (input$simdata == 30)
        data = similarity_30
      if (input$simdata == 360)
        data = similarity_360
      write.csv(data, file)
    }
  )
  
  output$similaritytable = renderDataTable({
    if (input$simdata == 30){
      data = similarity_30
      options = list(searching = FALSE, paging = FALSE)
    }
    if (input$simdata == 360){
      data = similarity_360
      options = list(searching = FALSE, lengthMenu = list(c(12, 120, -1), c('12', '120', 'All')))
    }
    datatable(data, options)
  })
  
}



ui = navbarPage("Rocks Data Explorer",
                
  ########## ABOUT ##########
  
  tabPanel("About This Site",
           tags$p("This site contains interactive tools for exploring the data reported in", 
                  tags$a(href="https://link.springer.com/article/10.3758/s13428-017-0884-8",
                         "Nosofsky, Sanders, Meagher, and Douglas (2018). Toward the Development of a Feature-Space Representation for a Complex Natural Category Domain. ",
                  tags$i('Behavior Research Methods,'), "50, 530-556.")),
           tags$p("The purpose of this study was to understand which aspects of natural stimuli the human mind pays attention to, with the goal of developing psychological 
                   theories of representation and categorization. We used categories of rocks as our example stimuli, but our methods and findings can be generalized to 
                   other science categories, such as categories of species, stars, or diseases. We collected two data sets in this study: one using 30 rocks, and one using 360 rocks"),
           tags$p('In this study, we asked human subjects to judge the similarity between pairs of rocks on a 1-9 scale (1 being most dissimilar and 9 being most similar), and we also asked the 
                   human subjects to rate each individual rock along a variety of dimensions hypothesized to be psychologically-relevant. We used these similarity judgments and dimension ratings
                   to derive an 8-dimensionsal multidimensional-scaling (MDS) solution. This MDS solution can be used to understand which dimensions of the rocks people pay
                   attention to. The MDS dimensions can be interpreted in terms of lightness of color, average grain size, roughness, shininess, organization, chromaticity, shape, and hue.
                   See the article linked above for more details.'),
           tags$p('The ', tags$b('Plotting Tool'), ' tab provides a tool that can create plots of the rocks along each of the derived MDS dimensions and the direct dimension ratings.'),
           tags$p('The ', tags$b('MDS Solution and Dimension Ratings'), ' tab provides a table that lists the MDS coordinates and average direct dimension ratings for each of the rocks.'),
           tags$p('The ', tags$b('Similarity Ratings'), ' tab provides a table that lists the average similarity judgment between each pair of rocks.'),
           tags$p("The data and stimuli from this study can be found", tags$a(href="https://osf.io/w64fv/", tags$b("here")), '.')
           
           ),
  
  ########## MDS PLOTTING TOOL ##########
  
  tabPanel("Plotting Tool",
           sidebarLayout(
             sidebarPanel(
               helpText('Select your desired options below, and click "Create Plot" to display the plot.'),
               actionButton("plotButton", tags$b("Create Plot")),
               downloadButton("downloadPlot", tags$b("Download Plot")),
               
               radioButtons("dataToggle", label = h4("Data Set"),
                            choices = list("30 Rocks" = 30,
                                           "360 Rocks" = 360),
                            selected = 30
               ),
               
               checkboxInput("correlation", label = ("Compute Correlation")),
               
               selectInput("x", 
                           label = h4("X Axis"),
                           choices = dimensions,
                           selected = 1),
               
               selectInput("y", 
                           label = h4("Y Axis"),
                           choices = dimensions,
                           selected = 2),
               
               helpText("Check below the rocks you want to display."),
               checkboxGroupInput("igneous", label = h4("Igneous Rocks"), 
                                  choices = rocks[1:10],
                                  selected = rocks[1:10]),
               actionButton("igselectall","Select/Deselect All"),
               
               checkboxGroupInput("metamorphic", label = h4("Metamorphic Rocks"), 
                                  choices = rocks[11:20],
                                  selected = rocks[11:20]),
               actionButton("metselectall","Select/Deselect All"),
               
               checkboxGroupInput("sedimentary", label = h4("Sedimentary Rocks"), 
                                  choices = rocks[21:30],
                                  selected = rocks[21:30]),
               actionButton("sedselectall","Select/Deselect All")
             ),
             
             mainPanel(helpText("Click and drag to draw a rectangle. Double-click to zoom in on the rectangle. Double-click again to zoom out."),
                       plotOutput("plot",
                                  click = "plot_click",
                                  dblclick = "plot_dblclick",
                                  brush = brushOpts(
                                    id = "plot_brush",
                                    resetOnNew = TRUE
                                  ),
                                  height = size, 
                                  width = size),
                       helpText("Click on a rock to see a larger picture of it as well as its category, subtype, and precise MDS coordinates."),
                       dataTableOutput("click_info")
             )
           )),
  
  ########## MDS SOLUTION ##########
  
  tabPanel("MDS Solution and Dimension Ratings",
           radioButtons("mdsdata", label = "Data Set",
                        choices = list("30 Rocks" = 30,
                                       "360 Rocks" = 360),
                        selected = 30
           ),
           downloadButton("downloadmds", "Download Table as .csv"),
           helpText('Each row in this table represents one of the rocks. Each column represents the value of that rock along a particular dimension.
                    Hover the cursor over a column label for more details. Click on the arrows next to a column label to sort the rows along that column.
                    Click the "Column Visibility" button to show or hide specific columns.'),
           dataTableOutput("mdstable")
           ),
  
  ########## SIMILARITY MATRIX ##########
  tabPanel("Similarity Ratings",
           radioButtons("simdata", label = "Data Set",
                        choices = list("30 Rocks" = 30,
                                       "360 Rocks" = 360),
                        selected = 30
           ),
           downloadButton("downloadsimilaritymatrix", "Download Matrix as .csv"),
           helpText('Each row and each column in this table represents one of the rocks. Each row and each column label takes the form "Category_Subtype_Token".
                    Cell i,j in this table indicates the average similarity rating of rocks i and j.
                    Empty cells indicate that no similarity ratings were collected for that pair of rocks.
                    Click on the arrows next to a column label to sort the rows along that column.'),
           dataTableOutput("similaritytable")
           )
)

shinyApp(ui = ui, server = server)
