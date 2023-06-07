library(dplyr)
library(shiny)
library(leaflet)
library(tmap)
library(sf)
library(mapview)
library(DT)

# an original data set for starting the app
dat <- readRDS("fruitTreeData.rds")
dat
# make a copy in case I need it
datOG <- readRDS("originalFruitTreeData.rds")

# highlight markers
greycircle <- makeIcon(
  iconUrl = "https://www.vhv.rs/dpng/d/425-4254086_circle-gray-circulo-png-forma-grey-circle-png.png",
  iconWidth =10, iconHeight = 10,
  iconAnchorX = 0, iconAnchorY = 0,
)
greencircle <- makeIcon(
  iconUrl = "https://www.clker.com/cliparts/k/I/c/l/f/f/green-circle-hi.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 0, iconAnchorY = 0,
)

marker<- makeIcon(
  iconUrl = "https://img.favpng.com/1/24/20/computer-icons-symbol-icon-design-pointer-map-png-favpng-Hz87qapyjWEyjWSBFdmH6CHW6.jpg",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY = 0,
)

# begin ui
ui <- fluidPage(
  # tags
  tags$head(
    # css style sheet
    # tags$link(rel = "stylesheet", type="text/css", href="with_map/www/style.css")
    tags$style(HTML("

@import url('https://fonts.googleapis.com/css2?family=Lobster+Two:ital,wght@1,700&display=swap');
@import url('https://fonts.googleapis.com/css2?family=Merriweather&display=swap');
@import url('https://fonts.googleapis.com/css2?family=Lobster+Two:ital,wght@1,700&family=Sriracha&display=swap');
      body {
      background: #AACFF0;
      margin: 1;
      font-family: 'Merriweather';

      }

      h1 {
      font-family:  'Lobster Two', cursive;
      font-size: 100px;
      text-align: center;
      margin: 1;
      color: white;
      background-image: url('https://newenglandapples.files.wordpress.com/2014/10/img_5595.jpg');
      background-size: 1400px;
      background-position: center;
      border: 2px black solid;
      background-repeat: no-repeat, repeat;
      padding: 10px;
      border-radius: 10px;


      }

      #add {
      color: white;
      font-family:  'Lobster Two', cursive;
      text-align: center;
      background-image: url('https://idsb.tmgrup.com.tr/ly/uploads/images/2020/06/10/40174.jpg');
      background-size: 1500px;
      background-position: center top;
      border: 2px black solid;
      background-repeat: no-repeat, repeat;
      margin: 1;
      padding: 10px;
      border-radius: 10px;
      }

      #verify {
      color: white;
      font-family:  'Lobster Two', cursive;
      text-align: center;
      background-image: url('https://www.verywellfit.com/thmb/aUqaS0dwAoTNmR2vSwhZpHy2e5k=/5700x3815/filters:fill(FFDB5D,1)/GettyImages-121330290-582dcb383df78c6f6ac67f94.jpg');
      background-size: 1500px;
      border: 2px black solid;
      background-repeat: no-repeat, repeat;
      margin: 1;
      padding: 10px;
      border-radius: 10px;
      }

      p{
      background-color: #F5C387;
      border: 1px black solid;
      padding: 4px;
      text-align: center;
      border-radius: 5px;

      }

      .btn {
      border-radius: 10%;
      border: 1px solid black;

      }

                    "

    ))
  ),

  h1("Fruitfull"),
  p("Fruitfull allows users to contribute to a database documenting public
  fruit trees in and around the city of Bellingham, Washington. The app's primary goal is to build
  a comprehensive database of these trees, allowing anyone seasonal access to free, fresh fruit only minutes
  from their front door.
     "),

  fluidRow(
    column(6,
           tmapOutput(outputId = "tmapMap", height=500)
    ),
    column(6,
           DT::dataTableOutput("updatedData")
    )
  ),
  fluidRow(
    h2("Add", id= "add"),
    p("To add a public fruit tree to the database click on the map, choose species, add a comment, then click 'Add Tree to Database.'
      If you are confident in the changes you've made click 'Save Changes.' Please only add public fruit trees to the
      database. A tree is considered public if it's trunk is entirley situated on public land. If you are unsure weather
      or not a tree is public, add a comment and leave it unverified."),

    column(4,
           numericInput(inputId = "lat",
                        label = "Latitude",
                        value = 48.7211,step = 1e-3)
    ),
    column(4,
           numericInput(inputId = "long",
                        label = "Longitude",
                        value=-122.4942,step = 1e-3)
    ),
    #


    column(4,
           selectInput(inputId = "species", label = "Species",
                       choices = c("Apple","Pear","Fig","Peach","Other"))
    ),
    column(2, h3(""),
           actionButton(inputId = "addObservation", label = "Add Tree")
    ),
    column(2, h3(""),
    uiOutput("saveChanges")
    ),

    column(3, textInput(inputId = 'comment',
                        label ='Comments',
                        value = "", width = NULL,
                        placeholder = NULL)),
  ),
  fluidRow(
    h2("Verify", id= "verify"),
    p("The status of a tree is confirmed when all aspects of its table entrty (locatiton, species, public)
    are confirmed to be correct. To verify existence of an unconfirmed tree, select the desired row in the table.
      If the information is correct click 'Verify' below and then click 'Save Changes.'"),
    uiOutput("verifyObservation")
  )
)

server <- function(input, output, session) {

  # this is a a reactiveValues object that gets updated as the user
  # add new rows
  theData <- reactiveValues()
  theData$dat <- dat
  theData$changeMade <- FALSE
  # this makes the tmap and returns it
  output$tmapMap <- renderTmap({
    dat2plot <- theData$dat
    # convert to sf for plotting
    dat2plot <- st_as_sf(dat2plot, coords = c("long","lat"),
                         crs=st_crs("EPSG:4326"))
    # get the data from the reactve

    map1 <- tm_shape(dat2plot) +
      tm_dots(col= "firebrick4", alpha=0.8,size = 0.05) +
      tm_legend(show = TRUE) +
      tm_view(set.zoom.limits = c(12,18))
    map1
  })


  # and here is the logic that will add rows to the data from the user
  observeEvent(input$addObservation,{
    to_add <- data.frame(lat = input$lat,
                         long = input$long,
                         species = input$species,
                         status = "Unconfirmed",
                         comment = input$comment
    )
    theData$dat <- rbind(theData$dat,to_add) # adding new data
    theData$changeMade <- TRUE
  })

  ########
  # Hey finn, here is what I did.
  # First I made a new reavtiveValue called `changeMade`
  # That value is initialized to FALSE but gets changed to TRUE if
  # addObservation is abserves as you have above. If changeMade gets set
  # to TRUE then the saveChanges widget gets rendered (right below) and if
  # that button is clicked the fruitTree.rds is overwritten.
  # the next thing you want to do is probably have a revert button of some
  # sort to undo the changes back to datOG.
  output$saveChanges <- renderUI({
    if(theData$changeMade == TRUE){
      actionButton(inputId = "saveChanges",
                   label = "Save Changes?")
    }
  })
  observeEvent(input$saveChanges,{
    saveRDS(theData$dat,"fruitTreeData.rds")
    theData$changeMade <- FALSE
  })
  #######


  # update numeric input with click
  observeEvent(input$tmapMap_click, {
    updateNumericInput(
      inputId = "long",
      value = input$tmapMap_click$lng
    )

    updateNumericInput(
      inputId = "lat",
      value = input$tmapMap_click$lat


    )

    # add marker to map where you clicked
    leafletProxy("tmapMap") %>%
    addMarkers(lat = input$tmapMap_click$lat,
    lng = input$tmapMap_click$lng, icon = marker , layerId = 'click' )

  })

  # confirm tree location
  selected <- reactive({input$updatedData_rows_selected})

  output$updatedData <- renderDataTable({
    tmp <- theData$dat[,1:4]
    tmp[,1:2] <- round(tmp[,1:2],4)
    tmp},
    selection='single')

  #  formatRound(c(1:2), 4)

  output$verifyObservation <- renderUI({
    row2highlight <- selected()
    if(!is.null(row2highlight)) {
      if(theData$dat[row2highlight, 4] == "Unconfirmed"){
        actionButton(inputId = "verifyObservation",
                     label = "Verify Tree Location ")
      }
    }
  })

  observeEvent(input$verifyObservation,{
    theData$dat[selected() , 4] <- "Confirmed"
    theData$changeMade <- TRUE
  })

  # highlighting observations
  observeEvent(input$updatedData_rows_selected, {

    if (!is.null(selected())) {
      selectedLat <- theData$dat[selected(), "lat"]
      selectedLong <- theData$dat[selected(), "long"]

      # Add a green marker to the selected tree's location
      leafletProxy("tmapMap") %>%
        addMarkers(lat = selectedLat, lng = selectedLong, icon = greencircle, layerId = 'HL' ) %>%
        clearImages()
    }
    # unconfimed highlighted by grey marker
    if(theData$dat[selected(), "status"]== "Unconfirmed"){

      # Add a grey marker to the selected tree's location
      leafletProxy("tmapMap") %>%
        addMarkers(lat = selectedLat, lng = selectedLong, icon = greycircle, layerId = 'HL' ) %>%
        clearImages()
    }
  })
}

shinyApp(ui = ui, server = server)

#List of stuff to add
# XX verification scheme for verifying tree location
# XXfix wierd error I get
# XX add tree by clicking location on map
# XX having selected values highlight on the map
# highlight with apple icon
# XXhilight unconfirmed trees different color
# XXsaving input data to new data set on a server
# nice HTMl & CSS design





