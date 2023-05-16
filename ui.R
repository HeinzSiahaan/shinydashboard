dashboardPage(
  #--------- Bagian Header ---------
  dashboardHeader(
    title = "Video Games Sales"
  ),
  
  #--------- Bagian Sidebar ---------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Chart", tabName = "page1",icon=icon("star")),
      menuItem("Trend Analysis", tabName = "page2",icon=icon("chart-line")),
      menuItem("Chart Analysis", tabName = "page3",icon=icon("chart-bar")) ,
      menuItem("Dataset", tabName = "page4",icon=icon("server")) 
    )
  ),
  
  #--------- Bagian Main / Body ---------
  dashboardBody(
    #tags$head(
    #  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    #),
    
    tabItems(
      #--------- Page 1 ---------
      tabItem(
        tabName = "page1",
        #---Page 1 Row 1
        fluidRow( 
            infoBoxOutput("info_box1"),
            infoBoxOutput("info_box2"),
            infoBoxOutput("info_box3")
        ),
        #---Page 1 Row 2
        fluidRow(
          box(
            width=3,
            height = 450,
            checkboxGroupInput(inputId = "input_genre_1",
            choices =  c(unique(sort(games.sales.clean$Genre))),
            label = "Select Genres", 
            selected = c(unique(sort(games.sales.clean$Genre)))
          )),
          box(
            width=9,
            height = 450, 
            plotlyOutput("plot1_bar")
          ) 
      ),
      #---Page 1 Row 3
      fluidRow(
        box(
          width=6,
          plotlyOutput("plot14_bar")
        ) ,
        box(
          width=6,
          plotlyOutput("plot13_bar")
        ) 
      )
      
      ),
      
       
      #--------- Page 2 ---------
      tabItem(
        tabName = "page2",
         
        #---Page 2 Row 1 ---------
        fluidRow(
          box(
            width=12,
            plotlyOutput("plot5_line")
          ) 
        ),
        fluidRow(
          box(
            width=4,
            selectInput(
              inputId = "input_genre2",
              label = "Choose games genre",
              choices = unique(sort(games.sales.clean$Genre)),
              selected = "Sports"
            )
          ),
          box(
            width=4,
            selectInput(
              inputId = "input_platform2",
              label = "Choose games platform",
              choices = unique(sort(games.sales.clean$Platform)),
              selected = "Wii"
            )
          ),
          box(
            width=4,
            selectInput(
              inputId = "input_publisher2",
              label = "Choose games publisher",
              choices = unique(sort(games.sales.clean$Publisher)),
              selected = "Nintendo"
            )
          )
        ),
        fluidRow(
          box(
            width=4,
            plotlyOutput("plot6_line")
          ),
          box(
            width=4,
            plotlyOutput("plot7_line")
          ) ,
          box(
            width=4,
            plotlyOutput("plot8_line")
          ) 
        )
      ),
      #--------- Page 3 ---------
      tabItem(
        tabName = "page3",
        fluidRow(
          box(
            width=4,height=125,
            sliderInput(
              inputId = "input_topn_3",
              label = "Top-N",
              min = 1,
              max=20,
              value=10
            )
        ),
        box(
          width=4,height=125,
          pickerInput(
            inputId = "input_genre_3",
            label = "Choose genre",
            choices = unique(sort(games.sales.clean.option$Genre)) ,
            selected = unique(sort(games.sales.clean.option$Genre)) ,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          )
        ) ,
        box(
          width=4,height=125,
          pickerInput(
            inputId = "input_platform_3",
            label = "Choose platform",
            choices = unique(sort(games.sales.clean.option$Platform)) ,
            selected = unique(sort(games.sales.clean.option$Platform)) ,
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          )
        )),
        #---Page 3 Row 2
        fluidRow(
        box(
          width=4,height=125,
          pickerInput(
            inputId = "input_publisher_3",
            label = "Choose publisher",
            choices = unique(sort(games.sales.clean.option$Publisher)),
            selected = unique(sort(games.sales.clean.option$Publisher)),
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          )
        ),
        box(
          width=4,height=125,
          pickerInput(
            inputId = "input_year_3",
            label = "Choose year",
            choices = unique(sort(games.sales.clean.option$Year_of_Release)),
            selected = unique(sort(games.sales.clean.option$Year_of_Release)),
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          )
        ),
        box(
          width=4,height=125,
          selectInput(
            inputId = "input_sales_country_3",
            label = "Choose region",
            choices = c("Global Sales", "NA Sales","EU Sales","JP Sales","Other Sales") ,
            selected ="Global Sales"
          )
        )
      ),
      #--Page 3 Row 3--
      fluidRow(
        box(
          width=6,
          plotlyOutput("plot9_bar")
        ),
        box(
          width=6,
          plotlyOutput("plot10_bar")
        )
      ),
      fluidRow(
        box(
          width=6,
          plotlyOutput("plot11_bar")
        ),
        box(
          width=6,
          plotlyOutput("plot12_bar")
        )
      )
      ),
      #--------- Page 4 ---------
      tabItem(
        tabName = "page4",
        fluidRow(
          box(
            width=12, 
            h1("Data Visualization Capstone Project"),
            h5("by Heinz Metrosan Donradt. S"), 
            br(),
            h4("This dataset consists of records from Metacritic providing insight into global video game ratings and sales."),
            #h4(a(href="https://www.kaggle.com/datasets/thedevastator/global-video-game-sales-ratings", "You can download dataset on this link"))
            h4("You can download dataset on this link: "),
            h4("https://www.kaggle.com/datasets/thedevastator/global-video-game-sales-ratings"),
          )
        ),
        fluidRow(
          box(
            width=12,
            title = "Data Global Video Game Sales per February 2023",
            dataTableOutput(outputId= "dataset")
          )
        )
      )
      
      
    )
  )
)