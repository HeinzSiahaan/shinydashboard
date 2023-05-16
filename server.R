shinyServer(function(input, output, session) {
  # Kode untuk infobox 1
  output$info_box1 <-renderInfoBox({
    
    # Data Wrangling
    games.sales.count.top.1 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_1) %>% 
      group_by(Name) %>%  
      summarise(sum.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>%
      #gather("key", "value", - c(Name, n)) %>%
      head(1) 
    
    infoBox(title="Top Games", value= games.sales.count.top.1$Name,icon = icon("gamepad"))
  })
  # Kode untuk infobox 2
  output$info_box2 <-renderInfoBox({
    
    # Data Wrangling
    games.sales.count.top.1 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_1) %>% 
      group_by(Publisher) %>%  
      summarise(sum.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>%
      #gather("key", "value", - c(Name, n)) %>%
      head(1) 
    
    top.publisher <- games.sales.clean  %>%
      filter(Publisher %in% games.sales.count.top.1$Publisher) %>%
      head(1)  
    
    infoBox(title="Top Publisher", value= top.publisher$Publisher,icon = icon("user-tie"))
  })
  # Kode untuk infobox 3
  output$info_box3 <-renderInfoBox({
    
    # Data Wrangling
    games.sales.count.top.1 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_1) %>% 
      group_by(Platform) %>%  
      summarise(sum.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>%
      #gather("key", "value", - c(Name, n)) %>%
      head(1) 
    
    top.platform <- games.sales.clean  %>%
      filter(Platform %in% games.sales.count.top.1$Platform) %>%
      head(1)  
    
    infoBox(title="Top Platform", value= top.platform$Platform,icon = icon("play-circle"))
  })
  
  # Kode untuk menampilkan dataset table
  output$dataset <- renderDataTable(
    games.sales.clean,
    options = list(scrollX = T,
                   scrollY = T)
  )
  
  # Kode untuk barplot (plot1)
  output$plot1_bar <- renderPlotly({
     
    
    # Data Wrangling
    games.sales.count.top.10 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_1) %>% 
      group_by(Name) %>%  
      summarise(sum.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>%
      #gather("key", "value", - c(Name, n)) %>%
      head(10) 
    
    games.sales.count.top.10.NA <- games.sales.clean %>%
      filter(Name %in% games.sales.count.top.10$Name)%>%
      group_by(Name) %>%  
      summarise(sum.Sales = sum(NA_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "NA Sales",
        label2 = glue("Name : {Name}
                      NA Sales : {sum.Sales} million copies")
      )                              
    
    games.sales.count.top.10.EU <- games.sales.clean %>%
      filter(Name %in% games.sales.count.top.10$Name)%>%
      group_by(Name) %>%  
      summarise(sum.Sales = sum(EU_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "EU Sales",
        label2 = glue("Name : {Name}
                      EU Sales : {sum.Sales} million copies")
      )  
    
    games.sales.count.top.10.JP <- games.sales.clean %>%
      filter(Name %in% games.sales.count.top.10$Name)%>%
      group_by(Name) %>%  
      summarise(sum.Sales = sum(JP_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "JP Sales",
        label2 = glue("Name : {Name}
                      JP Sales : {sum.Sales} million copies")
      ) 
    
    games.sales.count.top.10.Others <- games.sales.clean %>%
      filter(Name %in% games.sales.count.top.10$Name)%>%
      group_by(Name) %>%  
      summarise(sum.Sales = sum(Other_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "Other Sales",
        label2 = glue("Name : {Name}
                      Other Sales : {sum.Sales} million copies")
      ) 
    
    games.sales.count.top.10.All = rbind( games.sales.count.top.10.EU, games.sales.count.top.10.Others,games.sales.count.top.10.JP,games.sales.count.top.10.NA
                                        )
  
    
    #Data Visualization 
    games.sales.count.top.10.All$Region <- factor(games.sales.count.top.10.All$Region, levels = c("NA Sales","EU Sales","JP Sales","Other Sales"))
    plot1_bar <- ggplot(data = games.sales.count.top.10.All , mapping = aes(y = reorder(Name,
                                                                                        sum.Sales), 
                                                                            x =  sum.Sales, text=label2)) + 
      geom_col(mapping = aes(fill = Region),position = position_stack(reverse = TRUE)) + 
      scale_fill_manual(values = c( "#82AAE3","#91D8E4","#EAFDFC", "#BFEAF5"))+
      theme_minimal()+
      theme(legend.position = "top",legend.title = element_blank())+
      labs( 
           x = "Total Sales (in million copies)",
           y = NULL) 
    
  
    ggplotly(plot1_bar, tooltip = "text") %>%
      layout(legend=list(orientation = "h",  x = 0.1, y = 1.1 ))
    #%>% layout(plot1_bar, hovermode = 'x unified', hoverlabel=list(bgcolor='rgba(255,255,255,0.75)', font=list(color='black')))
    
  })
  
  output$plot13_bar <- renderPlotly({
    
    
    # Data Wrangling
    games.sales.count.top.10 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_1) %>% 
      group_by(Platform) %>%  
      summarise(sum.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>%
      #gather("key", "value", - c(Name, n)) %>%
      head(10) 
    
    games.sales.count.top.10.NA <- games.sales.clean %>%
      filter(Platform %in% games.sales.count.top.10$Platform)%>%
      group_by(Platform) %>%  
      summarise(sum.Sales = sum(NA_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "NA Sales",
        label2 = glue("Platform : {Platform}
                      NA Sales : {sum.Sales} million copies")
      )                              
    
    games.sales.count.top.10.EU <- games.sales.clean %>%
      filter(Platform %in% games.sales.count.top.10$Platform)%>%
      group_by(Platform) %>%  
      summarise(sum.Sales = sum(EU_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "EU Sales",
        label2 = glue("Platform : {Platform}
                      EU Sales : {sum.Sales} million copies")
      )  
    
    games.sales.count.top.10.JP <- games.sales.clean %>%
      filter(Platform %in% games.sales.count.top.10$Platform)%>%
      group_by(Platform) %>%  
      summarise(sum.Sales = sum(JP_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "JP Sales",
        label2 = glue("Platform : {Platform}
                      JP Sales : {sum.Sales} million copies")
      ) 
    
    games.sales.count.top.10.Others <- games.sales.clean %>%
      filter(Platform %in% games.sales.count.top.10$Platform)%>%
      group_by(Platform) %>%  
      summarise(sum.Sales = sum(Other_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "Other Sales",
        label2 = glue("Platform : {Platform}
                      Other Sales : {sum.Sales} million copies")
      ) 
    
    games.sales.count.top.10.All = rbind( games.sales.count.top.10.EU, games.sales.count.top.10.Others,games.sales.count.top.10.JP,games.sales.count.top.10.NA
    )
    
    
    #Data Visualization 
    games.sales.count.top.10.All$Region <- factor(games.sales.count.top.10.All$Region, levels = c("NA Sales","EU Sales","JP Sales","Other Sales"))
    plot1_bar <- ggplot(data = games.sales.count.top.10.All , mapping = aes(y = reorder(Platform,
                                                                                        sum.Sales), 
                                                                            x =  sum.Sales, text=label2)) + 
      geom_col(mapping = aes(fill = Region),position = position_stack(reverse = TRUE)) + 
      scale_fill_manual(values = c( "#82AAE3","#91D8E4","#EAFDFC", "#BFEAF5"))+
      scale_x_continuous(labels = comma) +
      theme_minimal()+
      theme(legend.position = "top",legend.title = element_blank())+
      labs( 
        x = "Total Sales (in million copies)",
        y = NULL) 
    
    
    ggplotly(plot1_bar, tooltip = "text") %>%
      layout(showlegend=F)
    #%>% layout(plot1_bar, hovermode = 'x unified', hoverlabel=list(bgcolor='rgba(255,255,255,0.75)', font=list(color='black')))
    
  })
  output$plot14_bar <- renderPlotly({
    
    
    # Data Wrangling
    games.sales.count.top.10 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_1) %>% 
      group_by(Publisher) %>%  
      summarise(sum.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>%
      #gather("key", "value", - c(Name, n)) %>%
      head(10) 
    
    games.sales.count.top.10.NA <- games.sales.clean %>%
      filter(Publisher %in% games.sales.count.top.10$Publisher)%>%
      group_by(Publisher) %>%  
      summarise(sum.Sales = sum(NA_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "NA Sales",
        label2 = glue("Publisher : {Publisher}
                      NA Sales : {sum.Sales} million copies")
      )                              
    
    games.sales.count.top.10.EU <- games.sales.clean %>%
      filter(Publisher %in% games.sales.count.top.10$Publisher)%>%
      group_by(Publisher) %>%  
      summarise(sum.Sales = sum(EU_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "EU Sales",
        label2 = glue("Publisher : {Publisher}
                      EU Sales : {sum.Sales} million copies")
      )  
    
    games.sales.count.top.10.JP <- games.sales.clean %>%
      filter(Publisher %in% games.sales.count.top.10$Publisher)%>%
      group_by(Publisher) %>%  
      summarise(sum.Sales = sum(JP_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "JP Sales",
        label2 = glue("Publisher : {Publisher}
                      JP Sales : {sum.Sales} million copies")
      ) 
    
    games.sales.count.top.10.Others <- games.sales.clean %>%
      filter(Publisher %in% games.sales.count.top.10$Publisher)%>%
      group_by(Publisher) %>%  
      summarise(sum.Sales = sum(Other_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Region = "Other Sales",
        label2 = glue("Publisher : {Publisher}
                      Other Sales : {sum.Sales} million copies")
      ) 
    
    games.sales.count.top.10.All = rbind( games.sales.count.top.10.EU, games.sales.count.top.10.Others,games.sales.count.top.10.JP,games.sales.count.top.10.NA
    )
    
    
    #Data Visualization 
    games.sales.count.top.10.All$Region <- factor(games.sales.count.top.10.All$Region, levels = c("NA Sales","EU Sales","JP Sales","Other Sales"))
    plot1_bar <- ggplot(data = games.sales.count.top.10.All , mapping = aes(y = reorder(Publisher,
                                                                                        sum.Sales), 
                                                                            x =  sum.Sales, text=label2)) + 
      geom_col(mapping = aes(fill = Region),position = position_stack(reverse = TRUE)) + 
      scale_fill_manual(values = c( "#82AAE3","#91D8E4","#EAFDFC", "#BFEAF5"))+
      scale_x_continuous(labels = comma) +
      theme_minimal()+
      theme(legend.position = "None",legend.title = element_blank())+
      labs( 
        x = "Total Sales (in million copies)",
        y = NULL) 
    
    
    ggplotly(plot1_bar, tooltip = "text") %>%
      layout(showlegend=F)
    #%>% layout(plot1_bar, hovermode = 'x unified', hoverlabel=list(bgcolor='rgba(255,255,255,0.75)', font=list(color='black')))
    
  })
  
  output$plot2_pie <- renderPlotly({ 
    # Data Wrangling
    games.sales.count.NA <- games.sales.clean %>%  
     filter(Genre %in% input$input_genre_1)%>%
      group_by() %>%  
      summarise(sum.Sales = sum(NA_Sales)) %>% 
      ungroup()   %>%
      mutate(
        Country = "NA Sales",
        label2 = glue("{sum.Sales}")
      ) 
    games.sales.count.EU <- games.sales.clean %>%  
      filter(Genre %in% input$input_genre_1)%>%
      group_by() %>%  
      summarise(sum.Sales = sum(EU_Sales)) %>% 
      ungroup()   %>%
      mutate(
        Country = "EU Sales",
        label2 = glue("{sum.Sales}")
      ) 
    games.sales.count.JP <- games.sales.clean %>%  
      filter(Genre %in% input$input_genre_1)%>%
      group_by() %>%  
      summarise(sum.Sales = sum(JP_Sales)) %>% 
      ungroup()   %>%
      mutate(
        Country = "JP Sales",
        label2 = glue("{sum.Sales}")
      ) 
    games.sales.count.Other <- games.sales.clean %>%  
      filter(Genre %in% input$input_genre_1)%>%
      group_by() %>%  
      summarise(sum.Sales = sum(Other_Sales)) %>% 
      ungroup()   %>%
      mutate(
        Country = "Other Sales",
        label2 = glue("{sum.Sales}")
      ) 
    games.sales.count.All = rbind(games.sales.count.NA, games.sales.count.EU,games.sales.count.JP,games.sales.count.Other)
    
    # Visualization
    plot2 <- plot_ly(games.sales.count.All, labels = ~Country, values = ~sum.Sales, type = 'pie')%>%
      layout(title = 'Proportion of Games Sales')
    
    ggplotly(plot2, tooltip = "text")
    
  })
  
  output$plot3_bar <- renderPlotly({
    
    # Data Wrangling
    games.sales.count.top.10 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_1) %>% 
      group_by(Genre) %>%  
      summarise(sum.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>% 
      head(10)
    
    games.sales.count.top.10<- games.sales.count.top.10 %>% 
      mutate(
        label = glue( "Genre : {Genre}
      Total Sales: {sum.Global} million copies"
        )
      )
    
    # Visualization
    plot3 <- ggplot(data = games.sales.count.top.10, aes(x = sum.Global, 
                                           y = reorder(Genre, sum.Global),
                                           text = label)) + 
      geom_col(aes(fill = sum.Global)) + 
      labs(title = "",
           x = "Total Sales (in million copies)",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot3, tooltip = "text")
    
  })
  
  output$plot4_area  <- renderPlotly({
    
    # Data Wrangling
    year.sales <- games.sales.clean %>%  
      group_by(Year_of_Release) %>%  
      summarise(sum.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      arrange(-Year_of_Release)  
    
    year.sales<- year.sales %>% 
      mutate(
        label = glue( "Year : {Year_of_Release}
      Total Sales: {sum.Global} million copies"
        )
      )
    
    # Visualization
    plot4 <- ggplot(data = year.sales, aes(x = Year_of_Release, 
                                                         y = sum.Global, 
                                                         text = label)) + 
      geom_area() + 
      labs(title = "",
           x =  NULL,
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot4, tooltip = "text")
    
  })
  
  output$plot5_line  <- renderPlotly({
    games.sales.count.NA <- games.sales.clean %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "NA Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.NA} million copies")
      )                              
    
    games.sales.count.EU <- games.sales.clean %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "EU Sales",
        label2 =   glue("{Year_of_Release} : {sum.Sales.EU} million copies")
      )  
    
    games.sales.count.JP <- games.sales.clean %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "JP Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.JP} million copies")
      ) 
    
    games.sales.count.Others <- games.sales.clean %>% 
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "Other Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.Other} million copies")
      ) 
    
    games.sales.count.Global <- games.sales.clean %>% 
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "Global Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.Global} million copies")
      ) 
    
    games.sales.count.All = rbind( games.sales.count.EU, games.sales.count.Others,games.sales.count.JP,games.sales.count.NA,games.sales.count.Global)
    games.sales.count.All %>% 
      mutate(
        Country=as.factor(Country)
      )
    
    
    
    plot5<-  ggplot(games.sales.count.All,mapping = aes(x=Year_of_Release))+ 
      geom_line(data=games.sales.count.NA,mapping=aes(y=sum.Sales.NA, color="NA Sales"))+
      geom_point(data=games.sales.count.NA,mapping=aes(y=sum.Sales.NA, color="NA Sales", text=label2))+ 
      geom_line(data=games.sales.count.EU,mapping=aes(y=sum.Sales.EU, color="EU Sales"))+
      geom_point(data=games.sales.count.EU,mapping=aes(y=sum.Sales.EU, color="EU Sales", text=label2))+ 
      geom_line(data=games.sales.count.JP,mapping=aes(y=sum.Sales.JP, color="JP Sales"))+
      geom_point(data=games.sales.count.JP,mapping=aes(y=sum.Sales.JP, color="JP Sales", text=label2))+ 
      geom_line(data=games.sales.count.Others,mapping=aes(y=sum.Sales.Other, color="Other Sales"))+
      geom_point(data=games.sales.count.Others,mapping=aes(y=sum.Sales.Other, color="Other Sales", text=label2))+ 
      geom_line(data=games.sales.count.Global,mapping=aes(y=sum.Sales.Global, color="Global Sales"))+
      geom_point(data=games.sales.count.Global,mapping=aes(y=sum.Sales.Global, color="Global Sales", text=label2))+
      labs(title = "Sales Trend",
           x = "Year",
           y = "", 
           caption = "Source : kaggle.com") + 
      theme(legend.position = "bottom")+
      theme_minimal()+
      theme(legend.position = "top", plot.title = element_text(hjust = 0.5))
    
    ggplotly(plot5, tooltip = "text")
    
  })
  
  output$plot6_line  <- renderPlotly({
    games.sales.clean.genre <- games.sales.clean %>%
      filter(Genre == input$input_genre2)  
      
    games.sales.count.NA <- games.sales.clean.genre %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "NA Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.NA} million copies")
      )                              
    
    games.sales.count.EU <- games.sales.clean.genre %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "EU Sales",
        label2 =   glue("{Year_of_Release} : {sum.Sales.EU} million copies")
      )  
    
    games.sales.count.JP <- games.sales.clean.genre %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "JP Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.JP} million copies")
      ) 
    
    games.sales.count.Others <- games.sales.clean.genre %>% 
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "Other Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.Other} million copies")
      ) 
    
    games.sales.count.Global <- games.sales.clean.genre %>% 
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "Global Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.Global} million copies")
      ) 
    
    games.sales.count.All = rbind( games.sales.count.EU, games.sales.count.Others,games.sales.count.JP,games.sales.count.NA,games.sales.count.Global)
    games.sales.count.All %>% 
      mutate(
        Country=as.factor(Country)
      )
    
    
    
    plot6<-  ggplot(games.sales.count.All,mapping = aes(x=Year_of_Release))+ 
      geom_line(data=games.sales.count.NA,mapping=aes(y=sum.Sales.NA, color="NA Sales"))+
      geom_point(data=games.sales.count.NA,mapping=aes(y=sum.Sales.NA, color="NA Sales", text=label2))+ 
      geom_line(data=games.sales.count.EU,mapping=aes(y=sum.Sales.EU, color="EU Sales"))+
      geom_point(data=games.sales.count.EU,mapping=aes(y=sum.Sales.EU, color="EU Sales", text=label2))+ 
      geom_line(data=games.sales.count.JP,mapping=aes(y=sum.Sales.JP, color="JP Sales"))+
      geom_point(data=games.sales.count.JP,mapping=aes(y=sum.Sales.JP, color="JP Sales", text=label2))+ 
      geom_line(data=games.sales.count.Others,mapping=aes(y=sum.Sales.Other, color="Other Sales"))+
      geom_point(data=games.sales.count.Others,mapping=aes(y=sum.Sales.Other, color="Other Sales", text=label2))+ 
      geom_line(data=games.sales.count.Global,mapping=aes(y=sum.Sales.Global, color="Global Sales"))+
      geom_point(data=games.sales.count.Global,mapping=aes(y=sum.Sales.Global, color="Global Sales", text=label2))+
      theme_minimal()+
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
      labs(title = "Sales Trend per genre",
           x = "",
           y = "" ) 
    
    ggplotly(plot6, tooltip = "text")
    
  })
  
  output$plot7_line  <- renderPlotly({
    games.sales.clean.platform <- games.sales.clean %>%
      filter(Platform == input$input_platform2)  
    
    games.sales.count.NA <- games.sales.clean.platform %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "NA Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.NA} million copies")
      )                              
    
    games.sales.count.EU <- games.sales.clean.platform %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "EU Sales",
        label2 =   glue("{Year_of_Release} : {sum.Sales.EU} million copies")
      )  
    
    games.sales.count.JP <- games.sales.clean.platform %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "JP Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.JP} million copies")
      ) 
    
    games.sales.count.Others <- games.sales.clean.platform %>% 
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "Other Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.Other} million copies")
      ) 
    
    games.sales.count.Global <- games.sales.clean.platform %>% 
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "Global Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.Global} million copies")
      ) 
    
    games.sales.count.All = rbind( games.sales.count.EU, games.sales.count.Others,games.sales.count.JP,games.sales.count.NA,games.sales.count.Global)
    games.sales.count.All %>% 
      mutate(
        Country=as.factor(Country)
      )
    
    
    
    plot7<-  ggplot(games.sales.count.All,mapping = aes(x=Year_of_Release))+ 
      geom_line(data=games.sales.count.NA,mapping=aes(y=sum.Sales.NA, color="NA Sales"))+
      geom_point(data=games.sales.count.NA,mapping=aes(y=sum.Sales.NA, color="NA Sales", text=label2))+ 
      geom_line(data=games.sales.count.EU,mapping=aes(y=sum.Sales.EU, color="EU Sales"))+
      geom_point(data=games.sales.count.EU,mapping=aes(y=sum.Sales.EU, color="EU Sales", text=label2))+ 
      geom_line(data=games.sales.count.JP,mapping=aes(y=sum.Sales.JP, color="JP Sales"))+
      geom_point(data=games.sales.count.JP,mapping=aes(y=sum.Sales.JP, color="JP Sales", text=label2))+ 
      geom_line(data=games.sales.count.Others,mapping=aes(y=sum.Sales.Other, color="Other Sales"))+
      geom_point(data=games.sales.count.Others,mapping=aes(y=sum.Sales.Other, color="Other Sales", text=label2))+ 
      geom_line(data=games.sales.count.Global,mapping=aes(y=sum.Sales.Global, color="Global Sales"))+
      geom_point(data=games.sales.count.Global,mapping=aes(y=sum.Sales.Global, color="Global Sales", text=label2))+
      theme_minimal()+
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
      labs(title = "Sales Trend per platform",
           x = "",
           y = "" ) 
    
    ggplotly(plot7, tooltip = "text")
    
  })
  
  output$plot8_line  <- renderPlotly({
    games.sales.clean.publisher <- games.sales.clean %>%
      filter(Publisher == input$input_publisher2)  
    
    games.sales.count.NA <- games.sales.clean.publisher %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "NA Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.NA} million copies")
      )                              
    
    games.sales.count.EU <- games.sales.clean.publisher %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "EU Sales",
        label2 =   glue("{Year_of_Release} : {sum.Sales.EU} million copies")
      )  
    
    games.sales.count.JP <- games.sales.clean.publisher %>%
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "JP Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.JP} million copies")
      ) 
    
    games.sales.count.Others <- games.sales.clean.publisher %>% 
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "Other Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.Other} million copies")
      ) 
    
    games.sales.count.Global <- games.sales.clean.publisher %>% 
      group_by(Year_of_Release) %>%  
      summarise(sum.Sales.NA = sum(NA_Sales),sum.Sales.EU = sum(EU_Sales),sum.Sales.JP = sum(JP_Sales),sum.Sales.Other = sum(Other_Sales),sum.Sales.Global = sum(Global_Sales)) %>% 
      ungroup() %>% 
      mutate(
        Country = "Global Sales",
        label2 = glue("{Year_of_Release} : {sum.Sales.Global} million copies")
      ) 
    
    games.sales.count.All = rbind( games.sales.count.EU, games.sales.count.Others,games.sales.count.JP,games.sales.count.NA,games.sales.count.Global)
    games.sales.count.All %>% 
      mutate(
        Country=as.factor(Country)
      )
    
    
    
    plot8<-  ggplot(games.sales.count.All,mapping = aes(x=Year_of_Release))+ 
      geom_line(data=games.sales.count.NA,mapping=aes(y=sum.Sales.NA, color="NA Sales"))+
      geom_point(data=games.sales.count.NA,mapping=aes(y=sum.Sales.NA, color="NA Sales", text=label2))+ 
      geom_line(data=games.sales.count.EU,mapping=aes(y=sum.Sales.EU, color="EU Sales"))+
      geom_point(data=games.sales.count.EU,mapping=aes(y=sum.Sales.EU, color="EU Sales", text=label2))+ 
      geom_line(data=games.sales.count.JP,mapping=aes(y=sum.Sales.JP, color="JP Sales"))+
      geom_point(data=games.sales.count.JP,mapping=aes(y=sum.Sales.JP, color="JP Sales", text=label2))+ 
      geom_line(data=games.sales.count.Others,mapping=aes(y=sum.Sales.Other, color="Other Sales"))+
      geom_point(data=games.sales.count.Others,mapping=aes(y=sum.Sales.Other, color="Other Sales", text=label2))+ 
      geom_line(data=games.sales.count.Global,mapping=aes(y=sum.Sales.Global, color="Global Sales"))+
      geom_point(data=games.sales.count.Global,mapping=aes(y=sum.Sales.Global, color="Global Sales", text=label2))+
      theme_minimal()+
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
      labs(title = "Sales Trend per publisher",
           x = "",
           y = "" )  
       
    
    ggplotly(plot8, tooltip = "text")
    
  })
  
  output$plot9_bar <- renderPlotly({
    
    # Data Wrangling
    games.sales.count.top.10 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_3,
             Platform %in% input$input_platform_3,
             Publisher %in% input$input_publisher_3,
             Year_of_Release %in% input$input_year_3) %>% 
      group_by(Name) %>%  
      summarise(sum.Global = sum(
        ifelse(input$input_sales_country_3=="Global Sales",Global_Sales,
               ifelse(input$input_sales_country_3=="NA Sales",NA_Sales,
                      ifelse(input$input_sales_country_3=="EU Sales",EU_Sales,
                             ifelse(input$input_sales_country_3=="JP Sales",JP_Sales,
                                    ifelse(input$input_sales_country_3=="Other Sales",Other_Sales,
        Global_Sales)))))
        )) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>% 
      head(input$input_topn_3)
    
    games.sales.count.top.10<- games.sales.count.top.10 %>% 
      mutate(
        label = glue( "Name : {Name}
      Total Sales: {sum.Global} million copies"
        )
      )
    
    # Visualization
    plot9 <- ggplot(data = games.sales.count.top.10, aes(x = sum.Global, 
                                                         y = reorder(Name, sum.Global),
                                                         text = label)) + 
      geom_col(aes(fill = sum.Global)) + 
      labs(title = glue("Top {input$input_topn_3} Selected Video Games"),
           x = "Total Sales (in million copies)",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot9, tooltip = "text")
    
  })
  output$plot10_bar <- renderPlotly({
    
    # Data Wrangling
    games.sales.count.top.10 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_3,
             Platform %in% input$input_platform_3,
             Publisher %in% input$input_publisher_3,
             Year_of_Release %in% input$input_year_3) %>% 
      group_by(Genre) %>%  
      summarise(sum.Global = sum(
        ifelse(input$input_sales_country_3=="Global Sales",Global_Sales,
               ifelse(input$input_sales_country_3=="NA Sales",NA_Sales,
                      ifelse(input$input_sales_country_3=="EU Sales",EU_Sales,
                             ifelse(input$input_sales_country_3=="JP Sales",JP_Sales,
                                    ifelse(input$input_sales_country_3=="Other Sales",Other_Sales,
                                           Global_Sales)))))
      )) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>% 
      head(input$input_topn_3)
    
    games.sales.count.top.10<- games.sales.count.top.10 %>% 
      mutate(
        label = glue( "Genre : {Genre}
      Total Sales: {sum.Global} million copies"
        )
      )
    
    # Visualization
    plot10 <- ggplot(data = games.sales.count.top.10, aes(x = sum.Global, 
                                                         y = reorder(Genre, sum.Global),
                                                         text = label)) + 
      geom_col(aes(fill = sum.Global)) + 
      labs(title = glue("Top {input$input_topn_3} Selected Genre"),
           x = "Total Sales (in million copies)",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot10, tooltip = "text")
    
  })
  
  output$plot11_bar <- renderPlotly({
    
    # Data Wrangling
    games.sales.count.top.10 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_3,
             Platform %in% input$input_platform_3,
             Publisher %in% input$input_publisher_3,
             Year_of_Release %in% input$input_year_3) %>% 
      group_by(Platform) %>%  
      summarise(sum.Global = sum(
        ifelse(input$input_sales_country_3=="Global Sales",Global_Sales,
               ifelse(input$input_sales_country_3=="NA Sales",NA_Sales,
                      ifelse(input$input_sales_country_3=="EU Sales",EU_Sales,
                             ifelse(input$input_sales_country_3=="JP Sales",JP_Sales,
                                    ifelse(input$input_sales_country_3=="Other Sales",Other_Sales,
                                           Global_Sales)))))
      )) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>% 
      head(input$input_topn_3)
    
    games.sales.count.top.10<- games.sales.count.top.10 %>% 
      mutate(
        label = glue( "Platform : {Platform}
      Total Sales: {sum.Global} million copies"
        )
      )
    
    # Visualization
    plot11 <- ggplot(data = games.sales.count.top.10, aes(x = sum.Global, 
                                                          y = reorder(Platform, sum.Global),
                                                          text = label)) + 
      geom_col(aes(fill = sum.Global)) + 
      labs(title = glue("Top {input$input_topn_3} Selected Platform"),
           x = "Total Sales (in million copies)",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot11, tooltip = "text")
    
  })
  
  output$plot12_bar <- renderPlotly({
    
    # Data Wrangling
    games.sales.count.top.10 <- games.sales.clean %>% 
      filter(Genre %in% input$input_genre_3,
             Platform %in% input$input_platform_3,
             Publisher %in% input$input_publisher_3,
             Year_of_Release %in% input$input_year_3) %>% 
      group_by(Publisher) %>%  
      summarise(sum.Global = sum(
        ifelse(input$input_sales_country_3=="Global Sales",Global_Sales,
               ifelse(input$input_sales_country_3=="NA Sales",NA_Sales,
                      ifelse(input$input_sales_country_3=="EU Sales",EU_Sales,
                             ifelse(input$input_sales_country_3=="JP Sales",JP_Sales,
                                    ifelse(input$input_sales_country_3=="Other Sales",Other_Sales,
                                           Global_Sales)))))
      )) %>% 
      ungroup() %>% 
      arrange(-sum.Global) %>% 
      head(input$input_topn_3)
    
    games.sales.count.top.10<- games.sales.count.top.10 %>% 
      mutate(
        label = glue( "Publisher : {Publisher}
      Total Sales: {sum.Global} million copies"
        )
      )
    
    # Visualization
    plot12 <- ggplot(data = games.sales.count.top.10, aes(x = sum.Global, 
                                                          y = reorder(Publisher, sum.Global),
                                                          text = label)) + 
      geom_col(aes(fill = sum.Global)) + 
      labs(title = glue("Top {input$input_topn_3} Selected Publisher"),
           x = "Total Sales (in million copies)",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot12, tooltip = "text")
    
  })
})
