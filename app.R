library(reticulate)
library(shiny)
library(argonR)
library(argonDash)
library(gdata)
library(corrplot)
library(summarytools)
library(rgdal)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(reshape2)
library(stringr)
library(ggplot2)

##########
# Andmete laadimine
##########
andmed = read.csv("./data/top_200_youtubers.csv", header=T, sep=",")

##########
# Andmete puhastamine
##########
andmed <- andmed %>% distinct(Country,Channel.Name, .keep_all = TRUE)
andmed <- andmed[!(is.na(andmed$Country) | andmed$Country == ""), ]
andmed$Country <- str_remove_all(andmed$Country, "[\r\n]")
andmed$followers <- round(andmed$followers / 1000000, digits = 2)
andmed$Likes <- round(andmed$Likes / 1000000, digits = 2)
andmed$Views <- round(andmed$Views / 1000000, digits = 2)
andmed <- rename.vars(andmed, from = "followers", to = "followers_M")
andmed <- rename.vars(andmed, from = "Likes", to = "Likes_M")
andmed <- rename.vars(andmed, from = "Views", to = "Views_M")

##########
# Kaardi laadimine ja ettevalmistamine
##########'

world_spdf <- readOGR( 
  dsn = paste0(getwd(),"/data/"), 
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = FALSE
)

world_spdf@data$POP2005[which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2) # rahvaarv miljonites
world_spdf@data$CHANNELS <- 0
world_spdf@data$CHANNELS_POP <- 0
world_spdf@data$FOLLOWERS <- 0
world_spdf@data$MAINTOPIC <- "N/A"

for (row in 1:nrow(andmed)) {
  country <- andmed[row, "Country"]
  world_row = world_spdf@data[world_spdf@data$ISO2 == country,]
  world_spdf@data$CHANNELS[which(world_spdf@data$ISO2 == country)] = world_row$CHANNELS + 1
  world_spdf@data$FOLLOWERS[which(world_spdf@data$ISO2 == country)] = world_row$FOLLOWERS + andmed[row, "followers_M"]
}

##########
# Päis
##########
header <- argonDashHeader(
  argonH1("Top YouTube kanalite analüüs", display = 2),
  "Õppeaine: Andmete visualiseerimine (Virumaa) - ITB8812",
  br(),
  "Õppejõud: Olga Dunajeva",
  br(),
  "Autorid: Airika Andruse ja Elmo Egers"
)

##########
# Tab-id
##########

# Tutvustus
tutvustusTab <- argonTab(
  tabName = "Tutvustus",
  active = TRUE,
  argonRow(
    argonColumn(
      width = 4,
      argonImage(src = "dataset-cover.jpg", card_mode = TRUE) 
    ),
    argonColumn(
      width = 8,
      argonH1("Tutvustus", display = 4),
      "Projekti andmestikuks on valitud Youtube populaarsemate kanalite andmestik. Andmestik on allalaaditud Kaggle.com veebilehe andmestike andmebaasist.",
      br(),
      "Andmestik on .csv formaadis.",
      br(),
      a(
        "https://www.kaggle.com/datasets/syedjaferk/top-200-youtubers-cleaned",
        style = "color:blue",
        href = "https://www.kaggle.com/datasets/syedjaferk/top-200-youtubers-cleaned"
      ),
      br(), br(),
      "Lisaks YouTube statistilistele andmetele on kasutusel maailmapiiride andmestik, mis on kasutusel üksnes Riikide piiride kuvamiseks ja statistika arvutamiseks.",
      br(),
      a(
        "https://thematicmapping.org/downloads/world_borders.php",
        style = "color:blue",
        href = "https://thematicmapping.org/downloads/world_borders.php"
      ),
      br(), br(),
      strong("Eesmärgid / uurimisküsimused:"), br(), br(),
      "1. Millistes riikides on kõige rohkem populaarseid YouTubereid ning milline on riikide üldine statistika?", br(),
      "2. Millised on kõige populaarsemad kanalite kategooriad?", br(),
      "3. Millised on kõige populaarsemad kanalite teemad?", br(),
      "4. Millistel kanalitel on kõige aktiivsemad jälgijad?", br()
    )
  )
)

# Tunnused
tunnusedTab <- argonTab(
  tabName = "Tunnused",
  argonH1("YouTube andmete tunnused: ", display = 4),
  tags$ul(
    tags$li("Country - Riik (riigikood) ('IN', 'US', 'KR', 'CA', 'BR', 'MX', 'SV', 'CL', 'NO', 'PR', 'BY', 'RU', 'PH', 'TH', 'AE', 'CO', 'ES', 'GB', 'AR', 'ID', 'NL','ES', 'IE', 'PK', 'AU', 'KW', 'SO')"),
    tags$li("Channel Name	- Kanali nimi "),
    tags$li("Category	- Kategooria ('Gaming & Apps','Sports', 'Music', 'Beauty & Fashion', 'Science & Tech', 'Fashion', 'LifeStyle')"),
    tags$li("Main Video Category	- Peamine video kategooria ('Music', 'Education', 'Shows', 'Gaming', 'Entertainment', 'People & Blogs', 'Sports', 'Howto & Style', 'Film & Animation', 'News & Politics', 'Pop music', 'Comedy', 'Nonprofits & Activism', 'Action-adventure game', 'Strategy video game', 'TV shows')"),
    tags$li("Username	- Kanali kasutajanimi"),
    tags$li("Followers	- Jälgijate arv"),
    tags$li("Main topic	-Põhiteema, mida on kõige rohkem arutatud"),
    tags$li("More Topics - Muud teemad, lisaks põhiteemale"),
    tags$li("Likes	- Meeldimiste arv"),
    tags$li("Boost Index - Kasvav näitaja"),
    tags$li("Engagement Rate - Kasutajate kaasamise määr"),
    tags$li("Engagement Rate 60days - Kasutajate kaasamise määr 60 päeva jooksul"),
    tags$li("Views - Vaatamiste arv"),
    tags$li("Views. Avg- Keskmine vaatamiste arv"),
    tags$li("Avg. 1 Day	- Keskmine vaatamiste arv ühe päeva jooksul"),
    tags$li("Avg. 3 Day	- Keskmine vaatamiste arv ühe päeva jooksul"),
    tags$li("Avg. 7 Day	- Keskmine vaatamiste arv ühe päeva jooksul"),
    tags$li("Avg. 14 Day	- Keskmine vaatamiste arv ühe päeva jooksul"),
    tags$li("Avg. 30 Day	- Keskmine vaatamiste arv ühe päeva jooksul"),
    tags$li("Avg. 60 Day	- Keskmine vaatamiste arv ühe päeva jooksul"),
    tags$li("Comments Avg	- Keskmine kommentaaride arv"),
    tags$li("Youtube Link	- Kanali link")
  ),
  hr(),
  argonH1("Maailmapiiride andmete tunnused:", display = 4),
  tags$ul(
    tags$li("Shape - Riigi/piirkonna piir hulknurkadena"),
    tags$li("FIPS	- FIPS 10-4 riigikood"),
    tags$li("ISO2	- ISO 3166-1 kahetäheline riigikood"),
    tags$li("ISO3	- ISO 3166-1 kolmetäheline riigikood"),
    tags$li("UN	- ISO 3166-1 numbriline kolmekohaline riigikood"),
    tags$li("NAME	- Riigi/ala nimi"),
    tags$li("AREA	- Riigi maa-ala, millest on lahutatud veekogud (1000 hektarit), FAO Statistics (2002)"),
    tags$li("POP2005 - Rahvaarv, World Population Prospects (2005)"),
    tags$li("REGION	- Makrogeograafiline (mandripiirkond), UN Statistics"),
    tags$li("SUBREGION - Geograafiline alampiirkond, UN Statistics"),
    tags$li("LON - Pikkuskraad"),
    tags$li("LAT - Laiuskraad")
  )
)



# Andmed
andmedTab <- argonTab(
  tabName = "Andmed",
  argonRow(
    argonColumn(
      width = 12,
      argonH1(icon("database"), "Andmestikus sisalduvad andmed", display = 4),
      br(),
      argonRow(
        argonInfoCard(
          value = nrow(andmed), 
          title = "KANALEID/OBJEKTE",
          icon = icon("youtube"), 
          icon_background = "danger",
          shadow = TRUE
        ),
        argonInfoCard(
          value = ncol(andmed), 
          title = "TUNNUSEID",
          icon = icon("columns"), 
          icon_background = "warning",
          shadow = TRUE
        )
      ), br(), br(),
      DT::dataTableOutput("tabel"),
      hr(),
      argonH1("Top YouTube andmete ülevaade", display = 4),
      print(dfSummary(andmed), method = "render"),
    )
  )
)

# Korrelatsioon
korrelatsioonTab <- argonTab(
  tabName = "Korrelatsioon",
  argonH1(icon("table-cells"), "Korrelatsioonigraafik", display = 4), br(),
  plotOutput("correlation")
)

# Riigid
kaartTab <- argonTab(
  tabName = "Riigid",
  argonRow(
    argonColumn(
      width = 4,
      argonH1(icon("globe"), "Riikide kartogramm", display = 4),
      uiOutput("world_layer_select")
    ),
    argonColumn(
      width = 8,
      leafletOutput("world", width="100%", height = 650)
    )
  )
)

# Populaarsed
channelsAnalysis <- argonTab(
  tabName = "Populaarsemad",
  argonH1(icon("chart-area"), "Populaarsemad riigid, kategooriad, teemad", display = 4), br(),
  uiOutput("channels_top_select"), br(),
  plotOutput(outputId = "channels_country"),
  hr(),
  plotOutput(outputId = "channels_category"),
  hr(),
  argonRow(
    plotOutput(outputId = "channels_topic")
  )
)

# Lineaarne regressioon
linearAnalysis <- argonTab(
  tabName = "Lihtne lineaarne regressioon",
  argonH1(icon("chart-area"), "Lihtne lineaarne regressioon", display = 4),
  plotOutput(outputId = "p"), br(), br(),
  plotOutput(outputId = "p2"), br(), br(),
  plotOutput(outputId = "p3"), br(), br(),
  plotOutput(outputId = "p4")
)

tabs <- argonRow(
  argonTabSet(
    id = "tabs",
    card_wrapper = TRUE,
    size = "sm",
    width = 12,
    iconList = list(icon("circle-info"), icon("circle-info"), icon("database"), icon("table-cells"), icon("globe"), icon("chart-area"), icon("chart-area")),
    tutvustusTab,
    tunnusedTab,
    andmedTab,
    korrelatsioonTab,
    kaartTab,
    channelsAnalysis,
    linearAnalysis
  )
)

ui <- argonDashPage(
  title = "Populaarsemate Youtube kanalite andmed",
  description = "Projekt õppeaines Andmete visualiseerimine (Virumaa) - ITB8812",
  author = "Airika Andruse, Elmo Egers",
  navbar = NULL,
  header = header,
  body = argonDashBody(
    tabs
  )
)

server <- function(input, output) {
  # Andmed
  output$tabel = DT::renderDT({ andmed })
  
  # Kaardi andmed
  world_layer = reactive({ input$world_layer_select })
  output$world = renderLeaflet({
    bins = NULL
    domain = NULL
    tooltip = NULL
    legend_title = NULL
    
    if (world_layer() == "channels") {
      bins <- c(0, 1, 3, 5, 10, 20, 50, 100)
      domain = world_spdf@data$CHANNELS
      legend_title = "Kanalite koguarv (tk)"
      tooltip <- paste(
        "Riik: ", world_spdf@data$NAME,"<br/>", 
        "Maa-ala: ", world_spdf@data$AREA, "<br/>", 
        "Rahvaarv: ", round(world_spdf@data$POP2005, 2), "M", "<br/>",
        "Kanalite koguarv: ", world_spdf@data$CHANNELS, "tk",
        sep="") %>%
        lapply(htmltools::HTML)
    } else if (world_layer() == "followers") {
      bins = c(0, 50, 250, 750, 1500, 3000)
      domain = world_spdf@data$FOLLOWERS
      legend_title = "Kanalite jälgijate koguarv (M inimest)"
      tooltip <- paste(
        "Riik: ", world_spdf@data$NAME,"<br/>", 
        "Maa-ala: ", world_spdf@data$AREA, "<br/>", 
        "Rahvaarv: ", round(world_spdf@data$POP2005, 2), "M", "<br/>",
        "Kanalite koguarv: ", world_spdf@data$CHANNELS, "tk", "<br/>",
        "Kanalite jälgijate koguarv: ", world_spdf@data$FOLLOWERS, "M inimest",
        sep="") %>%
        lapply(htmltools::HTML)
    } else if (world_layer() == "population") {
      bins <- c(0, 10, 20, 50, 100, 500, Inf)
      domain = world_spdf@data$POP2005
      tooltip <- paste(
        "Riik: ", world_spdf@data$NAME,"<br/>", 
        "Maa-ala: ", world_spdf@data$AREA, "<br/>", 
        "Rahvaarv: ", round(world_spdf@data$POP2005, 2), "M", "<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
    }
    
    palette <- colorBin(palette="YlOrBr", domain = domain, na.color = "transparent", bins = bins)
    
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView(lat=10, lng=0, zoom=2) %>%
      addPolygons( 
        fillColor = ~palette(domain),
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = tooltip,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = palette,
        values = ~domain,
        opacity=0.9,
        title = legend_title,
        position = "bottomleft"
      )
  })

  output$world_layer_select <- renderUI({
    selectInput("world_layer_select", "Kaardi kiht", c(
      "Kanalite arv" = "channels",
      "Jälgijate arv" = "followers",
      "Rahvaarv" = "population"))
  })
  
  output$channels_top_select <- renderUI({
    radioButtons(inputId = "channels_top_select", 
                 inline = TRUE,
                 selected = 10,
                 label = "Kuva:",
                 choices = c("Top 3" = 3, "Top 5" = 5, "Top 10" = 10))
  })
  channels_top = reactive({ input$channels_top_select })
  
  # Riigid (Country)
  output$channels_country = renderPlot({
    df = as.data.frame(table(andmed$Country))
    colnames(df) <- c("Riik", "Kanalite_arv")
    top_n(df, n = as.numeric(channels_top()), Kanalite_arv) %>%
      ggplot(., aes(reorder(Riik, -Kanalite_arv), Kanalite_arv, fill = Kanalite_arv)) +
        geom_bar(stat = "identity") +
        ggtitle("Populaarseimad riigid") +
        xlab("Riigikood") +
        ylab("Kanalite arv") +
        theme(
          plot.title = element_text(color="#666666", size=26, face="bold"),
          legend.title = element_text(size=14),
          legend.text = element_text(size=12),
          axis.title.x = element_text(color="#000000", size=16, face="bold", margin=margin(20, 0, 0, 0, "pt")),
          axis.title.y = element_text(color="#000000", size=16, face="bold", margin=margin(0, 20, 0, 0, "pt")),
          axis.text.x = element_text(size=14, margin=margin(5, 0, 0, 0))
        )
  })
  
  
  # Kategooria (Main Video Category)
  output$channels_category = renderPlot({
    df = as.data.frame(table(andmed$Main.Video.Category))
    colnames(df) <- c("Kategooria", "Kanalite_arv")
    top_n(df, n = as.numeric(channels_top()), Kanalite_arv) %>%
      ggplot(., aes(reorder(Kategooria, -Kanalite_arv), Kanalite_arv, fill = Kanalite_arv)) +
      scale_fill_gradient(low = "purple4", high = "purple") +
      geom_bar(stat = "identity") +
      ggtitle("Populaarseimad kategooriad") +
      xlab("Kategooriad") +
      ylab("Kanalite arv") +
      theme(
        plot.title = element_text(color="#666666", size=26, face="bold"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        axis.title.x = element_text(color="#000000", size=16, face="bold", margin=margin(20, 0, 0, 0, "pt")),
        axis.title.y = element_text(color="#000000", size=16, face="bold", margin=margin(0, 20, 0, 0, "pt")),
        axis.text.x = element_text(size=14, angle=90, margin=margin(0, 5, 0, 0))
      )
  })
  
  # Teema (Main topic)
  output$channels_topic = renderPlot({
    df = as.data.frame(table(andmed$Main.topic))
    colnames(df) <- c("Teema", "Kanalite_arv")
    top_n(df, n = as.numeric(channels_top()), Kanalite_arv) %>%
      ggplot(., aes(reorder(Teema, -Kanalite_arv), Kanalite_arv, fill = Kanalite_arv)) +
      scale_fill_gradient(low = "indianred4", high = "indianred") +
      geom_bar(stat = "identity") +
      ggtitle("Populaarseimad teemad") +
      xlab("Teemad") +
      ylab("Kanalite arv") +
      theme(
        plot.title = element_text(color="#666666", size=26, face="bold"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        axis.title.x = element_text(color="#000000", size=16, face="bold", margin=margin(20, 0, 0, 0, "pt")),
        axis.title.y = element_text(color="#000000", size=16, face="bold", margin=margin(0, 20, 0, 0, "pt")),
        axis.text.x = element_text(size=14, angle=90, margin=margin(0, 5, 0, 0))
      )
  })
  
  # Korrelatsioon
  output$correlation = renderPlot({
    cor_df <- round(cor(andmed[, c("followers_M", "Likes_M", "Engagement.Rate", "Engagement.Rate.60days", "Views_M", "Comments.Avg", "Avg..60.day")]), 2)
    melted_cor <- melt(cor_df)
    ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile() +
      geom_text(aes(Var2, Var1, label = value), size = 7) +
      scale_fill_gradient2(low = "red", high = "green",
                           limit = c(-1,1), name="Korrelatsioon") +
      theme(
        plot.title = element_text(color="#666666", size=26, face="bold"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color="#000000", size=16, face="bold", margin=margin(20, 0, 0, 0, "pt")),
        axis.text.y = element_text(color="#000000", size=16, face="bold", margin=margin(0, 20, 0, 0, "pt")),
        panel.background = element_blank()
      )
  })
  
  # Lihtne lineaarne regressioon jälgijate ja meeldimiste vahel
  output$p <- renderPlot({
    ggplot(andmed, aes(x = followers_M, y = Likes_M)) + 
      geom_point(color='blue') +
      geom_smooth(method = "lm", se = FALSE, color='red') +
      theme(
        axis.title.x = element_text(color="#000000", size=16, face="bold", margin=margin(20, 0, 0, 0, "pt")),
        axis.title.y = element_text(color="#000000", size=16, face="bold", margin=margin(0, 20, 0, 0, "pt")),
      )
  })
  
  # Lihtne lineaarne regressioon jälgijate ja vaatamiste vahel
  output$p2 <- renderPlot({
    ggplot(andmed, aes(x = followers_M, y = Views_M)) + 
      geom_point(color='blue') +
      geom_smooth(method = "lm", se = FALSE, color='red') +
      theme(
        axis.title.x = element_text(color="#000000", size=16, face="bold", margin=margin(20, 0, 0, 0, "pt")),
        axis.title.y = element_text(color="#000000", size=16, face="bold", margin=margin(0, 20, 0, 0, "pt")),
      )
  })
  
  # Lihtne lineaarne regressioon meeldimiste ja vaatamiste vahel
  output$p3 <- renderPlot({
    ggplot(andmed, aes(x = Likes_M, y = Views_M)) + 
      geom_point(color='blue') +
      geom_smooth(method = "lm", se = FALSE, color='red') +
      theme(
        axis.title.x = element_text(color="#000000", size=16, face="bold", margin=margin(20, 0, 0, 0, "pt")),
        axis.title.y = element_text(color="#000000", size=16, face="bold", margin=margin(0, 20, 0, 0, "pt")),
      )
  })
  
  # Lihtne lineaarne regressioon meeldimiste ja vaatamiste vahel
  output$p4 <- renderPlot({
    ggplot(andmed, aes(x = Likes_M, y = Boost.Index)) + 
      geom_point(color='blue') +
      geom_smooth(method = "lm", se = FALSE, color='red') +
      theme(
        axis.title.x = element_text(color="#000000", size=16, face="bold", margin=margin(20, 0, 0, 0, "pt")),
        axis.title.y = element_text(color="#000000", size=16, face="bold", margin=margin(0, 20, 0, 0, "pt")),
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
