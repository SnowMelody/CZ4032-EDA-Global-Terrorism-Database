library(shiny)
library(plotly)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(treemap)
library(tm)
library(wordcloud)
library(SnowballC)
library(leaflet)

df <- read.csv("testdata.csv", stringsAsFactors = FALSE)



# shiny app
ui <- navbarPage(
  "EDA of Terrorist Dataset",
  
  tabPanel("Correlation",
    plotlyOutput("corr.plot", height = "100%", width = "60%")
  ),
  
  navbarMenu("Attack Type",
    tabPanel("Num of events of each attack type (bar plot)",
      plotlyOutput("atktype.plot1", width = "75%")
    ),
    tabPanel("Num of events of each attack type (word cloud)",
      plotOutput("atktype.plot2")
    ),
    tabPanel("Num of events of each atk type per terrorist group (bubble plot)",
      plotlyOutput("atktype.plot3")
    ),
    tabPanel("Num of events of each atk type per terrorist group (line plot)",
      plotlyOutput("atktype.plot4")
    ),
    tabPanel("Num of events of each atk type per terrorist group (treemap)",
      plotOutput("atktype.plot5")
    ),
    tabPanel("Success rate of each atk type per terrorist group (line plot)",
      plotlyOutput("atktype.plot6")
    )
  ),
  
  navbarMenu("Weapon Type",
    tabPanel("Num of events of each weapon type (bar plot)",
      plotlyOutput("weaptype.plot1", width = "75%")
    ),
    tabPanel("Num of events of each weapon type (word cloud)",
      plotOutput("weaptype.plot2")
    ),
    tabPanel("Num of events of each weapon type per terrorist group (bubble plot)",
      plotlyOutput("weaptype.plot3")
    ),
    tabPanel("Num of events of each weapon type per terrorist group (line plot)",
      plotlyOutput("weaptype.plot4")
    ),
    tabPanel("Num of events of each weapon type per terrorist group (treemap)",
      plotOutput("weaptype.plot5")
    )
  ),
  
  navbarMenu("Target",
    tabPanel("Num of events of each target (bar plot)",
      plotlyOutput("target.plot1", width = "75%")
    ),
    tabPanel("Num of events of each target (word cloud)",
      plotOutput("target.plot2")
    ),
    tabPanel("Num of events of each target per terrorist group (bubble plot)",
      plotlyOutput("target.plot3")
    ),
    tabPanel("Num of events of each target per terrorist group (line plot)",
      plotlyOutput("target.plot4")
    ),
    tabPanel("Num of events of each target per terrorist group (treemap)",
      plotOutput("target.plot5")
    )
  ),
  
  navbarMenu("Year",
    tabPanel("Num of events of each terrorist group (bar plot)",
      plotlyOutput("terror.plot1", width = "75%")
    ),
    tabPanel("Num of events of each terrorist group per year (bubble plot)",
      plotlyOutput("terror.plot3")
    ),
    tabPanel("Num of events of each terrorist group per year (line plot)",
      plotlyOutput("terror.plot4")
    ),
    tabPanel("Num of events of each terrorist group per year (treemap)",
      plotOutput("terror.plot5")
    ),
    tabPanel("Num of kills of each terrorist group (bar plot)",
      plotlyOutput("terror.plot6", width = "75%")
    ),
    tabPanel("Num of kills of each terrorist group per year (bubble plot)",
      plotlyOutput("terror.plot7")
    ),
    tabPanel("Num of kills of each terrorist group per year (line plot)",
      plotlyOutput("terror.plot8")
    ),
    tabPanel("Num of kills of each terrorist group per year (treemap)",
      plotOutput("terror.plot9")
    ),
    tabPanel("Num of wounded of each terrorist group (bar plot)",
      plotlyOutput("terror.plot10", width = "75%")
    ),
    tabPanel("Num of wounded of each terrorist group per year (bubble plot)",
      plotlyOutput("terror.plot11")
    ),
    tabPanel("Num of wounded of each terrorist group per year (line plot)",
      plotlyOutput("terror.plot12")
    ),
    tabPanel("Num of wounded of each terrorist group per year (treemap)",
      plotOutput("terror.plot13")
    )
  ),
  
  navbarMenu("World Map",
    tabPanel("Events of Top 5 Terrorist Groups Based on Total Number of Kills",
      leafletOutput("world.plot1")
    ),
    tabPanel("Events of Top 5 Terrorist Groups Based on Total Number of Wounded",
      leafletOutput("world.plot2")
    )
  )
)



server <- function(input, output) {
  output$corr.plot <- renderPlotly({
    df_temp <- df
    df_temp$city <- as.numeric(factor(df_temp$city))
    df_temp$provstate <- as.numeric(factor(df_temp$provstate))
    df_temp$gname <- as.numeric(factor(df_temp$gname))
    df_temp$target1 <- as.numeric(factor(df_temp$target1))
    
    corrdata <- df_temp[, c("iyear", "imonth", "iday", "country", "region", "provstate", "city", 
                            "latitude", "longitude", "doubtterr", "multiple", "success", "suicide", 
                            "attacktype1", "targtype1", "target1", "natlty1", "gname", 
                            "weaptype1", "property", "ishostkid", "INT_LOG", "INT_IDEO", "INT_ANY", "nkill", "nwound")]
    
    # normalise and remove rows with NA
    corrdata <- scale(corrdata)
    corrdata <- na.omit(corrdata)
    corr <- cor(corrdata)
    
    # plot pearson correlation
    p <- ggcorrplot(corr, hc.order = TRUE, type = "lower", outline.col = "white")
    p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    p <- p + theme(axis.text.x = element_text(angle = 90))
    ggplotly(p)
  })
  
  
  output$atktype.plot1 <- renderPlotly({
    df_display <- df %>% group_by(attacktype1_txt) %>% summarise(n = length(attacktype1_txt)) %>% ungroup()
    
    # display barplot in descending order
    p <- ggplot(data = df_display, aes(x = reorder(attacktype1_txt, -n), y = n, text = paste("Attack type: ", attacktype1_txt, "\nNumber of events: ", n, sep=""), group=1))
    p <- p + geom_bar(stat = "identity", fill = "#FF3333") 
    p <- p + theme_bw(base_size = 10) + labs(x = "Attack type", y = "Number of events")
    p <- p + theme(axis.text.x = element_text(angle = 25))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$atktype.plot2 <- renderPlot({
    df_temp <- df %>% filter(!is.na(attacktype1_txt))
    df_temp <- df %>% filter(attacktype1_txt != "")
    text <- df_temp$attacktype1_txt
    
    word_corpus <- Corpus(VectorSource(text))
    word_corpus = tm_map(word_corpus, content_transformer(tolower))
    
    # create term document matrix
    tdm = TermDocumentMatrix(word_corpus, control = list(minWordLength = 1))
    
    # frequent terms and associations
    freqTerms <- findFreqTerms(tdm, lowfreq=1)
    m <- as.matrix(tdm)
    
    # calculate word frequency
    v <- sort(rowSums(m), decreasing=TRUE)
    words <- names(v)
    d <- data.frame(word = words, freq = v)
    wctop <- wordcloud(d$word, d$freq, min.freq = 5, colors=brewer.pal(9, "Set1"))
  })
  
  
  output$atktype.plot3 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,attacktype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    colnames(df_display) <- c("Terrorist group", "Attack type", "Number of events")
    p <- ggplot(data = df_display, aes(x = `Terrorist group`, y = `Number of events`, text = paste("Terrorist group: ", `Terrorist group`, "\nAttack type: ", `Attack type`, "\nNumber of events: ", `Number of events`, sep=""), group=1))
    p <- p + geom_point(aes(colour = `Attack type`, size = `Number of events`), alpha = 0.6, position = "jitter") + 
      scale_size_continuous(range = c(4, 10)) + labs(size = NULL)
    p <- p + theme(axis.text.x = element_text(angle = 10))
    
    ggplotly(p, tooltip=c("text"))
  })

  
  output$atktype.plot4 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,attacktype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    colnames(df_display) <- c("Terrorist group", "Attack type", "Number of events")
    p <- ggplot(data = df_display, aes(x = `Terrorist group`, y = `Number of events`, colour = `Attack type`))
    p <- p + geom_line(aes(group=1)) + geom_point(shape = 20, size = 1) + theme_bw()
    p <- p + theme(axis.text.x = element_text(angle = 10))
  
    ggplotly(p)
  })
  
  
  output$atktype.plot5 <- renderPlot({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,attacktype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    treemap(df_display,
            index = c("attacktype1_txt", "gname"),
            vSize = "n",
            palette = "Reds",
            title = "")
  })
  
  
  output$atktype.plot6 <- renderPlotly({
    # success rates of attacks per attack type per terrorist group
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,attacktype1_txt) %>% summarise((sum(success==1)/sum(success>=0))*100) %>% ungroup()
    
    colnames(df_display) <- c("Terrorist group", "Attack_type", "Successes")
    p <- ggplot(data = df_display, aes(x = `Terrorist group`, y = Successes, colour = Attack_type, text = paste("Terrorist group: ", `Terrorist group`, "\nSuccess rate: ", Successes, "%", "\nAttack type: ", Attack_type, sep=""), group=1))
    p <- p + geom_line(aes(group=1)) + geom_point(shape = 20, size = 1) + theme_bw() + labs(y = "Success rate (%)", colour = "Attack type")
    p <- p + theme(axis.text.x = element_text(angle = 10))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$weaptype.plot1 <- renderPlotly({
    df_display <- df %>% group_by(weaptype1_txt) %>% summarise(n = length(weaptype1_txt)) %>% top_n(n = 10) %>% ungroup()
    
    # display barplot in descending order
    p <- ggplot(data = df_display, aes(x = reorder(weaptype1_txt, -n), y = n, text = paste("Weapon type: ", weaptype1_txt, "\nNumber of events: ", n, sep=""), group=1))
    p <- p + geom_bar(stat = "identity", fill = "#FF3333") 
    p <- p + theme_bw(base_size = 10) + labs(x = "Weapon type", y = "Number of events")
    p <- p + theme(axis.text.x = element_text(angle = 25))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$weaptype.plot2 <- renderPlot({
    df_temp <- df %>% filter(!is.na(weaptype1_txt))
    df_temp <- df %>% filter(weaptype1_txt != "")
    text <- df_temp$weaptype1_txt
    
    word_corpus <- Corpus(VectorSource(text))
    word_corpus = tm_map(word_corpus, content_transformer(tolower))
    
    # create term document matrix
    tdm = TermDocumentMatrix(word_corpus, control = list(minWordLength = 1))
    
    # frequent terms and associations
    freqTerms <- findFreqTerms(tdm, lowfreq=1)
    m <- as.matrix(tdm)
    
    # calculate word frequency
    v <- sort(rowSums(m), decreasing=TRUE)
    words <- names(v)
    d <- data.frame(word = words, freq = v)
    wctop <- wordcloud(d$word, d$freq, min.freq = 5, colors=brewer.pal(9, "Set1"))
  })
  
  
  output$weaptype.plot3 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,weaptype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    colnames(df_display) <- c("Terrorist group", "Weapon type", "Number of events")
    p <- ggplot(data = df_display, aes(x = `Terrorist group`, y = `Number of events`, text = paste("Terrorist group: ", `Terrorist group`, "\nWeapon type: ", `Weapon type`, "\nNumber of events: ", `Number of events`, sep=""), group=1))
    p <- p + geom_point(aes(colour = `Weapon type`, size = `Number of events`), alpha = 0.6, position = "jitter") + 
      scale_size_continuous(range = c(4, 10)) + labs(size = NULL)
    p <- p + theme(axis.text.x = element_text(angle = 10))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$weaptype.plot4 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,weaptype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    colnames(df_display) <- c("Terrorist group", "Weapon type", "Number of events")
    p <- ggplot(data = df_display, aes(x = `Terrorist group`, y = `Number of events`, colour = `Weapon type`))
    p <- p + geom_line(aes(group=1)) + geom_point(shape = 20, size = 1) + theme_bw()
    p <- p + theme(axis.text.x = element_text(angle = 10))
    
    ggplotly(p)
  })
  
  
  output$weaptype.plot5 <- renderPlot({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,weaptype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    treemap(df_display,
            index = c("weaptype1_txt", "gname"),
            vSize = "n",
            palette = "Reds",
            title = "")
  })
  
  
  output$target.plot1 <- renderPlotly({
    df_display <- df %>% group_by(targtype1_txt) %>% summarise(n = length(targtype1_txt)) %>% top_n(n = 10) %>% ungroup()
    
    # display barplot in descending order
    p <- ggplot(data = df_display, aes(x = reorder(targtype1_txt, -n), y = n, text = paste("Target: ", targtype1_txt, "\nNumber of events: ", n, sep=""), group=1))
    p <- p + geom_bar(stat = "identity", fill = "#FF3333") 
    p <- p + theme_bw(base_size = 10) + labs(x = "Target", y = "Number of events")
    p <- p + theme(axis.text.x = element_text(angle = 25))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$target.plot2 <- renderPlot({
    df_temp <- df %>% filter(!is.na(targtype1_txt))
    df_temp <- df %>% filter(targtype1_txt != "")
    text <- df_temp$targtype1_txt
    
    word_corpus <- Corpus(VectorSource(text))
    word_corpus = tm_map(word_corpus, content_transformer(tolower))
    
    # create term document matrix
    tdm = TermDocumentMatrix(word_corpus, control = list(minWordLength = 1))
    
    # frequent terms and associations
    freqTerms <- findFreqTerms(tdm, lowfreq=1)
    m <- as.matrix(tdm)
    
    # calculate word frequency
    v <- sort(rowSums(m), decreasing=TRUE)
    words <- names(v)
    d <- data.frame(word = words, freq = v)
    wctop <- wordcloud(d$word, d$freq, min.freq = 5, colors=brewer.pal(9, "Set1"))
  })
  
  
  output$target.plot3 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,targtype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    colnames(df_display) <- c("Terrorist group", "Target", "Number of events")
    p <- ggplot(data = df_display, aes(x = `Terrorist group`, y = `Number of events`, text = paste("Terrorist group: ", `Terrorist group`, "\nTarget: ", Target, "\nNumber of events: ", `Number of events`, sep=""), group=1))
    p <- p + geom_point(aes(colour = Target, size = `Number of events`), alpha = 0.6, position = "jitter") + 
      scale_size_continuous(range = c(4, 10)) + labs(size = NULL)
    p <- p + theme(axis.text.x = element_text(angle = 10))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$target.plot4 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,targtype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    colnames(df_display) <- c("Terrorist group", "Target", "Number of events")
    p <- ggplot(data = df_display, aes(x = `Terrorist group`, y = `Number of events`, colour = Target))
    p <- p + geom_line(aes(group=1)) + geom_point(shape = 20, size = 1) + theme_bw()
    p <- p + theme(axis.text.x = element_text(angle = 10))
    
    ggplotly(p)
  })
  
  
  output$target.plot5 <- renderPlot({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(gname,targtype1_txt) %>% summarise(n = length(gname)) %>% ungroup()
    
    treemap(df_display,
            index = c("targtype1_txt", "gname"),
            vSize = "n",
            palette = "Reds",
            title = "")
  })
  
  
  output$terror.plot1 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    
    # display barplot in descending order
    p <- ggplot(data = df_display, aes(x = reorder(gname, -n), y = n, text = paste("Terrorist group: ", gname, "\nNumber of events: ", n, sep=""), group=1))
    p <- p + geom_bar(stat = "identity", fill = "#FF3333") 
    p <- p + theme_bw(base_size = 10) + labs(x = "Terrorist group", y = "Number of events")
    p <- p + theme(axis.text.x = element_text(angle = 25))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$terror.plot3 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(iyear,gname) %>% summarise(n = length(iyear)) %>% ungroup()
    
    colnames(df_display) <- c("Year", "Terrorist group", "Number of events")
    p <- ggplot(data = df_display, aes(x = Year, y = `Number of events`, text = paste("Year: ", Year, "\nTerrorist group: ", `Terrorist group`, "\nNumber of events: ", `Number of events`, sep=""), group=1))
    p <- p + geom_point(aes(colour = `Terrorist group`, size = `Number of events`), alpha = 0.6, position = "jitter") + 
      scale_size_continuous(range = c(4, 10)) + labs(size = NULL)
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$terror.plot4 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(iyear,gname) %>% summarise(n = length(iyear)) %>% ungroup()
    
    colnames(df_display) <- c("Year", "Terrorist group", "Number of events")
    p <- ggplot(data = df_display, aes(x = Year, y = `Number of events`, colour = `Terrorist group`))
    p <- p + geom_line() + geom_point(shape = 20, size = 1) + theme_bw()
    
    ggplotly(p)
  })
  
  
  output$terror.plot5 <- renderPlot({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = length(gname)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(iyear,gname) %>% summarise(n = length(iyear)) %>% ungroup()
    
    treemap(df_display,
            index = c("gname", "iyear"),
            vSize = "n",
            palette = "Reds",
            title = "")
  })
  
  
  output$terror.plot6 <- renderPlotly({
    df_display <- df %>% group_by(gname) %>% summarise(n = sum(nkill)) %>% top_n(n = 10) %>% ungroup()
    
    # display barplot in descending order
    p <- ggplot(data = df_display, aes(x = reorder(gname, -n), y = n, text = paste("Terrorist group: ", gname, "\nNumber of kills: ", n, sep=""), group=1))
    p <- p + geom_bar(stat = "identity", fill = "#FF3333") 
    p <- p + theme_bw(base_size = 10) + labs(x = "Terrorist group", y = "Number of kills")
    p <- p + theme(axis.text.x = element_text(angle = 10))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$terror.plot7 <- renderPlotly({
    # filter top 10 for display
    df_temp <- df %>% group_by(gname) %>% summarise(n = sum(nkill)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_temp$gname) %>% group_by(iyear,gname) %>% summarise(n = sum(nkill)) %>% ungroup()
    
    colnames(df_display) <- c("Year", "Terrorist group", "Number of kills")
    p <- ggplot(data = df_display, aes(x = Year, y = `Number of kills`, text = paste("Year: ", Year, "\nTerrorist group: ", `Terrorist group`, "\nNumber of kills: ", `Number of kills`, sep=""), group=1))
    p <- p + geom_point(aes(colour = `Terrorist group`, size = `Number of kills`), alpha = 0.6, position = "jitter") + 
      scale_size_continuous(range = c(4, 10)) + labs(size = NULL)
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$terror.plot8 <- renderPlotly({
    # filter top 10 for display
    df_temp <- df %>% group_by(gname) %>% summarise(n = sum(nkill)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_temp$gname) %>% group_by(iyear,gname) %>% summarise(n = sum(nkill)) %>% ungroup()
    
    colnames(df_display) <- c("Year", "Terrorist group", "Number of kills")
    p <- ggplot(data = df_display, aes(x = Year, y = `Number of kills`, colour = `Terrorist group`))
    p <- p + geom_line() + geom_point(shape = 20, size = 1) + theme_bw()
    
    ggplotly(p)
  })
  
  
  output$terror.plot9 <- renderPlot({
    # filter top 10 for display
    df_temp <- df %>% group_by(gname) %>% summarise(n = sum(nkill)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_temp$gname) %>% group_by(iyear,gname) %>% summarise(n = sum(nkill)) %>% ungroup()
    
    treemap(df_display,
            index = c("gname", "iyear"),
            vSize = "n",
            palette = "Reds",
            title = "")
  })
  
  
  output$terror.plot10 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = sum(nwound)) %>% top_n(n = 10) %>% ungroup()
    
    # display barplot in descending order
    p <- ggplot(data = df_display, aes(x = reorder(gname, -n), y = n, text = paste("Terrorist group: ", gname, "\nNumber of wounded: ", n, sep=""), group=1))
    p <- p + geom_bar(stat = "identity", fill = "#FF3333") 
    p <- p + theme_bw(base_size = 10) + labs(x = "Terrorist group", y = "Number of wounded")
    p <- p + theme(axis.text.x = element_text(angle = 10))
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$terror.plot11 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = sum(nwound)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(iyear,gname) %>% summarise(n = sum(nwound)) %>% ungroup()
    
    colnames(df_display) <- c("Year", "Terrorist group", "Number of wounded")
    p <- ggplot(data = df_display, aes(x = Year, y = `Number of wounded`, text = paste("Year: ", Year, "\nTerrorist group: ", `Terrorist group`, "\nNumber of wounded: ", `Number of wounded`, sep=""), group=1))
    p <- p + geom_point(aes(colour = `Terrorist group`, size = `Number of wounded`), alpha = 0.6, position = "jitter") + 
      scale_size_continuous(range = c(4, 10)) + labs(size = NULL)
    
    ggplotly(p, tooltip=c("text"))
  })
  
  
  output$terror.plot12 <- renderPlotly({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = sum(nwound)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(iyear,gname) %>% summarise(n = sum(nwound)) %>% ungroup()
    
    colnames(df_display) <- c("Year", "Terrorist group", "Number of wounded")
    p <- ggplot(data = df_display, aes(x = Year, y = `Number of wounded`, colour = `Terrorist group`))
    p <- p + geom_line() + geom_point(shape = 20, size = 1) + theme_bw()
    
    ggplotly(p)
  })
  
  
  output$terror.plot13 <- renderPlot({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = sum(nwound)) %>% top_n(n = 10) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname) %>% group_by(iyear,gname) %>% summarise(n = sum(nwound)) %>% ungroup()
    
    treemap(df_display,
            index = c("gname", "iyear"),
            vSize = "n",
            palette = "Reds",
            title = "")
  })
  
  
  output$world.plot1 <- renderLeaflet({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = sum(nkill)) %>% top_n(n = 5) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname)
    dfr <- df_display
    
    leaflet(data = dfr) %>%
    addTiles() %>%

    addMarkers(lat = dfr$latitude, lng = dfr$longitude, clusterOptions = markerClusterOptions(),
               popup = paste("<strong>Date: </strong>", dfr$iday,"/",dfr$imonth,"/", dfr$iyear,
                            "<br><br><strong>Place: </strong>", dfr$city,"-",dfr$country_txt,
                            "<br><strong>Killed: </strong>", dfr$nkill,
                            "<br><strong>Wounded: </strong>", dfr$nwound,
                            "<br><strong>Attack type: </strong>", dfr$attacktype1_txt,
                            "<br><strong>Weapon type: </strong>", dfr$weaptype1_txt,
                            "<br><strong>Target: </strong>", dfr$targtype1_txt,
                            "<br><strong>Target's nationality: </strong>", dfr$natlty1_txt,
                            "<br><strong>Success: </strong>", dfr$success))
  })
  
  
  output$world.plot2 <- renderLeaflet({
    df_display <- df %>% filter(gname != "Unknown")
    df_display <- df_display %>% group_by(gname) %>% summarise(n = sum(nwound)) %>% top_n(n = 5) %>% ungroup()
    df_display <- df %>% filter(gname %in% df_display$gname)
    dfr <- df_display
    
    leaflet(data = dfr) %>%
    addTiles() %>%
      
    addMarkers(lat = dfr$latitude, lng = dfr$longitude, clusterOptions = markerClusterOptions(),
               popup = paste("<strong>Date: </strong>", dfr$iday,"/",dfr$imonth,"/", dfr$iyear,
                             "<br><br><strong>Place: </strong>", dfr$city,"-",dfr$country_txt,
                             "<br><strong>Killed: </strong>", dfr$nkill,
                             "<br><strong>Wounded: </strong>", dfr$nwound,
                             "<br><strong>Attack type: </strong>", dfr$attacktype1_txt,
                             "<br><strong>Weapon type: </strong>", dfr$weaptype1_txt,
                             "<br><strong>Target: </strong>", dfr$targtype1_txt,
                             "<br><strong>Target's nationality: </strong>", dfr$natlty1_txt,
                             "<br><strong>Success: </strong>", dfr$success))
  })
  
}



app <- shinyApp(ui=ui, server=server)
runApp(app, host="0.0.0.0", port=8080)