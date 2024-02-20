library(shiny)
library(psych)
library(ggplot2)
library(dplyr)
library(shinythemes)




dane <- ToothGrowth
colnames(dane) <- c("dlugosc", "suplement", "dawkowanie")
dane$suplement <- as.factor(dane$suplement)
dane$dawkowanie <- as.factor(dane$dawkowanie)

# UI
ui <- fluidPage(
  titlePanel("Analiza danych - ToothGrowth"),
  navbarPage(title = "",
             tabPanel("Statystyki ",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "stat", 
                                      label = "Statystyki",
                                      choices = c("Długość zębów",
                                                  "Długość zębów ze względu na suplementacje",
                                                  "Dlugość zębów ze względu na dawkowanie"), 
                                      selected = "Długość zębów")
                        ),
                        mainPanel(
                          tableOutput("stat1")
                        )
                      )
             ),
             tabPanel("Wykresy gęstości",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "dens",
                                      label = "Wykresy gętości - różne czynniki",
                                      choices = c("Długość zębów w zależności od dawkowania", 
                                                  "Długość zębów w zależności od suplementacji"),
                                      selected = "Długość zębów w zależności od dawkowania")
                        ),
                        mainPanel(
                          plotOutput("plot2")
                        )
                      )
              ),
             tabPanel("Wykresy pudełkowe",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "box", 
                                      label = "Wykresy pudełkowe - różne czynniki",
                                      choices = c("Długość zębów",
                                                  "Dlugość zębów ze względu na dawkowanie",
                                                  "Długość zębów ze względu na suplementacje"), 
                                      selected = "Długość zębów")
                        ),
                        mainPanel(
                          plotOutput("plot1")
                          
                        )
                      )
             ),
             tabPanel("Analiza wariancji - dawkowanie",
                     sidebarLayout(
                       sidebarPanel(
                         checkboxInput("aov",
                                       "ANOVA"),
                         checkboxInput("tk",
                                       "Test Tukey'a"),
                       ),
                       mainPanel(
                         textOutput("wynik1TEXT"),
                         verbatimTextOutput("wynik1_2TEXT")
                       )
                     )                          
             ),
             tabPanel("Analiza wariancji - suplementacja",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxInput("aov2",
                                        "ANOVA"),
                          checkboxInput("bartlett",
                                        "Test homogeniczności"),
                          checkboxInput("shapiro",
                                        "Test normalnosci"),
                        ),
                        mainPanel(
                          textOutput("wynik2TEXT"),
                          textOutput("wynik4TEXT"),
                          tableOutput("wynik3TABELA")
                        )
                      )                          
             ),
             
             
)
)

# Server
server <- function(input, output) {
  output$stat1 <- renderTable({
    if(input$stat == "Długość zębów"){
      dane %>%
        summarise(
          srednia = mean(dlugosc),
          odchylenie_stan = sd(dlugosc),
          wariancja = var(dlugosc),
          mediana = median(dlugosc),
          zakres_min = min(dlugosc),
          zakres_max = max(dlugosc),
          skosnosc = skew(dlugosc),
          kurtoza = kurtosi(dlugosc)
        )
    }else if (input$stat == "Dlugość zębów ze względu na dawkowanie"){
      dane %>%
        group_by(dawkowanie) %>%
        summarise(
          srednia = mean(dlugosc),
          odchylenie_stan = sd(dlugosc),
          wariancja = var(dlugosc),
          mediana = median(dlugosc),
          zakres_min = min(dlugosc),
          zakres_max = max(dlugosc),
          skosnosc = skew(dlugosc),
          kurtoza = kurtosi(dlugosc))
    }else{
      dane %>%
        group_by(suplement) %>%
        summarise(
          srednia = mean(dlugosc),
          odchylenie_standardowe = sd(dlugosc),
          wariancja = var(dlugosc),
          mediana = median(dlugosc),
          zakres_min = min(dlugosc),
          zakres_max = max(dlugosc),
          skosnosc = skew(dlugosc),
          kurtoza = kurtosi(dlugosc))
    }
  })
  
  
  
  output$plot1 <- renderPlot({
    if (input$box == "Długość zębów"){
      dane %>%
        ggplot()+
        geom_boxplot(aes(y=dlugosc), fill = "skyblue", color = "black", alpha = 0.7)+
        labs(title = "Wykres pudełkowy dla długości zębów świnek")
    }
    else if (input$box == "Długość zębów ze względu na suplementacje"){
      
      dane %>%
        ggplot()+
        geom_boxplot(aes(x = factor(suplement), y = dlugosc), fill = "orange", color = "black", width = 0.5)+
        labs(title = "Suplement a długość zębów - Wykres Pudełkowy", x = "Suplement", y = "Długość")+
        scale_x_discrete(labels = c("Sok pomarańczowy", "Kwas askorbinowy"))+
        theme_minimal()
    }
    else{
      dane %>%
        ggplot()+
        geom_boxplot(aes(x = factor(dawkowanie), y = dlugosc),fill = "yellow", color = "black", width = 0.5)+
        labs(title = "Dawkowanie a długość zębów - Wykres Pudełkowy", x = "Dawkowanie", y = "Długość")+
        theme_minimal()
    }
  })
  
    
    
    output$plot2 <- renderPlot({
    if(input$dens == "Długość zębów w zależności od dawkowania"){
      dane %>%
        ggplot(aes(x = dlugosc, fill = factor(dawkowanie)))+
        geom_density(alpha = 0.5)+
        scale_fill_discrete(name = "Dawkowanie")+
        labs(x = "Długość", y = "Gęstość")
    }
    else{
      dane%>%
        ggplot(aes(x = dlugosc, fill = factor(suplement)))+
        geom_density(alpha = 0.5)+
        scale_fill_discrete(name = "Suplement", labels = c("Sok pomarańczowy", "Kwas askorbinowy"))+
        labs(x = "Długość", y = "Gęstość")
    }
  })
    reactive1 <- reactive({
      if(input$aov){
        aov_result <- summary(aov(dane$dlugosc ~ dane$dawkowanie))
        p_value <- round(aov_result[[1]]$`Pr(>F)`[1], digits = 16)
        result <- paste("Wartość p-value w teście ANOVA wynosi około ", p_value,
                        " .Przejdźmy do testu Tukey'a")
        return(result)
        
      }
    })
    reactive2 <- reactive({
      if (input$tk){
        anova_dose <- aov(dane$dlugosc ~ dane$dawkowanie)
        tukey_result <- TukeyHSD(anova_dose)
        result <- capture.output(tukey_result)
        return(result)
      }
    })
    output$wynik1TEXT <- renderText({
      reactive1()
    })
    output$wynik1_2TEXT <- renderPrint({
      reactive2()
    })
    
    reactive3 <- reactive({
      if(input$aov2){
        aov2_result <- summary(aov(dane$dlugosc ~ dane$suplement))
        p_value <- round(aov2_result[[1]]$`Pr(>F)`[1], digits = 4)
        result <- paste("Wartość p-value w teście ANOVA wynosi około ", p_value,
                        " .Sprawdźmy założenia do testu ANOVA.")
        return(result)
      }
    })
    reactive5 <- reactive({
      if(input$bartlett){
        result <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(dane$dlugosc ~ dane$suplement)$p.value, 4))
        return(result)
      }
    })
    reactive4 <-reactive({
      if(input$shapiro){
        shapiro <- tapply(dane$dlugosc, dane$suplement, shapiro.test)
        result <- data.frame(
          Grupa = names(shapiro),
          P_value = sapply(shapiro, function(x) x$p.value)
        )
        return(result)
      }
      })
    output$wynik2TEXT <- renderText({
      reactive3()
    })
    output$wynik3TABELA <- renderTable({
      reactive4()
    })
    output$wynik4TEXT <- renderText({
      reactive5()
    })
    
}


# Uruchom aplikację Shiny
shinyApp(ui = ui, server = server)