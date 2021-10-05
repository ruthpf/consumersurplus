rm(list=ls())

library(shiny)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(jsonlite)



#Using rdpdf
tablecs <- Table[which(as.numeric(Table$QID)>73),c(1:8)]

value <- suppressWarnings(parse_number(as.character(tablecs$A)))
value <- value[complete.cases(value)]

product <- c(rep("Google Search",9), rep("YouTube",9), rep("Google Maps", 9), rep("Google Docs", 9), rep("Smartphone", 9), rep("Gmail", 9), rep("Google Switch", 9))
#Maybe grepl keep and access and if it doesn't work use a shiny app
Accept <- tablecs$Total[grepl("get paid", tablecs$A)] 
Keep <- tablecs$Total[grepl("keep", tablecs$A)] 
DK <- tablecs$Total[grepl("Don’t", tablecs$A)] 
datacs <- data.frame("Product" = product, "Value"= value, "Give up"= Accept, "Keep"= Keep, "DK"= DK)


#Exclude Don't Know results
datacs$`Give.up` = datacs$`Give.up`/(1-datacs$`DK`)
datacs$`Keep` = datacs$`Keep`/(1-datacs$`DK`)
datacs = select(datacs, -`DK`)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Consumer Surplus Graphics"),
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
      tags$b("Data:"),
      selectInput("Product", "Product",choices = unique(datacs$Product), selected="Google Search", multiple=FALSE ),
      textInput("x", "x", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
      textInput("y", "y" , placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
      hr(),
      tags$b("Plot:"),
      checkboxInput("se", "Add confidence interval around the regression line", TRUE),
      hr(),
      selectInput("formula", "Relationship", choices=c("Linear", "Log"), selected="Linear", multiple=FALSE),
      hr(),
    ),
    
    mainPanel(
      br(),
      tags$b("Compute parameters in R:"),
      verbatimTextOutput("summary"),
      br(),
      tags$b("Regression plot:"),
      uiOutput("results"),
      plotlyOutput("plot"),
      br(),
      tags$b("Interpretation:"),
      uiOutput("interpretation"),
      br(),
      br()
    )
  )
)

server <- function(input, output, session) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  observe({
    product <- input$Product
    filtereddata <- which(datacs$Product==product)
    updateTextInput(session, "x", value = combine_words(datacs$Keep[filtereddata], and=""))
    updateTextInput(session, "y", value = combine_words(datacs$Value[filtereddata], and=""))
  })
  
  # Data output
  
  
  output$data <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
      "Invalid input or not enough observations"
    } else if (length(x) != length(y)) {
      "Number of observations must be equal for x and y"
    } else {
      withMathJax(
        paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
        br(),
        paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
        br(),
        paste0("\\(n =\\) ", length(x))
      )
    }
  })
  
  
  
  output$summary <- renderPrint({
    formula <- input$formula
    if (formula=="Linear"){
      y <- extract(input$y)}
    if (formula=="Log"){
      y <- log(extract(input$y))
    }
    x <- extract(input$x)
    fit <- lm(y ~ x)
    summary(fit)
  })
  
  output$results <- renderUI({
    formula <- input$formula
    x <- extract(input$x)
    
    if (formula=="Linear"){
      y <- extract(input$y)
      fit <- lm(y ~ x)
      cs <- fit$coef[[1]]*fit$coef[[1]]/-fit$coef[[2]]*0.5
      
    }
    if (formula=="Log"){
      y <- log(extract(input$y))
      fit <- lm(y ~ x)
      cs <- exp(fit$coef[[1]]/-fit$coef[[2]])*(-fit$coef[[2]])
    }
    
    withMathJax(
      paste0(
        "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
        ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
        ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3),
        ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3), 
        " , Consumer Surplus (Monthly)",  "\\( = \\) ", enc2utf8("\u00A3"), format(round(cs,2), nsmall=2)
      )
    )
  })
  
  output$interpretation <- renderUI({
    formula <- input$formula
    if (formula=="Linear"){
      y <- extract(input$y)}
    if (formula=="Log"){
      y <- log(extract(input$y))
    }
    x <- extract(input$x)
    fit <- lm(y ~ x)
    if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
        br(),
        paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
      )
    } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
        br(),
        paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
        br(),
        paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$xlab, " and ", input$ylab, ".")
      )
    } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
        br(),
        paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$xlab, " = 0, the mean of ", input$ylab, " is not significantly different from 0."),
        br(),
        paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
      )
    } else {
      withMathJax(
        paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
        br(),
        paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$ylab, " is not significantly different from 0.")
      )
    }
  })
  
  output$plot <- renderPlotly({
    formula <- input$formula
    if (formula=="Linear"){
      y <- extract(input$y)}
    if (formula=="Log"){
      y <- log(extract(input$y))
    }
    x <- extract(input$x)
    fit <- lm(y ~ x)
    dat <- data.frame(x, y)
    p <- ggplot(dat, aes(x = x, y = y)) +
      geom_point() +
      stat_smooth(method = "lm", se = input$se) +
      ylab("Price") +
      xlab("Quantity") +
      theme_minimal()
    ggplotly(p)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)




# Create Consumer Surplus Output Results ----------------------------------

CS <- data.frame()

for (i in 1:length(unique(datacs$Product))){
  
  x = which(datacs$Product==unique(datacs$Product)[i])
#Linear

linear_fit <- lm(datacs$Value[x] ~ datacs$`Keep`[x])
linear_cs <- linear_fit$coef[[1]]*linear_fit$coef[[1]]/-linear_fit$coef[[2]]*0.5


#Log
log_fit <- lm(log(datacs$Value[x]) ~ datacs$`Keep`[x])
log_cs <- exp(log_fit$coef[[1]]/-log_fit$coef[[2]])*(-log_fit$coef[[2]])

Median <- exp((log_fit$coef[[2]]*0.5)+log_fit$coef[[1]])

#Save as dataframe

CS <- bind_rows(CS, data.frame("Product"= unique(datacs$Product)[i], "Linear"=linear_cs, "Log"=log_cs, "Median"= Median))
}
Country =  strsplit(strsplit(getwd(), "/Consumer")[[1]], "Google EMEA 2021/")[[1]][2]
CS$Country = Country

#Save in the Country Consumer Folder
save(CS, file="Consumer Surplus.Rdata")
write.csv(CS, file="Consumer Surplus.csv")


#Save the consumer surplus in the database

setwd(str_replace_all(paste(Sys.getenv(x = "USERPROFILE"),"Public First Dropbox/Policy and Research Team/Polling/Client Tables/Google EMEA 2021/0_Countries Database", sep="/"), "\\\\", "/"))
load("CS Compiled.Rdata")
CS_Compiled <- CS_Compiled[-which(CS_Compiled$Country==Country),]
CS_Compiled <- bind_rows(CS_Compiled, CS)
save(CS_Compiled, file="Consumer Surplus Compiled.Rdata")
write.csv(CS_Compiled, file="Consumer Surplus Compiled.csv")



