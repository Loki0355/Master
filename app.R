#Packages laden
library(shiny)
library(plotly)
library(dplyr)

#Wertebereiche für Reliabilität und Varianz
reliabilität <- c(0.6, 0.7, 0.8, 0.9, 0.95)
varianz <- c(0.1, 0.5, 1)

#Daten generieren
generate_data_base <- function(rel_x, rel_y, var_x, var_y) {
  expand.grid(bias_x = seq(-1, 1, length.out = 50),
              bias_y = seq(-1, 1, length.out = 50)) %>%
    
    #Formeln definieren
    mutate(
      s2_x = (1 - rel_x) * var_x,
      s2_y = (1 - rel_y) * var_y,
      S    = s2_y / (s2_y + s2_x),
      var_bayes = S * s2_x,
      b_bayes   = S * bias_x + (1 - S) * bias_y
    )
}

#Definieren der Modelle
compute_metrics <- function(df, mode = c("diff_self",  "diff_peer",  "diff_mittel",
                                         "cmp_self",   "cmp_peer",   "cmp_mittel")) {
  mode <- match.arg(mode)
  if (mode %in% c("diff_self", "cmp_self")) {
    df <- df %>% mutate(mse_alt = bias_x^2 + s2_x, alt_label = "Selbst", alt_col = "red")
  } else if (mode %in% c("diff_peer", "cmp_peer")) {
    df <- df %>% mutate(mse_alt = bias_y^2 + s2_y, alt_label = "Fremd", alt_col = "forestgreen")
  } else {
    df <- df %>% mutate(b_alt = (bias_x + bias_y)/2,
                        var_alt = (s2_x + s2_y)/4,
                        mse_alt = b_alt^2 + var_alt,
                        alt_label = "Mittelwert", alt_col = "gold")
  }
  df <- df %>% mutate(mse_bayes = b_bayes^2 + var_bayes,
                      diff_mse  = mse_bayes - mse_alt)
  df
}

#Anfangseinstellungen
control_sidebar <- function(ab) {
  tagList(
    selectInput(ab("rel_x"), "Reliabilität X", reliabilität, 0.8),
    selectInput(ab("rel_y"), "Reliabilität Y", reliabilität, 0.8),
    selectInput(ab("var_x"), "Varianz X",     varianz, 0.5),
    selectInput(ab("var_y"), "Varianz Y",     varianz, 0.5)
  )
}

#Title Layout
diff_ui <- function(id, title) {
  ab <- NS(id)
  fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel( control_sidebar(ab) ),
      mainPanel( plotlyOutput(ab("plot"), height = "650px") )
    )
  )
}

#Minimum und Maximum festlegen
BIAS_LIM <- c(-1, 1)        
DIFF_LIM <- c(-1.5, 1.5)    
MSE_LIM  <- c(0, 1.5)


#Server Differenzplots
diff_server <- function(id, mode) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      df <- generate_data_base(as.numeric(input$rel_x), as.numeric(input$rel_y),
                               as.numeric(input$var_x), as.numeric(input$var_y)) |>
        compute_metrics(mode)
      plot_ly(df) %>%
        add_trace(x = ~bias_x, y = ~bias_y, z = ~diff_mse,
                  type = "scatter3d", mode = "markers",
                  marker = list(size = 2, color = ~diff_mse,
                                colorscale = "RdBu",
                                cmin = DIFF_LIM[1], cmax = DIFF_LIM[2], 
                                colorbar = list(title = "ΔMSE")),
                  hovertemplate = "BiasX:%{x:.2f}<br>BiasY:%{y:.2f}<br>ΔMSE:%{z:.3f}<extra></extra>") %>%   
        layout(                                                    
          scene = list(
            xaxis = list(title = "Bias X", range = BIAS_LIM),       
            yaxis = list(title = "Bias Y", range = BIAS_LIM),     
            zaxis = list(title = "ΔMSE", range = DIFF_LIM),
            aspectmode = "cube",
            camera = list(
              eye = list(x = 1.5, y = 1.5, z = 1.5),
              center = list(x = 0, y = 0, z = 0)
            )
          )
        )
    })
  })
}

#Title Layout
compare_ui <- function(id, title) {
  ab <- NS(id)
  fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel( control_sidebar(ab) ),
      mainPanel( plotlyOutput(ab("plot"), height = "650px") )
    )
  )
}

#Vergleichsserver
compare_server <- function(id, mode) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      df <- generate_data_base(as.numeric(input$rel_x), as.numeric(input$rel_y),
                               as.numeric(input$var_x), as.numeric(input$var_y)) |>
        compute_metrics(mode)
      max_mse <- max(df$mse_bayes, df$mse_alt)
      plot_ly(df) %>%
        add_trace(x = ~bias_x, y = ~bias_y, z = ~mse_alt,
                  type = "scatter3d", mode = "markers",
                  name = paste("MSE", unique(df$alt_label)),
                  marker = list(size = 2, color = unique(df$alt_col), opacity = 0.6),
                  hovertemplate = "Bias X:%{x:.2f}<br>Bias Y:%{y:.2f}<br>MSE Alt:%{z:.3f}<extra></extra>") %>%
        add_trace(x = ~bias_x, y = ~bias_y, z = ~mse_bayes,
                  type = "scatter3d", mode = "markers",
                  name = "Bayes‑MSE", marker = list(size = 2, color = "blue"),
                  hovertemplate = "Bias X:%{x:.2f}<br>Bias Y:%{y:.2f}<br>MSE Bayes:%{z:.3f}<extra></extra>") %>%
        add_trace(x = c(0,0), y = c(0,0), z = MSE_LIM,                     
                  type = "scatter3d", mode = "lines",
                  line = list(color = "black", width = 4), showlegend = FALSE) %>%
        layout(                                                          
          scene = list(
            xaxis = list(title = "Bias X", range = BIAS_LIM),
            yaxis = list(title = "Bias Y", range = BIAS_LIM),
            zaxis = list(title = "MSE",    range = MSE_LIM),
            aspectmode = "cube",
            camera = list(
              eye = list(x = 1.5, y = 1.5, z = 1.5),
              center = list(x = 0, y = 0, z = 0)
            )
          )
        )
    })
  })
}

#Panels
ui <- navbarPage(
  "Interaktive MSE‑Vergleiche",
  tabPanel("ΔBayes–Selbst",  diff_ui("diff_self",   "ΔMSE: Bayes–Selbst")),
  tabPanel("ΔBayes–Fremd",   diff_ui("diff_peer",   "ΔMSE: Bayes–Fremd")),
  tabPanel("ΔBayes–Mittel",  diff_ui("diff_mittel", "ΔMSE: Bayes–Mittel")),
  
  tabPanel("Bayes vs. Selbst",   compare_ui("cmp_self",   "Bayes‑MSE vs. Selbst‑MSE")),
  tabPanel("Bayes vs. Fremd",    compare_ui("cmp_peer",   "Bayes‑MSE vs. Fremd‑MSE")),
  tabPanel("Bayes vs. Mittel",   compare_ui("cmp_mittel", "Bayes‑MSE vs. Mittel‑MSE"))
)

#Server
server <- function(input, output, session) {
  diff_server("diff_self",   "diff_self")
  diff_server("diff_peer",   "diff_peer")
  diff_server("diff_mittel", "diff_mittel")
  
  compare_server("cmp_self",   "cmp_self")
  compare_server("cmp_peer",   "cmp_peer")
  compare_server("cmp_mittel", "cmp_mittel")
}

shinyApp(ui, server)
