# === Add required packages ===
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(scales)
library(tidyr)

# === Load & preprocess data ===

# Panel A - Real estate price index
df_price <- read.csv("QCNR628BIS.csv")
df_price <- df_price %>%
  rename(date = observation_date, price_index = QCNR628BIS) %>%
  mutate(date = ymd(date)) %>%
  arrange(date) %>%
  mutate(yoy_change = (price_index / lag(price_index, 4) - 1) * 100)

neg_run <- rle(df_price$yoy_change < 0)
consec_neg <- any(neg_run$values & neg_run$lengths >= 3)

# Panel B - Lending interest rate
df_interest <- read.csv("API_FR.INR.LNDP_DS2_en_csv_v2_91141.csv", skip = 4)
df_china_interest <- df_interest %>%
  filter(Country.Name == "China") %>%
  select(starts_with("X")) %>%
  pivot_longer(cols = everything(), names_to = "year", names_prefix = "X", values_to = "interest_rate") %>%
  mutate(year = as.integer(year),
         interest_rate = as.numeric(interest_rate)) %>%
  filter(!is.na(interest_rate))

# Panel C - Exchange rate
exchange_data <- read.csv("exchange_rate.csv")
exchange_data <- exchange_data %>%
  rename(Date = observation_date, Rate = EXCHUS) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2023-12-31")) %>%
  arrange(Date) %>%
  mutate(Rolling_SD = rollapply(Rate, width = 6, FUN = sd, fill = NA, align = "right"))

# Panel D - Bankruptcy comparison
df_bankrupt <- read.csv("data.csv", check.names = FALSE) %>% filter(`Bankrupt?` == 1)
df_nonbankrupt <- read.csv("data.csv", check.names = FALSE) %>% filter(`Bankrupt?` == 0)

mean_b <- df_bankrupt %>% select(-`Bankrupt?`) %>% summarise(across(everything(), mean, na.rm = TRUE)) %>% t()
mean_nb <- df_nonbankrupt %>% select(-`Bankrupt?`) %>% summarise(across(everything(), mean, na.rm = TRUE)) %>% t()

comparison_df <- data.frame(
  Variable = rownames(mean_b),
  Mean_Bankrupt = mean_b[,1],
  Mean_NonBankrupt = mean_nb[,1]
)

overall_mean <- (comparison_df$Mean_Bankrupt + comparison_df$Mean_NonBankrupt) / 2
comparison_df$is_ratio <- overall_mean < 1
comparison_df$Relative_Diff <- ifelse(
  comparison_df$is_ratio,
  abs(comparison_df$Mean_Bankrupt - comparison_df$Mean_NonBankrupt),
  ifelse(comparison_df$Mean_NonBankrupt == 0, NA, abs((comparison_df$Mean_Bankrupt - comparison_df$Mean_NonBankrupt) / comparison_df$Mean_NonBankrupt))
)
comparison_df_final <- comparison_df %>%
  arrange(desc(Relative_Diff)) %>%
  slice(1:10)

# === UI ===
ui <- fluidPage(
  titlePanel("Country Garden Risk Intelligence Dashboard"),
  sidebarLayout(
    position = "right", 
    conditionalPanel(
      condition = "input.tabs == 'A. Market Demand Tracker' || input.tabs == 'B. Debt & Interest Burden' || input.tabs == 'C. FX Risk Panel' || input.tabs == 'D. Bankruptcy Risk Predictor'",
      sidebarPanel(width = 3,
                   
                   conditionalPanel(
                     condition = "input.tabs == 'A. Market Demand Tracker'",
                     tags$div(
                       style = "color:#b30000; font-weight:bold; background-color:#fff5f5; padding:10px; border-left:5px solid #cc0000; margin-bottom:15px;",
                       textOutput("alert_text")
                     ),
                     tags$hr(),
                     tags$p(
                       "This chart tracks China's residential property price index over time, 
                         highlighting a key turning point in August 2020. The second chart shows 
                         the annual percentage change to visualize growth trends and potential market slowdowns."
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.tabs == 'B. Debt & Interest Burden'",
                     uiOutput("styled_debt_alert"),
                     tags$br(),
                     numericInput("debt_input", "Simulated Debt (in B RMB):", value = 200, min = 0),
                     numericInput("revenue_input", "Simulated Revenue (in B RMB):", value = 50, min = 0),
                     tags$hr(),
                     tags$p(
                       "This line chart shows China's historical lending interest rates. 
                         Input fields allow simulation of debt and revenue levels to assess the 
                         estimated interest burden relative to company income."
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.tabs == 'C. FX Risk Panel'",
                     dateRangeInput("fx_date_range", 
                                    label = "Select Date Range:",
                                    start = min(exchange_data$Date),
                                    end = max(exchange_data$Date),
                                    min = min(exchange_data$Date),
                                    max = max(exchange_data$Date)),
                     tags$hr(),
                     tags$p(
                       "This chart presents the RMB/USD exchange rate alongside 
                         6-month rolling volatility. It helps visualize currency trends and 
                         periods of elevated market uncertainty."
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "input.tabs == 'D. Bankruptcy Risk Predictor'",
                     tags$p(
                       "This bar chart highlights the top 10 financial indicators that differentiate bankrupt from non-bankrupt firms.
                        Among them, Fixed Assets to Assets shows the most significant relative difference, suggesting it is a strong predictor of bankruptcy risk."
                     )
                   )
      ) 
    ), 
    mainPanel(width = 9, 
      tabsetPanel(id = "tabs",
                  tabPanel("Overview",
                           h2("Welcome to the Country Garden Risk Intelligence Dashboard"),
                           p("This dashboard provides interactive insights into key financial and market risk indicators for Country Garden. It consolidates multiple dimensions of financial health and performance, offering real-time analysis based on reliable datasets."),
                           h3("Modules:"),
                           tags$ul(
                             tags$li(tags$b("Market Demand Tracker:"), " Tracks quarterly changes in China's residential property market index."),
                             tags$li(tags$b("Debt & Interest Burden:"), " Estimates interest cost exposure relative to revenue, highlighting potential financial strain."),
                             tags$li(tags$b("FX Risk Panel:"), " Visualizes exchange rate trends and monitors volatility in RMB/USD over time."),
                             tags$li(tags$b("Bankruptcy Risk Indicator:"), " Identifies key financial variables that differentiate bankrupt firms from solvent ones.")
                           ),
                           
                           p("Use the tabs above to navigate through the dashboard and the sidebar to adjust input parameters for a customized view.")
                  ),

                  tabPanel("A. Market Demand Tracker",
                           plotOutput("price_plot", height = "350px"),
                           tags$div(style = "margin-top: 30px;"), 
                           plotOutput("yoy_plot", height = "350px")),
                  
                  tabPanel("B. Debt & Interest Burden",
                           plotOutput("rate_plot")),
                  
                  tabPanel("C. FX Risk Panel",
                           plotOutput("fx_combined_plot", height = "400px")),
                           
                  
                  tabPanel("D. Bankruptcy Risk Predictor",
                           plotOutput("bankrupt_plot"))
      )
    )
  )
)

# === Server ===
custom_plot_theme <- theme_minimal() + 
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12),
  )

server <- function(input, output) {
  
  # Panel A
  output$alert_text <- renderText({
    if (consec_neg) {
      "⚠️ Warning: Property prices declined for 3 or more consecutive quarters."
    } else {
      "✅ Market stable: No extended price decline detected."
    }
  })
  
  output$price_plot <- renderPlot({
    ggplot(df_price, aes(x = date, y = price_index)) +
      geom_line(color = "steelblue") +
      geom_vline(xintercept = as.Date("2020-08-01"), linetype = "dashed", color = "red3") +
      labs(title = "Residential Property Price Index in China", x = "Date", y = "Price Index") +
      custom_plot_theme
  })
  
  output$yoy_plot <- renderPlot({
    ggplot(df_price %>% filter(!is.na(yoy_change)), aes(x = date, y = yoy_change, fill = yoy_change > 0)) +
      geom_col() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "red")) +
      labs(title = "Annual % Change in Property Price Index", x = "Date", y = "YOY Change (%)") +
      guides(fill = "none") +
      custom_plot_theme
  })
  
  # Panel B
  output$styled_debt_alert <- renderUI({
    debt <- input$debt_input
    revenue <- input$revenue_input
    latest_rate <- tail(df_china_interest$interest_rate, 1)
    estimated_cost <- (latest_rate / 100) * debt
    warning <- estimated_cost > (0.10 * revenue)
    
    message <- paste0("Estimated Annual Interest Cost: ", round(estimated_cost, 2), "B RMB — ",
                      ifelse(warning, "⚠️ High risk: exceeds 10% of revenue.", "✅ Within manageable range."))
    
    div(
      style = paste0(
        "font-weight:bold; padding:10px; border-left:5px solid;",
        "background-color:", ifelse(warning, "#fff5f5", "#f5fff5"), ";",
        "border-left-color:", ifelse(warning, "#cc0000", "#009933"), ";",
        "color:", ifelse(warning, "#b30000", "#006600"), ";"
      ),
      message
    )
  })
  
  
  output$rate_plot <- renderPlot({
    ggplot(df_china_interest, aes(x = year, y = interest_rate)) +
      geom_line(color = "darkorange", size = 1.2) +
      geom_point() +
      labs(title = "Lending Interest Rate in China", x = "Year", y = "Interest Rate (%)") +
      custom_plot_theme
  })
  
  # Panel C
  filtered_fx_data <- reactive({
    req(input$fx_date_range)
    exchange_data %>%
      filter(Date >= input$fx_date_range[1],
             Date <= input$fx_date_range[2])
  })
  
  output$fx_combined_plot <- renderPlot({
    ggplot() +
      geom_line(data = exchange_data %>%
                  filter(Date >= input$fx_date_range[1],
                         Date <= input$fx_date_range[2]),
                aes(x = Date, y = Rate, color = "Exchange Rate"), size = 1.2) +
      geom_line(data = exchange_data %>%
                  filter(Date >= input$fx_date_range[1],
                         Date <= input$fx_date_range[2]),
                aes(x = Date, y = Rolling_SD * 30, color = "Volatility"), size = 1.2) +
      scale_y_continuous(
        name = "Exchange Rate",
        sec.axis = sec_axis(~./30, name = "Rolling Std Dev")
      ) +
      scale_color_manual(values = c("Exchange Rate" = "steelblue", "Volatility" = "darkred")) +
      labs(title = "RMB/USD Exchange Rate & 6-Month Volatility",
           x = "Date", color = "") +
      custom_plot_theme +
      theme(
        legend.position = "top"
      )
  })
  
  
  
  # Panel D
  output$bankrupt_plot <- renderPlot({
    comparison_df_final$Variable <- factor(comparison_df_final$Variable,
                                           levels = comparison_df_final$Variable[order(comparison_df_final$Relative_Diff)])
    
    ggplot(comparison_df_final, aes(x = Variable, y = Relative_Diff)) +
      geom_col(fill = "firebrick") +
      geom_text(aes(label = round(Relative_Diff, 1)), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = "Top 10 Bankruptcy Risk Indicators (Log Scale)",
        x = "Financial Metric",
        y = "Relative Difference (log scale)"
      ) +
      scale_y_log10(labels = comma, expand = expansion(mult = c(0, 0.2))) +
      custom_plot_theme
    
  })
  
  output$bankrupt_table <- renderTable({
    comparison_df_final
  })
}

# Run the app
shinyApp(ui = ui, server = server)
