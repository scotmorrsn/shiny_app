library(tidyverse)
library(shiny)
library(gt)
library(ggridges)

## Imperial App - The goals of this shiny app
## 1. Allow anyone to enter data (SI or Standard) and get a torque output
## 2. Visualize the distribution of probable scores based on estimated variance of measurement
## 3. Visualize symmetry and BW normalized absolute torques vs norms to help with decision making

# Define UI for application that calculates torque and plots likely distribution
ui <- fluidPage(
  
  ##Title of App
  headerPanel("Knee Extension Torque Test with Estimate of Measurement Uncertainty"),
  
  # Sidebar with input for data
  sidebarLayout(
    sidebarPanel(
      
      ## note to user
      p("This app takes your test data & estimates the joint moment with estimated uncertainty displayed a few ways.
Recent updates include an estimate of the liklihood that the observed score meets the stated target which is also now adjustable - Scot"
      ),
      ## note to user
      p("Test Goal = torque measurement you select based on goals"),
      
      # Add UI element to toggle between lbs and kgs
      radioButtons(inputId = "unitToggle",
                   label = "Select units of Measurement",
                   choices = c("lbs", "kgs"),
                   selected = "lbs"),
      
      ## user inputs
      numericInput(inputId = "bodymass", value = 200, label = "Body Mass in selected units"),
      numericInput(inputId = "ma", value = 0.3, label = "Moment Arm ALWAYS in meters (Joint line to dynomometer)") ,
      numericInput(inputId = "involved", value = 22, label = "Tested Force on the Involved") ,
      numericInput(inputId = "uninvolved", value = 220, label = "Tested Force on the Uninvolved"),
      actionButton("run", "Run Calculation"),
      tags$hr(),
      numericInput(inputId = "target", value = 2.99, label = "Adjust for different Goal (Knee default 2.99Nm/Kg sd = 0.195) Can be adjusted for other goals such as 2.07 Nm/Kg for knee power estimates per Graham et al 2023. This will update the estimate and goal weight")),
    
    
    
    # user outputs
    mainPanel(
      gt_output("table"),
      br(),
      br(),
      plotOutput("Plot1"),
      plotOutput("Plot2"),
      
      
      
      ## note to user
      p("A  SEM of 0.14 (calculated from data in study under review) for a rigidly fixated, in line dynamometer tested at 60deg was used. Sample SEM, SD and other estimates are based on data from that sample as well. You can get an estimate for other joints since torque is torque but the estimates will be less likely to represent the true score.
            The 2.99Nm/Kg goal is a bootstrapped distribution based on 9 papers that reported knee extension torque in a variety of field and court sports. Represented sports are basketball, soccer, volleyball, rugby league, and handball.
Schons et al., 2019, Tsai et al., 2019, Balasas et al., 2017, Comfort et al., 2011, Scanlan et al., 2018 Gorostiaga et al., 1999, Carvalho et al., 2014, Risberg et al., 2018, Risberg et al., 2018")
      
      
    )
    
  )
)

server <- function(input, output) {
  
  convert_mass <- function(bodymass, to_metric) { ## to_metric = input in lbs. This function puts out mass as Kg either way 
    if (to_metric) {
      return(bodymass/ 2.2) # lbs to Kg
    } else {
      return(bodymass) # mass as Kg entered 
    }
  }
  
  convert_involved <- function(involved, to_metric) { ## to_metric = input in lbs. This function puts out force as N either way
    if (to_metric) {
      return(involved * 4.44822) # pounds-force to newtons
    } else {
      return(involved * 9.80665) # Kg to newtons
    }
  }
  
  convert_uninvolved <- function(uninvolved, to_metric) {
    if (to_metric) {
      return(uninvolved * 4.44822) # pounds-force to newtons
    } else {
      return(uninvolved * 9.80665) # Kg to newtons
    }
  }
  
  
  dat <- reactive({
    
    req(input$run)
    
    to_metric <- input$unitToggle == "lbs"
    
    uninvolved <- convert_uninvolved(input$uninvolved, to_metric)
    involved <- convert_involved(input$involved, to_metric)
    bodymass <- convert_mass(input$bodymass, to_metric)
    ma <- input$ma
    
    ## create reactive inputs based on bodymass
    involved_torque <- ((involved * ma ) / bodymass) ## Gives Nm/Kg
    uninvolved_torque <- ((uninvolved * ma ) / bodymass)
    
    tibble(
      Involved_Nm_Kg = involved_torque,
      Uninvolved_Nm_Kg = uninvolved_torque,
      Test_Goal = ((input$target /(4.4482216153 * input$ma)) * bodymass),
      LSI_Percentage = (involved_torque / uninvolved_torque)
    )
    
  })
  
  
  ## set reactive button
  Prob_met_goal <- reactiveVal(NULL)
  
  ##reactive 2
  posterior_sim <- reactive({
    req(input$run)
    to_metric <- input$unitToggle == "lbs"
    involved <- convert_involved(input$involved, to_metric)
    bodymass <- convert_mass(input$bodymass, to_metric)
    
    
    involved_torque2 <- ((involved * input$ma ) / bodymass) ## Gives Nm/Kg
    posterior_mu <- .92 * involved_torque2 + (1 - .92) * input$target ## Gives posterior mean
    posterior_sd <- sqrt((.92 * (0.1950984^2)/1))
    
    set.seed(9)
    rnorm(n = 1e5, mean = posterior_mu, sd = posterior_sd)
  })
  
  Prob_met_goal <- reactive({
    paste0(round(sum((posterior_sim() < 2.99) / 1e5)*100, 2))
  })
  
  
  
  
  ## table output
  output$table <- render_gt({
    
    dat() %>%
      gt() %>%
      fmt_number(columns = Test_Goal,
                 decimals = 1) %>%
      fmt_number(columns = Involved_Nm_Kg:Uninvolved_Nm_Kg,
                 decimals = 2) %>%
      fmt_percent(columns = LSI_Percentage,
                  decimals = 1) %>%
      
      cols_label(
        Involved_Nm_Kg = md("**Involved (Nm/kg)**"),
        Uninvolved_Nm_Kg = md("**Uninvolved (Nm/kg)**"),
        LSI_Percentage = md("**LSI%**"),
        Test_Goal = md("**>50% Pass Confidence Goal**"),
      )  %>%
      tab_header(title = md("**Limb Symmetry Index**")) %>%
      opt_align_table_header(align = "left") %>%
      tab_options(column_labels.border.top.color = "white",
                  column_labels.border.top.width = px(3),
                  table.border.top.color = "white",
                  table.border.bottom.color = "black") %>%
      cols_align(align = "center")
    
  })
  
  
  
  ## plot output
  output$Plot1 <- renderPlot({
    
    #runif(input$bodymass)
    
    # number of simulations
    N <- 1000
    
    # population mean & SD
    pop_mean <- input$target
    pop_sd <- 0.1950984
    
    # Random draws into a data frame
    knee_dat <- data.frame(
      in_knee = (rnorm(n = N, mean = dat()$Involved_Nm_Kg, sd = pop_sd)),
      un_knee = (rnorm(n = N, mean = dat()$Uninvolved_Nm_Kg, sd = pop_sd)),
      goal_moment = rnorm(n = N, mean = pop_mean, sd = pop_sd)
    )
    
    # covert to long data frame
    knee_long <- knee_dat %>%
      pivot_longer(cols = in_knee:un_knee) %>%
      mutate(mean_diff = mean(goal_moment),
             maxlower = mean_diff - 1.96 * sd(goal_moment),
             maxupper = mean_diff + 1.96 * sd(goal_moment),
             sd_test = sd(goal_moment),
             name = case_when(name == "in_knee" ~ "Injured",
                              name == "un_knee" ~ "Uninjured"))
    
    ## ready to plot!
    knee_long %>%
      ggplot(aes(x = value, y = name, fill = name)) +
      stat_density_ridges(quantile_lines = FALSE, alpha = 0.75, na.rm = TRUE) +
      geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
      geom_vline(aes(xintercept = mean_diff),
                 color = "black",
                 size = 1.2,
                 alpha = .5)+
      geom_vline(aes(xintercept = maxlower),
                 color = "red",
                 linetype = "dashed",
                 size = 1.2,
                 alpha = 0.5) +
      geom_vline(aes(xintercept = maxupper),
                 color = "red",
                 linetype = "dashed",
                 size = 1.2,
                 alpha = 0.5) +
      stat_summary(fun = mean,
                   colour="black",
                   size= 0.2) +
      labs(
        title = "Joint Moment: Injured vs Non",
        subtitle = "Normalized to Body Mass",
        y = (NULL),
        x = "Internal Knee Extension Moment (Nm/Kg)"
      ) +
      theme_classic() +
      theme(legend.position="none",
            axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 12, face = "bold"),
            plot.subtitle = element_text(size = 16, face = "bold"),
            plot.title = element_text(size = 18, face = "bold"))
  })
  
  
  
  output$Plot2 <- renderPlot({
    
    #runif(input$bodymass)
    
    #req(dat())
    
    par(mar = c(5, 5, 5, 2))
    
    par(cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
    
    hist(posterior_sim(),
         main = "", # Remove the default title
         xlab = "Distribution of possible 'True Score' (Nm/Kg)",
         ylab = "Simulated Score Frequency")
    mtext(side = 3, line = 0.5, # Place the text at the top
          paste0(Prob_met_goal(), "% probability still low"),
          adj = 0, # Left-justified
          cex = 1.5,# Font size, adjust as needed
          font = 2,
          col = "red") # Font color, change to your desired color
    abline(v = input$target,
           col = "red",
           lwd = 4,
           lty = 2)
  })
  
  
  
  
  
}

shinyApp(ui, server)



# 
# library(tidyverse)
# library(shiny)
# library(gt)
# library(ggridges)
# library(webshot2)
# # library(htmlwidgets)
# # library(htmltools)
# 
# ## Imperial App - The goals of this shiny app
# ## 1. Allow anyone to enter data (SI or Standard) and get a torque output
# ## 2. Visualize the distribution of probable scores based on estimated variance of measurement
# ## 3. Visualize symmetry and BW normalized absolute torques vs norms to help with decision making
# 
# # Define UI for application that calculates torque and plots likely distribution
# ui <- fluidPage(
# 
#   ##Title of App
#   headerPanel("Knee Extension Torque Test with Estimate of Measurement Uncertainty"),
# 
#   # Sidebar with input for data
#   sidebarLayout(
#     sidebarPanel(
# 
#       ## note to user
#       p("This app takes your test data & estimates the joint moment with estimated uncertainty displayed a few ways.
# Recent updates include an estimate of the liklihood that the observed score meets the stated target which is also now adjustable - Scot"
#       ),
#       ## note to user
#       p("Test Goal = torque measurement you select based on goals"),
# 
#       # Add UI element to toggle between lbs and kgs
#       radioButtons(inputId = "unitToggle",
#                    label = "Select units of Measurement",
#                    choices = c("lbs", "kgs"),
#                    selected = "lbs"),
# 
#       ## user inputs
#       numericInput(inputId = "bodymass", value = 200, label = "Body Mass in selected units"),
#       numericInput(inputId = "ma", value = 0.3, label = "Moment Arm ALWAYS in meters (Joint line to dynomometer)") ,
#       numericInput(inputId = "involved", value = 22, label = "Tested Force on the Involved") ,
#       numericInput(inputId = "uninvolved", value = 220, label = "Tested Force on the Uninvolved"),
#       actionButton("run", "Run Calculation"),
#       tags$hr(),
#       numericInput(inputId = "target", value = 2.99, label = "Adjust for different Goal (Knee default 2.99Nm/Kg sd = 0.195) Can be adjusted for other goals such as 2.07 Nm/Kg for knee power estimates per Graham et al 2023. This will update the estimate and goal weight")),
# 
#     
# 
#     # user outputs
#     # mainPanel(
#     #   gt_output("table"),
#     #   br(),
#     #   br(),
#     #   plotOutput("Plot1"),
#     #   plotOutput("Plot2"),
#     #   downloadButton("download_pdf", "Download Report as PDF"),
# 
#     mainPanel(
#       uiOutput("table_ui"),
#       br(),
#       br(),
#       uiOutput("Plot1_ui"),
#       uiOutput("Plot2_ui"),
#       # Add the download button here
#       downloadButton("download_pdf", "Download Report as PDF"),
# 
# 
#       ## note to user
#       p("A  SEM of 0.14 (calculated from data in study under review) for a rigidly fixated, in line dynamometer tested at 60deg was used. Sample SEM, SD and other estimates are based on data from that sample as well. You can get an estimate for other joints since torque is torque but the estimates will be less likely to represent the true score.
#             The 2.99Nm/Kg goal is a bootstrapped distribution based on 9 papers that reported knee extension torque in a variety of field and court sports. Represented sports are basketball, soccer, volleyball, rugby league, and handball.
# Schons et al., 2019, Tsai et al., 2019, Balasas et al., 2017, Comfort et al., 2011, Scanlan et al., 2018 Gorostiaga et al., 1999, Carvalho et al., 2014, Risberg et al., 2018, Risberg et al., 2018")
# 
# 
#     )
# 
#   )
# )
# 
# server <- function(input, output) {
#   
#   convert_mass <- function(bodymass, to_metric) { ## to_metric = input in lbs. This function puts out mass as Kg either way 
#     if (to_metric) {
#       return(bodymass/ 2.2) # lbs to Kg
#     } else {
#       return(bodymass) # mass as Kg entered 
#     }
#   }
#   
#   convert_involved <- function(involved, to_metric) { ## to_metric = input in lbs. This function puts out force as N either way
#     if (to_metric) {
#       return(involved * 4.44822) # pounds-force to newtons
#     } else {
#       return(involved * 9.80665) # Kg to newtons
#     }
#   }
#   
#   convert_uninvolved <- function(uninvolved, to_metric) {
#     if (to_metric) {
#       return(uninvolved * 4.44822) # pounds-force to newtons
#     } else {
#       return(uninvolved * 9.80665) # Kg to newtons
#     }
#   }
#   
#   
#   dat <- reactive({
#     
#     req(input$run)
#     
#     to_metric <- input$unitToggle == "lbs"
#     
#     uninvolved <- convert_uninvolved(input$uninvolved, to_metric)
#     involved <- convert_involved(input$involved, to_metric)
#     bodymass <- convert_mass(input$bodymass, to_metric)
#     ma <- input$ma
#     
#     ## create reactive inputs based on bodymass
#     involved_torque <- ((involved * ma ) / bodymass) ## Gives Nm/Kg
#     uninvolved_torque <- ((uninvolved * ma ) / bodymass)
#     
#     tibble(
#       Involved_Nm_Kg = involved_torque,
#       Uninvolved_Nm_Kg = uninvolved_torque,
#       Test_Goal = ((input$target /(4.4482216153 * input$ma)) * bodymass),
#       LSI_Percentage = (involved_torque / uninvolved_torque)
#     )
#     
#   })
#   
# 
#   ## set reactive button
#   Prob_met_goal <- reactiveVal(NULL)
# 
#   ##reactive 2
#   posterior_sim <- reactive({
#     req(input$run)
#     to_metric <- input$unitToggle == "lbs"
#     involved <- convert_involved(input$involved, to_metric)
#     bodymass <- convert_mass(input$bodymass, to_metric)
#     
# 
#     involved_torque2 <- ((involved * input$ma ) / bodymass) ## Gives Nm/Kg
#     posterior_mu <- .92 * involved_torque2 + (1 - .92) * input$target ## Gives posterior mean
#     posterior_sd <- sqrt((.92 * (0.1950984^2)/1))
# 
#     set.seed(9)
#     rnorm(n = 1e5, mean = posterior_mu, sd = posterior_sd)
#   })
# 
#   Prob_met_goal <- reactive({
#     paste0(round(sum((posterior_sim() < 2.99) / 1e5)*100, 2))
#   })
# 
#  
# 
# 
#   ## table output
#   output$table <- render_gt({
# 
#     dat() %>%
#       gt() %>%
#       fmt_number(columns = Test_Goal,
#                  decimals = 1) %>%
#       fmt_number(columns = Involved_Nm_Kg:Uninvolved_Nm_Kg,
#                  decimals = 2) %>%
#       fmt_percent(columns = LSI_Percentage,
#                   decimals = 1) %>%
# 
#       cols_label(
#         Involved_Nm_Kg = md("**Involved (Nm/kg)**"),
#         Uninvolved_Nm_Kg = md("**Uninvolved (Nm/kg)**"),
#         LSI_Percentage = md("**LSI%**"),
#         Test_Goal = md("**>50% Pass Confidence Goal**"),
#       )  %>%
#       tab_header(title = md("**Limb Symmetry Index**")) %>%
#       opt_align_table_header(align = "left") %>%
#       tab_options(column_labels.border.top.color = "white",
#                   column_labels.border.top.width = px(3),
#                   table.border.top.color = "white",
#                   table.border.bottom.color = "black") %>%
#       cols_align(align = "center")
# 
#   })
# 
# 
# 
#   ## plot output
#   output$Plot1 <- renderPlot({
#     
#     #runif(input$bodymass)
# 
#     # number of simulations
#     N <- 1000
# 
#     # population mean & SD
#     pop_mean <- input$target
#     pop_sd <- 0.1950984
# 
#     # Random draws into a data frame
#     knee_dat <- data.frame(
#       in_knee = (rnorm(n = N, mean = dat()$Involved_Nm_Kg, sd = pop_sd)),
#       un_knee = (rnorm(n = N, mean = dat()$Uninvolved_Nm_Kg, sd = pop_sd)),
#       goal_moment = rnorm(n = N, mean = pop_mean, sd = pop_sd)
#     )
# 
#     # covert to long data frame
#     knee_long <- knee_dat %>%
#       pivot_longer(cols = in_knee:un_knee) %>%
#       mutate(mean_diff = mean(goal_moment),
#              maxlower = mean_diff - 1.96 * sd(goal_moment),
#              maxupper = mean_diff + 1.96 * sd(goal_moment),
#              sd_test = sd(goal_moment),
#              name = case_when(name == "in_knee" ~ "Injured",
#                               name == "un_knee" ~ "Uninjured"))
# 
#     ## ready to plot!
#     knee_long %>%
#       ggplot(aes(x = value, y = name, fill = name)) +
#       stat_density_ridges(quantile_lines = FALSE, alpha = 0.75, na.rm = TRUE) +
#       geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
#       geom_vline(aes(xintercept = mean_diff),
#                  color = "black",
#                  size = 1.2,
#                  alpha = .5)+
#       geom_vline(aes(xintercept = maxlower),
#                  color = "red",
#                  linetype = "dashed",
#                  size = 1.2,
#                  alpha = 0.5) +
#       geom_vline(aes(xintercept = maxupper),
#                  color = "red",
#                  linetype = "dashed",
#                  size = 1.2,
#                  alpha = 0.5) +
#       stat_summary(fun = mean,
#                    colour="black",
#                    size= 0.2) +
#       labs(
#         title = "Joint Moment: Injured vs Non",
#         subtitle = "Normalized to Body Mass",
#         y = (NULL),
#         x = "Internal Knee Extension Moment (Nm/Kg)"
#       ) +
#       theme_classic() +
#       theme(legend.position="none",
#             axis.text = element_text(size = 12, face = "bold"),
#             axis.title = element_text(size = 12, face = "bold"),
#             plot.subtitle = element_text(size = 16, face = "bold"),
#             plot.title = element_text(size = 18, face = "bold"))
#   })
# 
# 
# 
#   output$Plot2 <- renderPlot({
# 
#     par(mar = c(5, 5, 5, 2))
# 
#     par(cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
# 
#     hist(posterior_sim(),
#          main = "", # Remove the default title
#          xlab = "Distribution of possible 'True Score' (Nm/Kg)",
#          ylab = "Simulated Score Frequency")
#     mtext(side = 3, line = 0.5, # Place the text at the top
#           paste0(Prob_met_goal(), "% probability still low"),
#           adj = 0, # Left-justified
#           cex = 1.5,# Font size, adjust as needed
#           font = 2,
#           col = "red") # Font color, change to your desired color
#     abline(v = input$target,
#            col = "red",
#            lwd = 4,
#            lty = 2)
#   })
#   
# 
# }
# 
# shinyApp(ui, server)