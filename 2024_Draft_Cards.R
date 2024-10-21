library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magick)
library(tidyverse)
library(scales)
library(caret)
library(randomForest)
library(glmnet)
library(boot)
library(Metrics)
library(gridExtra)
library(xgboost)
library(readxl)
library(grid)
library(rsconnect)
library(rsvg)

# Load the data
scouting_data <- read_csv("2024_MLB_Draft.csv") %>%
  rename(
    Drafted_Or_College = `Drafted/College`,
    Command = Commandp  # Rename Commandp to Command
  ) %>%
  filter(!is.na(College))

slots <- read_csv("2024_Draftslots.csv")

# Define team logos globally
team_logos <- list(
  "AZ" = "https://www.mlbstatic.com/team-logos/109.svg",
  "ATL" = "https://www.mlbstatic.com/team-logos/144.svg",
  "BAL" = "https://www.mlbstatic.com/team-logos/110.svg",
  "BOS" = "https://www.mlbstatic.com/team-logos/111.svg",
  "CWS" = "https://www.mlbstatic.com/team-logos/145.svg",
  "CHC" = "https://www.mlbstatic.com/team-logos/112.svg",
  "CIN" = "https://www.mlbstatic.com/team-logos/113.svg",
  "CLE" = "https://www.mlbstatic.com/team-logos/114.svg",
  "COL" = "https://www.mlbstatic.com/team-logos/115.svg",
  "DET" = "https://www.mlbstatic.com/team-logos/116.svg",
  "HOU" = "https://www.mlbstatic.com/team-logos/117.svg",
  "KC" = "https://www.mlbstatic.com/team-logos/118.svg",
  "LAA" = "https://www.mlbstatic.com/team-logos/108.svg",
  "LAD" = "https://www.mlbstatic.com/team-logos/119.svg",
  "MIA" = "https://www.mlbstatic.com/team-logos/146.svg",
  "MIL" = "https://www.mlbstatic.com/team-logos/158.svg",
  "MIN" = "https://www.mlbstatic.com/team-logos/142.svg",
  "NYY" = "https://www.mlbstatic.com/team-logos/147.svg",
  "NYM" = "https://www.mlbstatic.com/team-logos/121.svg",
  "OAK" = "https://www.mlbstatic.com/team-logos/133.svg",
  "PHI" = "https://www.mlbstatic.com/team-logos/143.svg",
  "PIT" = "https://www.mlbstatic.com/team-logos/134.svg",
  "SD" = "https://www.mlbstatic.com/team-logos/135.svg",
  "SF" = "https://www.mlbstatic.com/team-logos/137.svg",
  "SEA" = "https://www.mlbstatic.com/team-logos/136.svg",
  "STL" = "https://www.mlbstatic.com/team-logos/138.svg",
  "TB" = "https://www.mlbstatic.com/team-logos/139.svg",
  "TEX" = "https://www.mlbstatic.com/team-logos/140.svg",
  "TOR" = "https://www.mlbstatic.com/team-logos/141.svg",
  "WSH" = "https://www.mlbstatic.com/team-logos/120.svg"
)

# Function to create player card plot
create_player_card <- function(player_name, team_abbr, pick_number) {
  player_data <- scouting_data %>%
    filter(Name == player_name)
  
  if (nrow(player_data) == 0) {
    stop("Player not found.")
  }
  
  slot_number <- slots %>%
    filter(Pick == pick_number) %>%
    pull(Slot)
  
  team_logo_url <- team_logos[[team_abbr]]
  
  # Check if player is a pitcher
  is_pitcher <- "Position" %in% colnames(player_data) &&
    (player_data$Position == "SP" || player_data$Position == "RP")
  
  # Reshape the data for plotting
  if (is_pitcher) {
    player_data <- player_data %>%
      select(FB, FBf, `2nd`, `2ndf`, `3rd`, `3rdf`, `4th`, `4thf`, `5th`,
             `5thf`, Command, Commandf, Velos) %>%
      pivot_longer(cols = -Velos, names_to = c("skill", "type"),
                   names_pattern = "(.*?)(f?)$", values_to = "grade") %>%
      mutate(type = ifelse(type == "", "Current", "Future"))
  } else {
    player_data <- player_data %>%
      select(Contact, Contactf, `G Power`, `G Powerf`, `R Power`, `R Powerf`,
             Speed, Speedf, Field, Fieldf, Arm) %>%
      pivot_longer(cols = everything(), names_to = c("skill", "type"),
                   names_pattern = "(.*?)(f?)$", values_to = "grade") %>%
      mutate(type = ifelse(type == "", "Current", "Future"))
  }
  
  # Convert grade to numeric and handle NAs
  player_data$grade <- as.numeric(player_data$grade)
  player_data <- player_data %>%
    filter(!is.na(grade))
  
  # Get team information
  team_info <- scouting_data %>%
    filter(Name == player_name) %>%
    select(all_of(c("Drafted_Or_College", "Age", "Position",
                    "College", "Slot", "ETA", "Height", "Weight",
                    "TLDR", "FV", "Bonus_pred"))) %>%
    distinct()
  
  if (nrow(team_info) == 0) {
    stop("Team information not found.")
  }
  
  player_position <- team_info$Position[1]
  college <- team_info$College[1]
  age <- team_info$Age[1]
  height <- team_info$Height[1]
  weight <- team_info$Weight[1]
  FV <- team_info$FV[1]
  Bonus_pred <- team_info$Bonus_pred[1]
  tldr <- team_info$TLDR[1]
  velos_text <- ifelse(is_pitcher, paste("Velocities:", player_data$Velos[1]), "")
  
  # Convert 'type' to a factor and set the levels to ensure the order
  player_data$type <- factor(player_data$type, levels = c("Current", "Future"))
  
  # Create the grid plot
  plot <- ggplot(player_data, aes(x = skill, y = type, fill = grade)) +
    geom_tile(color = "white", width = 0.9, height = 0.2) +  # Adjust tile size as needed
    scale_fill_gradientn(colors = c("blue", "white", "red"),
                         limits = c(20, 80)) +
    geom_text(aes(label = grade), vjust = 0.5, hjust = 0.5,
              color = "black", size = 6) +
    labs(x = "Skill", y = "Level") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_blank(),  # Removed title from plot
      plot.subtitle = element_blank(),  # Removed subtitle from plot
      plot.caption = element_text(size = 12, hjust = 0, vjust = 0.5),  # Left-align
      legend.position = "none"
    )
  
  # If team logo exists, add it to the plot
  if (!is.null(team_logo_url)) {
    # Load team logo image
    logo <- image_read_svg(team_logo_url)
    
    # Arrange the plot and logo using grid package
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(8, 3, heights = unit(c(0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.3, 0.3), "null"))))  # Adjusted heights
    
    # Player name and subtitle
    grid.text(paste(player_name), vp = viewport(layout.pos.row = 1,
                                                layout.pos.col = 2),
              gp = gpar(fontsize = 16, fontface = "bold"))
    grid.text(paste("Position:", player_position, "| Age:", age,
                    "| College/Commit: ", college,
                    "\nHeight:", height, "| Weight:", weight, "lbs",
                    "\nFV:", FV, "| Expected Slot:", Bonus_pred, "| Slot:", slot_number),
              vp = viewport(layout.pos.row = 2:3, layout.pos.col = 2),
              gp = gpar(fontsize = 12, fontface = "italic"))
    
    # Plot chart
    print(plot, vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
    
    # Plot velocities
    if (velos_text != "") {
      grid.text(velos_text,
                vp = viewport(layout.pos.row = 5, layout.pos.col = 2),
                gp = gpar(fontsize = 12, fontface = "plain"))
    }
    
    # Quick Hit title
    grid.text("Quick Hit:", vp = viewport(layout.pos.row = 6,
                                          layout.pos.col = 2),
              gp = gpar(fontsize = 12, fontface = "bold"))
    
    # Plot TLDR text
    grid.text(label_wrap_gen(width = 100)(tldr),
              vp = viewport(layout.pos.row = 7, layout.pos.col = 2),
              gp = gpar(fontsize = 16, fontface = "plain"))  # Adjust width as needed
    
    # Plot logo with increased size
    grid.raster(logo, vp = viewport(layout.pos.row = 1:3,
                                    layout.pos.col = 3,
                                    height = unit(10, "cm")))  # Increased size
  } else {
    # Arrange the plot without the logo using grid package
    grid.newpage()
    pushViewport(viewport(
      layout = grid.layout(8, 3, heights = unit(c(0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.3, 0.3), "null"))))  # Adjusted heights
    
    # Player name and subtitle
    grid.text(paste(player_name), vp = viewport(layout.pos.row = 1,
                                                layout.pos.col = 2),
              gp = gpar(fontsize = 16, fontface = "bold"))
    grid.text(paste("Position:", player_position, "| Age:", age, 
                    "| College:", college,
                    "\nHeight:", height, "| Weight:", weight, "lbs",
                    "\nFV:", FV, "| Expected Slot:", Bonus_pred, "| Slot:", slot_number),
              vp = viewport(layout.pos.row = 2:3, layout.pos.col = 2),
              gp = gpar(fontsize = 12, fontface = "italic"))
    
    # Plot chart
    print(plot, vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
    
    # Plot velocities
    if (velos_text != "") {
      grid.text(velos_text,
                vp = viewport(layout.pos.row = 5, layout.pos.col = 2),
                gp = gpar(fontsize = 12, fontface = "plain"))
    }
    
    # Quick Hit title
    grid.text("Quick Hit:", vp = viewport(layout.pos.row = 6,
                                          layout.pos.col = 2),
              gp = gpar(fontsize = 12, fontface = "bold"))
    
    # Plot TLDR text
    grid.text(label_wrap_gen(width = 75)(tldr),
              vp = viewport(layout.pos.row = 7, layout.pos.col = 2),
              gp = gpar(fontsize = 16, fontface = "plain"))  # Adjust width as needed
  }
}

# Define UI for application
ui <- fluidPage(
  titlePanel("2024 MLB Draft Scouting Cards"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Player Name:", choices = unique(scouting_data$Name)),
      selectInput("team", "Team Abbreviation:", choices = names(team_logos)),
      numericInput("pick", "Pick Number:", value = 1, min = 1),
      actionButton("submit", "Create Player Card")
    ),
    mainPanel(
      plotOutput("scoutingPlot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    output$scoutingPlot <- renderPlot({
      create_player_card(input$player, input$team, input$pick)
    })
  })
}

shinyApp(ui = ui, server = server)
