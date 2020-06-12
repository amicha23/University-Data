# Load libraries so they are available
library(shiny)
library(leaflet)

# Load in states table so that it can be used later in calculations
states <- list(
  "Alabama" = "AL",
  "Alaska" = "AK",
  "Arizona" = "AZ",
  "Arkansas" = "AR",
  "California" = "CA",
  "Colorado" = "CO",
  "Connecticut" = "CT",
  "Delaware" = "DE",
  "Washington D.C." = "DC",
  "Florida" = "FL",
  "Georgia" = "GA",
  "Hawaii" = "HI",
  "Idaho" = "ID",
  "Illinois" = "IL",
  "Indiana" = "IN",
  "Iowa" = "IA",
  "Kansas" = "KS",
  "Kentucky" = "KY",
  "Louisiana" = "LA",
  "Maine" = "ME",
  "Maryland" = "MD",
  "Massachusetts" = "MA",
  "Michigan" = "MI",
  "Minnesota" = "MN",
  "Missouri" = "MO",
  "Mississippi" = "MS",
  "Montana" = "MT",
  "Nebraska" = "NE",
  "Nevada" = "NV",
  "New Hampshire" = "NH",
  "New Jersey" = "NJ",
  "New Mexico" = "NM",
  "New York" = "NY",
  "North Carolina" = "NC",
  "North Dakota" = "ND",
  "Ohio" = "OH",
  "Oklahoma" = "OK",
  "Oregon" = "OR",
  "Pennsylvania" = "PA",
  "Rhode Island" = "RI",
  "South Carolina" = "SC",
  "South Dakota" = "SD",
  "Tennessee" = "TN",
  "Texas" = "TX",
  "Utah" = "UT",
  "Virginia" = "VA",
  "Vermont" = "VT",
  "Washington" = "WA",
  "West Virginia" = "WV",
  "Wisconsin" = "WI",
  "Wyoming" = "WY"
)

# Loading in the necessary dataframes.
df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
  stringsAsFactors = FALSE
)

# Joining data frames based on name
join_result <- right_join(df1, df3, by = c("Name" = "INSTNM"))
join_result2 <- left_join(
  join_result,
  df2,
  by =
    c(
      "Total.price.for.in-state.students.living.on.campus.2013-14"
      = "tuition"
    )
)

# Table with tution averages per state.
tuition_table <- join_result2 %>%
  select(
    Name = "Name",
    State = "STABBR",
    In_State = "Total.price.for.in-state.students.living.on.campus.2013-14",
    Out_of_State =
      "Total.price.for.out-of-state.students.living.on.campus.2013-14"
  ) %>%
  group_by(State) %>%
  summarize(
    In_state = mean(In_State, na.rm = T),
    Out_of_State = mean(Out_of_State, na.rm = T)
  ) %>%
  arrange(-In_state)

# Remove na values from tuition table
final_tuition_table <- na.omit(tuition_table)

# Scatter plot from tuition table
scatter_plot <- tabPanel(
  "Tuition",
  titlePanel("Average Tuition per State"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "yaxis",
        label = "Tuition",
        choices = list(
          "In State" = "In_state",
          "Out of State" = "Out_of_State"
        ),
        selected = "In_state"
      ),
    ),
    mainPanel(
      h1("University Tuition"),
      p("This chart illustrates the average in-state tuition and
        average out-of-state tuition per state"),
      plotlyOutput(outputId = "scatter_plot"),
      p("This graph illustrates the average tuition of all
      the universities in each state by in-state and out-of-state tuition.
      The scatter plot can help advise students to look
      into universities by state based on tuition price.
      In this way, students can narrow
      their college selection by observing which states,
      on average, hold universities with
      higher or lower tuition statements. Washington D.C.,
      Connecticut, Massachusetts, and Rhode Island
      average the highest costs for in-state
      and out-of-state tuition around 45k-50k. Based on lowest
      average in-state tuition by state, Wyoming and
      North Dakota are the cheapest nearing 20k. Based on lowest
      average out-of-state tution, North Dakota,
      Missouri, and South Dakota have the lowest
      costs pricing around 25k.")
    )
  )
)

# Map with all universities in the United States
map_panel <- mainPanel(
  leafletOutput("map")
)

map_sidebar_content <- sidebarPanel(
  selectInput(
    "state",
    label = "Universities in:",
    choices = states,
    selected = list("Washington" = "WA")
  )
)

map <- tabPanel(
  "Map",
  titlePanel("Map of All Universities in the United States"),
  map_sidebar_content,
  map_panel
)


# Academics Information Panel
sidebar_content <- sidebarPanel(
  selectInput(
    "academicInput",
    label = "Choose a State Whose Academic Performance You Want To See",
    choices = states,
    selected = list("Washington" = "WA")
  )
)

# Graphs for academic information.
academic_graph <- mainPanel(
  plotlyOutput("gpa_graph"),
  plotlyOutput("sat_graph"),
  plotlyOutput("act_graph"),
  plotlyOutput("acceptance_rate_graph"),
  plotlyOutput("ranking_graph"),
  textOutput("summary")
)

academic <- tabPanel(
  "Academics",
  h1("Academics"),
  p("These charts illustrate the average academic performance for
    each school in a given state"),
  sidebarLayout(
    sidebar_content,
    academic_graph
  )
)

# Ethnicity Tab
div_content <- sidebarPanel(
  selectInput(
    "diversityInput",
    label = "Choose which state you want to see the diversity statistics",
    choices = states,
    selected = list("Washington" = "WA")
  )
)

# Pie chart for ethnicity information.
pie_chart <- mainPanel(
  plotlyOutput("pie_chart")
)

ethnicity <- tabPanel(
  "Diversity Breakdown",
  h1("Ethnic Diversity Breakdown"),
  p("This chart represent average ratio of ethnicity in
    all university in that states."),
  sidebarLayout(div_content, pie_chart),
  textOutput(outputId = "ethnicity_summary")
)

# all uni summary table
summary_states <- tabPanel(
  "Summary Table of University Statistics",
  DT::dataTableOutput("summarystates")
)


ui <- navbarPage(
  theme = "myapp.css",
  inverse = TRUE,
  # application title
  "University Statistics in the US",
  # introduction page of the application
  tabPanel(
    "Introduction",
    mainPanel(uiOutput("introduction"))
  ),
  # Tuition Scatter Plot
  scatter_plot,
  # Map of all universities in the US
  map,
  # Ethnicity information
  ethnicity,
  # Academic Information
  academic,
  # Summary of all the universities in each state
  summary_states,
  tabPanel(
    "Major Takeaways",
    mainPanel(uiOutput("takeaways"))
  )
)
