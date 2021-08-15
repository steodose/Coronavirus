##### COVID-19 vaccinations from Sharon Marchilis #####
##### By: Stephan Teodosescu #####

library(tidyverse)
library(ggiraph)
library(patchwork)
library(ggtext)
library(glue)

library(tidycensus)
library(geofacet)
library(zoo)

# Load data from Our World in Data
data_url <- "https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/us_state_vaccinations.csv"

all_data <- read_csv(data_url)
all_data$location[all_data$location == "New York State"] <- "New York" #Recode New York State to New York


##### Custom ggplot theme (inspired by Owen Phillips at the F5) #####
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Identify what is not a state and/or DC
not_states_or_dc <- c("American Samoa", "Bureau of Prisons", 
                      "Dept of Defense", "Federated States of Micronesia", "Guam", 
                      "Indian Health Svc", "Long Term Care", "Marshall Islands", 
                      "Northern Mariana Islands", "Puerto Rico", "Republic of Palau", 
                      "United States", "Veterans Health", "Virgin Islands")

# Get latest date
latest_date <- max(all_data$date) %>% 
    format(format="%B %d")

# Filter for 
bar_graph_data_recent <- all_data %>%  
    filter(date == max(date), !(location %in% not_states_or_dc)) %>%  
    mutate(
        PctFullyVaccinated = round(people_fully_vaccinated_per_hundred, 1)  
    ) %>%  
    select(State = location, PctFullyVaccinated)

# Create basic bar graph
bar_graph <- ggplot(bar_graph_data_recent, 
                    aes(x = reorder(State, PctFullyVaccinated), 
                        y = PctFullyVaccinated)) +
    geom_col(color = "black", fill = ifelse(bar_graph_data_recent$PctFullyVaccinated >= 50.0,
                                            "#045CA4", "grey")) +
    theme_custom() +
    labs(
        x = "", y = "Fully Vaccinated (%)",
        title = "U.S. State Vaccination Status",
        subtitle = glue("Vaccination rates by state, as of
                        **{latest_date}**. Colors represent states \n with
                        <span style = 'color:#045CA4;'>**greater than**</span> vs.
                        <span style = 'color:#999999;'>**less than**</span> 50% of its residents fully vaccinated."),
        caption = "Data: Our World in Data\nGraphic: @steodosescu"
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    coord_flip()

bar_graph #View in the Plot pane in Rstudio
ggsave("US State Vaccination Status.png")

# Create tooltip column
bar_graph_data_recent <- bar_graph_data_recent %>%
    mutate(
        tooltip_text = paste0(toupper(State), "\n", 
                              PctFullyVaccinated, "%")
    )

#Make interactive with girafe() function
latest_vax_graph <- ggplot(bar_graph_data_recent, 
                           aes(x = reorder(State, PctFullyVaccinated), 
                               y = PctFullyVaccinated,
                               tooltip = tooltip_text, data_id = State #<<
                           )) +
    geom_col_interactive(color = "black", fill="#045CA4") +
    theme_custom() +
    labs(
        x = "", y = "Fully Vaccinated (%)",
        title = "Vaccination Status, July 2021",
        subtitle = glue("Hover to see individual state vaccination percentages."),
        caption = "Data: Our World in Data\nGraphic: @steodosescu"
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    coord_flip()

girafe(ggobj = latest_vax_graph, width_svg = 5, height_svg = 4)

# Make bar graph showing vaccinations rates from early on this year.
bar_graph_data_early <- all_data %>%
    filter(date == "2021-02-14", !(location %in% not_states_or_dc)) %>%
    arrange(people_fully_vaccinated_per_hundred) %>%
    mutate(
        PctFullyVaccinated = round(people_fully_vaccinated_per_hundred, 1),
        tooltip_text = paste0(toupper(location), "\n", PctFullyVaccinated, "%")
    ) %>%
    select(State = location, PctFullyVaccinated, tooltip_text)

# Link the two interactive plots
early_vax_graph <- ggplot(bar_graph_data_early, aes(x = reorder(State, PctFullyVaccinated), y = PctFullyVaccinated, tooltip = tooltip_text, data_id = State)) +
    geom_col_interactive(color = "black", fill="#045CA4") +
    theme_custom() +
    labs(
        x = "", y = "Fully Vaccinated (%)",
        title = "Vaccination Status, February 14, 2021",
        subtitle = glue("Hover to see individual state vaccination percentages."),
        caption = "Data: Our World in Data\nGraphic: @steodosescu"
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    coord_flip()


girafe(code = print(early_vax_graph + latest_vax_graph), 
       width_svg = 8, height_svg = 4) %>% 
    girafe_options(opts_hover(css = "fill:cyan;"))


##### NYT Small Multiples Plot #####

## Inspiration: https://livefreeordichotomize.com/2021/04/07/nytimes-map-how-to/

# Enter Census API key
census_api_key("a43c5cbea0b01193b086d5432e1710713093e5e", install = TRUE, overwrite = TRUE)

# Pull state level population data for 2019
pop <- get_acs(geography = "state", variables = "B01003_001", year = 2019)

# Pull in COVID cases data from the New York Time GitHub repo
cases <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-states.csv")

#Wrangle the data
d <- cases %>%
    group_by(state) %>%
    mutate(case = c(cases[1], diff(cases))) %>%
    ungroup() %>%
    filter(!(date == as.Date("2021-03-08") & state == "Missouri")) %>% 
    left_join(pop, by = c("fips" = "GEOID")) %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(
        case_7 = rollmean(case, k = 7, fill = NA),
        case_per_100 = (case_7 / estimate) * 100000) %>%
    ungroup() %>%
    filter(date > as.Date("2021-01-31"), date < as.Date("2021-08-10"))

states <- tibble(state = state.name,
                 state_ = state.abb) %>%
    add_row(state = "District of Columbia", state_ = "DC")

d <- left_join(d, states, by = "state") %>%
    filter(!is.na(state_))
