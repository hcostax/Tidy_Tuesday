
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
suppressMessages(library(tidyverse))
suppressMessages(library(viridis))
suppressMessages(library(ggtext))
suppressMessages(library(lubridate))
suppressMessages(library(janitor))
suppressMessages(library(patchwork))
suppressMessages(library(glue))

# tuesdata <- tidytuesdayR::tt_load('2021-05-18')
tuesdata <- tidytuesdayR::tt_load(2021, week = 21)

survey <- tuesdata$survey

data <- survey

# Cleaning ----------------------------------------------------------------
# Let's clean up the country
data <- as.data.frame(data)
data <- data %>%
  mutate(country = case_when(grepl("us\\s", country)|grepl("usa\\s", country) ~ "USA",
                             TRUE ~ country),
         country = tolower(country),
         country = case_when(country %in% c("united states", "us", "usa", "u.s.", "united states of america", "u.s>", "isa", "united state", "u.s.a", "u.s.a.", "america", "the united states", "united state of america", "united stated", "united statws", "u.s", "unites states", "u. s.", "united sates", "united states of american", "uniited states", "united sates of america", "united states (i work from home and my clients are all over the us/canada/pr", "unted states", "united statesp", "united stattes", "united statea", "united statees", "uniyed states", "uniyes states", "united states of americas", "us of a", "u.sa", "united status", "uniteed states", "united stares", "unite states", "the us", "unitedstates", "united statues", "untied states", "usa (company is based in a us territory, i work remote)", "unitied states", "united sttes", "uniter statez", "u. s", "usa tomorrow", "united stateds", "unitef stated", "usaa", "united states- puerto rico", "usd", "usa, but for foreign gov't", "united statss", "united  states", "usa-- virgin islands", "united statew", "usat", "ua", "united y") ~ "usa",
                             country %in% c("united kingdom", "uk", "england", "great britain", "england/uk", "england, uk.", "britain", "united kingdom (england)", "united kingdom.", "u.k.", "united kindom", "england, uk", "uk for u.s. company", "united kingdomk", "england, gb", "u.k. (northern england)", "u.k", "england, united kingdom", "englang", "uk (england)", "uk, remote", "unites kingdom", "uk, but for globally fully remote company") ~ "uk",
                             country %in% c("canada", "canada, ottawa, ontario", "canadw", "can", "i am located in canada but i work for a company in the us", "canda", "canada and usa", "csnada", "canad", "canada") ~ "canada",
                             TRUE ~ country))

# Remove any countries that have fewer than 10 people
reduced <- data %>%
  group_by(country) %>%
  filter(n() > 10) %>%
  ungroup()

# Remove industries with fewer than 10 people
reduced2 <- reduced %>%
  group_by(industry) %>%
  filter(n() > 10) %>%
  ungroup()

nrow(reduced2) # we still have over 24,000 responses


canada_data <- reduced2 %>%
  filter(currency == 'CAD' & gender == "Woman") %>%
  select(how_old_are_you, industry, annual_salary, city, 
         highest_level_of_education_completed, 
         years_of_experience_in_field, gender, race) %>%
  rename(education = highest_level_of_education_completed,
         experience = years_of_experience_in_field)

head(canada_data)





(
  big_plot <- canada_data %>%
    mutate(
      experience = factor(experience,
        levels = c("1 year or less", "2 - 4 years", 
                        "5-7 years", "8 - 10 years",
                        "11 - 20 years", "21 - 30 years",
                        "31 - 40 years", "41 years or more")),
        how_old_are_you = factor(how_old_are_you,
                            levels = c("under 18", "18-24", 
                                       "25-34", "35-44",
                                       "45-54", "55-64",
                                       "65 or over"))
    ) %>%
    
    ggplot() +
    geom_jitter(
      aes(x = how_old_are_you, 
          y = experience, colour = annual_salary),
      size = 3,
      alpha = 0.55
    ) +
    scale_color_viridis(
      option = "D", 
      "Annual Salary",
      guide = guide_legend(
        title.position = "top",
        label.theme = element_text(
          angle = 0, 
          color = "white", size = 10)
      )
      ) +
    labs(x = "How old are?",
         y = "Years of experience in field"
  ) +
    annotate(
      'text',
      x = 5.9,
      y = 2.5,
      family = "Kanit Light",
      size = 3.3,
      color = "white",
      lineheight = .9,
      label = "Outlier Salary \nCAD$ 960.000"
      ,
      label.colour = NA,
      fill = NA
    ) +

    geom_curve(
      aes(
        x = 6,
        y = 3,
        xend = 5.005,
        yend = 4.911
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.5,
      curvature = +0.3,
      color = "white"
    ) +
    
  theme_minimal() +
    theme(
      text = element_text(family = "Kanit Light"),
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(colour = "grey50",
                                        linetype = "dashed"),
      axis.text = element_text(colour = "white", size = 12),
      axis.title = element_text(colour = "white", size = 14),
      legend.background = element_rect(fill = "black"),
      legend.text = element_text(colour = "white", size = 9),
      legend.title = element_text(colour = "white", size = 10),
      legend.title.align = 0.5,
      legend.position = c(0.095, 0.75)
    )
)



# Popular filds on Canada -------------------------------------------------


# Grab only the canada, and the top 10 industries
CADTop10Ind <- reduced2 %>%
  filter(country == "canada") %>%
  group_by(industry) %>%
  mutate(count = n()) %>%
  arrange(-count) %>%
  filter(annual_salary < 500000) %>%
  filter(industry %in% unique(.$industry)[1:5]) 
  

nrow(CADTop10Ind)

# Make a plot
CADTop10Ind %>%
  ggplot(aes(y = industry))+
  geom_bar() + theme_hcostax()


(
  popular <- CADTop10Ind %>%
    
    mutate(
      experience = factor(years_of_experience_in_field,
                          levels = c("1 year or less", "2 - 4 years", 
                                     "5-7 years", "8 - 10 years",
                                     "11 - 20 years", "21 - 30 years",
                                     "31 - 40 years", "41 years or more")),
      how_old_are_you = factor(how_old_are_you,
                               levels = c("under 18", "18-24", 
                                          "25-34", "35-44",
                                          "45-54", "55-64",
                                          "65 or over"))
    ) %>%
    
    ggplot( aes(x = annual_salary, fill = industry)) +
    geom_histogram(alpha=0.50, position = 'identity', 
                   show.legend = FALSE) +
    
    scale_color_viridis(discrete = TRUE, 
                        option = "D") +
    scale_fill_viridis(discrete = TRUE) +
    labs(title = "Top industries in Canada: Woman Salaries",
         y = "Salaries \ndensity") +
    facet_wrap(. ~ industry, nrow = 1) +
    guides(x = guide_axis(angle = 90)) +
    theme_minimal() +
    theme(
      text = element_text(family = "Kanit Light"),
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "grey80", colour = "grey80"),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_markdown(
        colour = "white",
        family = "Kanit",
        size = 16
      ),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text = element_text(colour = "white", size = 10),
      axis.title = element_text(colour = "white", size = 12),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 14),
      strip.background = element_rect(fill = "grey80", colour = "grey80")
    )
)




# Highest level of education completed -----------------------------------------

CADTop10Ind <- CADTop10Ind %>%
  mutate(
    education = factor(highest_level_of_education_completed,
                       levels = c("High School", "College degree", 
                                  "Master's degree", "PhD",
                                  "Professional degree (MD, JD, etc.)", 
                                  "Some college")),
    experience = factor(years_of_experience_in_field,
                        levels = c("1 year or less", "2 - 4 years", 
                                   "5-7 years", "8 - 10 years",
                                   "11 - 20 years", "21 - 30 years",
                                   "31 - 40 years", "41 years or more")),
    how_old_are_you = factor(how_old_are_you,
                             levels = c("under 18", "18-24", 
                                        "25-34", "35-44",
                                        "45-54", "55-64",
                                        "65 or over"))
  )



(
  recently_watched <- CADTop10Ind  %>%
    ggplot() +
    geom_jitter(
      aes(x = experience,
          y = annual_salary, colour = annual_salary),
      size = 3,
      alpha = 0.55,
      show.legend = FALSE
    ) +
    scale_color_viridis(#discrete = TRUE, 
      option = "D") +
    labs(title = "Highest level of education completed: Woman Salaries in Canada",
         y = "Salaries \nper experience") +
    facet_wrap(. ~ education, nrow = 1) +
    guides(x = guide_axis(angle = 90)) +
    theme_minimal() +
  theme(
    text = element_text(family = "Kanit Light"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "grey80", colour = "grey80"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_markdown(
      colour = "white",
      family = "Kanit",
      size = 16
    ),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text = element_text(colour = "white", size = 10),
    axis.title = element_text(colour = "white", size = 12),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 14),
    strip.background = element_rect(fill = "grey80", colour = "grey80")
  )
)




# Title -------------------------------------------------------------------


(
  title <-
    tibble(
      x = c(-1,-1, 0.35, 2,-1,-1),
      y = c(0.48, 0.40, 0.40, 0.40, 0.33, 0.13),
      label = c(
        "Women Salary in Canada",
        "DataViz:",
        "@hcostax",
        "|  Source: Ask a Manager Survey",
        "Week 21, 2021 Coming Tuesday",
        "Here I show women salaries in Canada <br>
              with concentration of wages per industry <br>
              and wages per level education. <br> 
        However, years of experience contribute <br> 
        more than educational level to obtain <br>
        better wages. "
      )
    ) %>%
    ggplot(aes(x, y, label = label)) +
    geom_textbox(
      width = unit(5, "inch"),
      family = c("Bebas Neue", "Kanit Light", "Kanit Light", "Kanit Light", "Kanit", "Kanit Light"),
      color = c("#ff0000", "#fcfcfc", "#5b00e1", "grey80", "white", "grey80"),
      size = c(8.55, 4, 4, 4, 6, 5),
      fill = NA,
      box.colour = NA,
      hjust = 0.3
    ) +
    scale_x_continuous(limits = c(-5, 5)) +
    scale_y_continuous(limits = c(0, 0.55)) +
    theme_bw() +
    theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank()
    )
)


# Bring together ----------------------------------------------------------
layout <- c(
  area(
    t = 1,
    l = 1,
    b = 8,
    r = 5
  ),
  area(
    t = 1,
    l = 6,
    b = 8,
    r = 10
  ),
  area(
    t = 9,
    l = 1,
    b = 10,
    r = 10
  ),
  area(
    t = 11,
    l = 1,
    b = 12,
    r = 10
  )
)

layout
plot(layout)

title + big_plot + popular + recently_watched +
  plot_layout(design = layout) &
  theme(plot.background = element_rect(fill = "black", colour = "black"))

ggsave(
  paste0("Canadian_women_salaries", format(Sys.time(), "%d%m%Y"), ".png"),
  dpi = 900,
  width = 15,
  height = 8.36,
  type = "cairo-png"
)
















