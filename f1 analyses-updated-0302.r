# overall standings for driver
standings_verstappen <- driver_standings %>% filter(driverRef == "max_verstappen")

# only the standings at the end of the year -- for everyone that started in 2014
# or later, this just needs to be filtered by circuitId == 24
yearend_standings_verstappen <- standings_verstappen %>%
     filter(circuitId == 24)

# regression model -- this one is linear, but others are exponential
reg_max <- yearend_standings_verstappen %>% lm(position ~ year, data=.)

# same format for all the graphs
yearend_standings_verstappen %>% ggplot(data=., aes(year, position)) +
     geom_point() +

# the pretty_breaks(n = ) should be the number of yearend standings for each driver
     scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
     coord_cartesian(ylim=c(20,1)) +
     scale_y_continuous(breaks = seq(20, 1, -1)) +
     labs(x = "Season", y = "Final Standing", title = "Max Verstappen's F1 Standings Since Debut") +
     theme(plot.title = element_text(face = "bold", hjust=0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray20", size=0.1),
        panel.grid.minor = element_blank()) +

# specific to the linear model below
    geom_abline(intercept = coefficients(reg_max)[1],
                slope = coefficients(reg_max)[2],
                color = "red3")

standings_hamilton <- driver_standings %>% filter(driverRef == "hamilton")
yearend_standings_hamilton <- standings_hamilton %>%
    filter(driverStandingsId == 13974 | driverStandingsId == 355 |
               driverStandingsId == 29882 | driverStandingsId == 64293 |
               driverStandingsId == 64879 | driverStandingsId == 65492 |
               driverStandingsId == 65960 | driverStandingsId == 67034 |
               driverStandingsId == 67527 | driverStandingsId == 68133 |
               driverStandingsId == 68585 | driverStandingsId == 69110 |
               driverStandingsId == 69730 | driverStandingsId == 70190 |
               driverStandingsId == 70740 | driverStandingsId == 71806)
reg_hamilton <- yearend_standings_hamilton %>% lm(position ~ year + I(year^2), data=.)
yearend_standings_hamilton %>% ggplot(data=., aes(year, position)) +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Lewis Hamilton's F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +
     # specific to the exponential model below
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size=1, color = "red3")

standings_alonso <- driver_standings %>% filter(driverRef == "alonso")
yearend_standings_alonso <- standings_alonso %>%
    filter(driverStandingsId == 11749 | driverStandingsId == 12404 |
               driverStandingsId == 12784 | driverStandingsId == 71227 |
               driverStandingsId == 13202 | driverStandingsId == 13973 |
               driverStandingsId == 358 | driverStandingsId == 29867 |
               driverStandingsId == 64291 | driverStandingsId == 64881 |
               driverStandingsId == 65494 | driverStandingsId == 65957 |
               driverStandingsId == 67019 | driverStandingsId == 67546 |
               driverStandingsId == 68152 | driverStandingsId == 68597 |
               driverStandingsId == 69113 | driverStandingsId == 70758 |
               driverStandingsId == 71812)
reg_alonso <- yearend_standings_alonso %>% lm(position ~ year, data=.)
yearend_standings_alonso %>% ggplot(data=., aes(year, position)) +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 19)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Fernando Alonso's F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +
    geom_abline(intercept = coefficients(reg_alonso)[1],
                slope = coefficients(reg_alonso)[2],
                color = "red3")

standings_hulkenberg <- driver_standings %>% filter(driverRef == "hulkenberg")
yearend_standings_hulkenberg <- standings_hulkenberg %>%
    filter(driverStandingsId == 64304 | driverStandingsId == 65511 |
               driverStandingsId == 65977 | driverStandingsId == 67021 |
               driverStandingsId == 67533 | driverStandingsId == 68138 |
               driverStandingsId == 68594 | driverStandingsId == 69115 |
               driverStandingsId == 69735 | driverStandingsId == 70207)
reg_hulkenberg <- yearend_standings_hulkenberg %>% lm(position ~ year + I(year^2), data=.)
yearend_standings_hulkenberg %>% ggplot(data=., aes(year, position)) +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Nico Hulkenberg's F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size=1, color = "red3")

standings_perez <- driver_standings %>% filter(driverRef == "perez")
yearend_standings_perez <- standings_perez %>%
    filter(driverStandingsId == 64900 | driverStandingsId == 65497 |
               driverStandingsId == 65966 | driverStandingsId == 67025 |
               driverStandingsId == 67536 | driverStandingsId == 68144 |
               driverStandingsId == 68590 | driverStandingsId == 69119 |
               driverStandingsId == 69741 | driverStandingsId == 70192 |
               driverStandingsId == 70744 | driverStandingsId == 71821)
reg_perez <- yearend_standings_perez %>% lm(position ~ year, data=.)
yearend_standings_perez %>% ggplot(data=., aes(year, position)) +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Sergio Perez's F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
         panel.grid.minor = element_blank()) +
    geom_abline(intercept = coefficients(reg_perez)[1],
                slope = coefficients(reg_perez)[2],
                color = "red3")

standings_bottas <- driver_standings %>% filter(driverRef == "bottas")
yearend_standings_bottas <- standings_bottas %>%
    filter(driverStandingsId == 65969 | driverStandingsId == 67020 |
               driverStandingsId == 67544 | driverStandingsId == 68139 |
               driverStandingsId == 68586 | driverStandingsId == 69116 |
               driverStandingsId == 69729 | driverStandingsId == 70187 |
               driverStandingsId == 70742 | driverStandingsId == 71809)
reg_bottas <- yearend_standings_bottas %>% lm(position ~ year + I(year^2), data=.)
yearend_standings_bottas %>% ggplot(data=., aes(year, position)) +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Valterri Bottas's F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size=1, color = "red3")

standings_magnussen <- driver_standings %>% filter(driverRef == "kevin_magnussen")
yearend_standings_magnussen <- standings_magnussen %>%
    filter(circuitId == 24)
reg_magnussen <- yearend_standings_magnussen %>% lm(position ~ year, data=.)
yearend_standings_magnussen %>% ggplot(data=., aes(year, position)) +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Kevin Magnussen's F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +
    geom_abline(intercept = coefficients(reg_magnussen)[1],
                slope = coefficients(reg_magnussen)[2],
                color = "red3")

# overall standings for driver
standings_sainz <- driver_standings %>% filter(driverRef == "sainz")

# only the standings at the end of the year -- for everyone that started in 2014
# or later, this just needs to be filtered by circuitId == 24
yearend_standings_sainz <- standings_sainz %>%
    filter(circuitId == 24)

# regression model -- this one is linear, but others are exponential
reg_sainz <- yearend_standings_sainz %>% lm(position ~ year, data=.)

# same format for all the graphs
yearend_standings_sainz %>% ggplot(data=., aes(year, position)) +
    geom_point() +

# the pretty_breaks(n = ) should be the number of yearend standings for each driver

    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Carlos Sainz Jr.'s F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +

    # specific to the linear model below
    geom_abline(intercept = coefficients(reg_sainz)[1],
                slope = coefficients(reg_sainz)[2],
                color = "red3")
#stroll graph
standings_stroll <- driver_standings %>% filter(driverRef == "stroll")

yearend_standings_stroll <- standings_stroll %>% filter(circuitId == 24)

reg_stroll <- yearend_standings_stroll %>% lm(position ~ year, data=.)

yearend_standings_stroll %>% ggplot(data=., aes(year, position)) +geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Lance Stroll's F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +

    geom_abline(intercept = coefficients(reg_stroll)[1],
                slope = coefficients(reg_stroll)[2],
                color = "red3")

#leclerc graph
standings_leclerc <- driver_standings %>% filter(driverRef == "leclerc")

yearend_standings_leclerc <- standings_leclerc %>% filter(circuitId == 24)

reg_leclerc <- yearend_standings_leclerc %>% lm(position ~ year, data=.)

yearend_standings_leclerc %>% ggplot(data=., aes(year, position)) +geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Charles Leclerc’s F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +

    geom_abline(intercept = coefficients(reg_leclerc)[1],
                slope = coefficients(reg_leclerc)[2],
                color = "red3")

#russell graph

standings_russell <- driver_standings %>% filter(driverRef == "russell")
yearend_standings_russell <- standings_russell %>% filter(
    driverStandingsId == 69744 | driverStandingsId == 70201 |
        driverStandingsId == 70753 | driverStandingsId == 71807 )
reg_russell <- yearend_standings_russell %>% lm(position ~ year + I(year^2), data=.)
yearend_standings_russell %>% ggplot(data=., aes(year, position)) +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "George Russell’s F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +   stat_smooth(method = "lm", formula = y ~ x + I(x^2), size=1, color = "red3")

#norris graph
standings_norris <- driver_standings %>% filter(driverRef == "norris")

yearend_standings_norris <- standings_norris %>% filter(circuitId == 24)

reg_norris <- yearend_standings_norris %>% lm(position ~ year, data=.)

yearend_standings_norris %>% ggplot(data=., aes(year, position)) +geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    coord_cartesian(ylim=c(20,1)) +
    scale_y_continuous(breaks = seq(20, 1, -1)) +
    labs(x = "Season", y = "Final Standing", title = "Lando Norris’ F1 Standings Since Debut") +
    theme(plot.title = element_text(face = "bold", hjust=0.5),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "gray20", size=0.1),
          panel.grid.minor = element_blank()) +

    geom_abline(intercept = coefficients(reg_norris)[1],
                slope = coefficients(reg_norris)[2],
                color = "red3")

predict_max <- predict(reg_max, newdata = data.frame(year = 2023))
predict_hamilton <- predict(reg_hamilton, newdata=data.frame(year = 2023))
predict_alonso <- predict(reg_alonso, newdata=data.frame(year = 2023))
# hulkenberg did not race 2021 or 2022 so his prediction starts in 2021
predict_hulkenberg <- predict(reg_hulkenberg, newdata=data.frame(year=2021))
predict_perez <- predict(reg_perez, newdata=data.frame(year=2023))
predict_bottas <- predict(reg_bottas, newdata=data.frame(year=2023))
predict_magnussen <- predict(reg_magnussen, newdata=data.frame(year=2023))
predict_sainz <- predict(reg_sainz, newdata = data.frame(year = 2023))
predict_stroll <- predict(reg_stroll , newdata=data.frame(year = 2023))
predict_leclerc <- predict(reg_leclerc, newdata=data.frame(year = 2023))
predict_russell <- predict(reg_russell, newdata=data.frame(year=2021))
predict_norris <- predict(reg_norris, newdata=data.frame(year=2023))

# the list is ordered alphabetically to make understanding the output easier
predictions <- order(c(predict_alonso, predict_bottas, predict_hamilton, predict_hulkenberg,
                       predict_leclerc, predict_magnussen, predict_max, predict_norris, predict_perez,
                       predict_russell, predict_sainz, predict_stroll))