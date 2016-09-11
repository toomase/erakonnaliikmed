library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(extrafont)
library(ggthemes)
library(forcats)

load(file = "data/erakondade_liikmes.RData")
load(file = "data/nimed_sooga.RData")
load(file = "data/eesnime_sugu.RData")

# töötle andmeid
erakondade_liikmed_toodeldud <- erakondade_liikmed %>%
    mutate_at(vars(synniaeg, liikmeks_astumise_aeg), 
              funs(as_date(., "%d.%m.%Y"))) %>%
    mutate(vanus = floor(interval(start = synniaeg, end = Sys.Date())
           / duration(num = 1, units = "years")),
           liikmestaas = floor(interval(start = liikmeks_astumise_aeg, end = Sys.Date())
                               / duration(num = 1, units = "years")),
           # eraldi eesnimi soo tuvastamiseks
           eesnimi = str_to_upper(str_extract(nimi, "([^ ]+)")),
           # järjesta erakonnad
           erakond = str_to_title(erakond),
           jrk = ifelse(str_detect(erakond, "Reformi"), 1,
                        ifelse(str_detect(erakond, "Res Publica"), 2,
                               ifelse(str_detect(erakond, "Sotsiaald"), 3,
                                      ifelse(str_detect(erakond, "Keske"), 4,
                                             ifelse(str_detect(erakond, "Vabaer"), 5,
                                                    ifelse(str_detect(erakond, "Konserv"), 6, NA)))))),
           erakond = factor(erakond),
           erakond = fct_reorder(erakond, jrk)) %>%
    # lisa eesnime järgi inimese tõenäoline sugu
    left_join(nimed_sooga, by = c("eesnimi" = "nimi"))

# Riigikogu erakondade liikmete vanuseline jaotus
erakondade_liikmed_toodeldud %>%
    filter(!is.na(jrk)) %>%  ## ainult Riigikogu erakonnad
    ggplot(aes(vanus, ..density.., group = erakond)) +
    geom_density(size = 1, colour = "#2b8cbe") +
    facet_wrap(~erakond, ncol = 2) +
    theme_tufte() +
    ylab("tihedus") +
    labs(title = "Riigikogu erakondade liikmete vanuseline jaotus") +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          text = element_text(family = "Chivo"),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 11))

# Riigikogu erakondade liikmete vanuseline jaotus
erakondade_liikmed_toodeldud %>%
    # ainult Riigikogu erakonnad ja liikmed, kelle sugu õnnestus tuvastada
    filter(!is.na(jrk), !is.na(sugu)) %>%
    ggplot(aes(vanus, ..density.., group = sugu, colour = sugu)) +
    scale_colour_manual(values = c("#2b8cbe", "#fc9272")) +
    geom_density(size = 1.3) +
    facet_wrap(~erakond, ncol = 2) +
    theme_tufte() +
    ylab("tihedus") +
    labs(title = "Riigikogu erakondade liikmete vanuseline ja sooline jaotus",
         subtitle = "") +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          text = element_text(family = "Chivo"),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 11))

# Riigikogu erakondade liikmete staaž
erakondade_liikmed_toodeldud %>%
    # välista Vabaerakond, kuna neil kõigil väga lühike staaž
    filter(!is.na(jrk), !str_detect(erakond, "Vabae")) %>%
    ggplot(aes(liikmestaas, ..density..)) +
    geom_density(size = 1, colour = "#2b8cbe") +
    facet_wrap(~erakond, ncol = 2) +
    theme_tufte() +
    ylab("tihedus") +
    xlab("staaž") +
    labs(title = "Riigikogu erakondade liikmete staaž",
         subtitle = "Välistatud on väga lühikese ajalooga Vabaerakond") +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          text = element_text(family = "Chivo"),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 11))