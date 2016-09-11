## Kraabi Eesti meeste ja naiste nimed, et nende järgi ennustada erakonna
## liikmete sugu

library(rvest)
library(tidyr)
library(purrr)
library(stringr)

# library(genderizeR) abil leitud populaarsemate nimede sugu
load(file = "data/eesnime_sugu.RData")

# url, kust nimed kraapida
url_poiste <- "http://nimi.ee/poiste-nimed/"
url_tydrukute <- "http://nimi.ee/tudrukute-nimed/"

# kraabi tähestiku lõikes lingid, kust saab omakorda nimed kätte
nimede_lingi_poisid <- read_html(url_poiste) %>%
    html_nodes("#subpages-widget-2 a") %>%
    html_attr("href")

nimede_lingi_tydrukute <- read_html(url_tydrukute) %>%
    html_nodes("#subpages-widget-2 a") %>%
    html_attr("href")

# funktsioon, mis kraabib leheküljelt kõik nimed    
kraabi_nimed <- function(x){
    read_html(x) %>%
        html_nodes("span") %>%
        html_text()
}

# krabai kõik meeste nimed
poiste_nimed <- map(nimede_lingi_poisid, kraabi_nimed)

# kraabi kõik naiste nimed
tydrukute_nimed <- map(nimede_lingi_tydrukute, kraabi_nimed)

# töötle meeste nimede tabel
poiste_nimed_toodeldud <- poiste_nimed %>%
    unlist() %>%  # listist vektor
    repair_encoding() %>%
    data_frame() %>%
    select(nimi = 1) %>%
    # lisa tunnus
    mutate(sugu = "mees")

tydrukute_nimed_toodeldud <- tydrukute_nimed %>%
    unlist() %>%
    repair_encoding() %>%
    data_frame() %>%
    select(nimi = 1) %>%
    mutate(sugu = "naine")

# genderizeR tulemus ühes tabelis koos
nimed_sooga_genderizer <- eesnime_sugu_1 %>%
    bind_rows(eesnime_sugu_2) %>%
    mutate(probability = as.numeric(probability),
           sugu = ifelse(gender == "male", "mees", "naine")) %>%
    filter(probability > 0.8) %>%
    select(nimi = name, sugu) %>%
    distinct()

# pane nimed ühte tabelisse kokku ja töötle andmeid
set.seed(10)
nimed_sooga <- poiste_nimed_toodeldud %>%
    bind_rows(tydrukute_nimed_toodeldud) %>%
    filter(nimi != "Search for:") %>%
    bind_rows(nimed_sooga_genderizer) %>%
    mutate(nimi = str_to_upper(nimi),
           nimi = str_trim(nimi)) %>%
    distinct() %>%
    filter(!is.na(nimi), nimi != "") %>%
    arrange(nimi) %>%
    group_by(nimi) %>%
    sample_n(1)

#salvesta lõpptulemus
save(nimed_sooga, file = "data/nimed_sooga.RData")