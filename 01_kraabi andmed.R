## Kraabi kõigi Eesti erakondade liikmete nimed ja sünniajad Äriregistrist

library(RSelenium)
library(dplyr)
library(rvest)
library(purrr)
library(stringr)

url <- "https://ariregister.rik.ee/erakonnad"

erakonnad_html <- read_html(url)

# erakondade loetelu koos viitega iga erakonna liikmete andmetele
erakonnad <- erakonnad_html %>%
    html_nodes(".td_v:nth-child(2)") %>%
    html_text() %>%
    repair_encoding() %>%
    data_frame() %>%
    select(erakond = 1) %>%
    # css, mille järgi hiljem iga erakonna liikmed kraapida
    mutate(css_paaris = str_c(".tr_1:nth-child(", row_number() + 1, ") .td_v:nth-child(8) a"),
           css_paaritu = str_c(".tr_0:nth-child(", row_number() + 1, ") .td_v:nth-child(8) a"),
           css = ifelse(row_number() %% 2, css_paaris, css_paaritu))

# käivita Rselenium javascript lehelt andmete kraapimiseks
psPath <- "C:/Users/toomase/Documents/Programs/phantomjs-2.1.1-windows/bin/phantomjs.exe"
startServer()
remDr <- remoteDriver(browserName = "phantomjs", 
                      extraCapabilities = list(phantomjs.binary.path = psPath))
remDr$open()

# funktsioon, mis kraabib kõigi erakondade liikmete andmed
kraabi_liikmed <- function(x){
    remDr$navigate(url)
    # ava erakonna liikmete leht
    erakonna_liikmed <- remDr$findElement(using = "css", x)
    
    erakonna_liikmed$clickElement()
    
    doc <- read_html(remDr$getPageSource()[[1]])
    
    jrk <- doc %>%
        html_nodes(".td_v:nth-child(1)") %>%
        html_text()
    
    nimi <- doc %>%
        html_nodes(".td_v:nth-child(2)") %>%
        html_text() %>%
        repair_encoding()
    
    synniaeg <- doc %>%
        html_nodes(".td_v:nth-child(3)") %>%
        html_text()
    
    liikmeks_astumise_aeg <- doc %>%
        html_nodes(".td_v:nth-child(4)") %>%
        html_text()
    
    nimekiri <- data_frame(jrk, nimi, synniaeg, liikmeks_astumise_aeg)
    
    # kraabi lehekülgede arv, millel erakonna liikmed on
    # vajalik, et järgmises etapis nii mitu korda "järgmine" klikkida
    lehekylgi <- doc %>%
        html_nodes(".bbl") %>%
        html_text() %>%
        as.numeric() %>%
        max()
    
    # funktsioon klikib lingile "järgmine" ja kraabib lehelt andmed
    jargmine_lehekylg <- function(x){
        erakonna_liikmed <- remDr$findElement(using = "css", ".bbt")
        erakonna_liikmed$clickElement()
        
        doc <- read_html(remDr$getPageSource()[[1]])
        
        jrk <- doc %>%
            html_nodes(".td_v:nth-child(1)") %>%
            html_text()
        
        nimi <- doc %>%
            html_nodes(".td_v:nth-child(2)") %>%
            html_text() %>%
            repair_encoding()
        
        synniaeg <- doc %>%
            html_nodes(".td_v:nth-child(3)") %>%
            html_text()
        
        liikmeks_astumise_aeg <- doc %>%
            html_nodes(".td_v:nth-child(4)") %>%
            html_text()
        
        tulemus <- data_frame(jrk, nimi, synniaeg, liikmeks_astumise_aeg)
    }
    
    # kliki nii mitu korda "järgmine" kui palju lehekülgi erakonna kota on
    # kraabi kõigilt lehtedelt andmed
    liikmed_kokku <- map_df(1:(lehekylgi-1), jargmine_lehekylg)
    
    # pane kokku erakonna esimese lehekülje ja teiste lehekülgede andmed
    nimekiri_kokku <- bind_rows(nimekiri, liikmed_kokku) %>%
        mutate(erakonna_css = x)
    
    return(nimekiri_kokku)
}

# kraabi kõgi erakondade liikmete andmed (kokku 12 erakonda)
erakondade_liikmed_kokku <- map_df(head(erakonnad$css, 12), kraabi_liikmed)

# lisa erakonna nimekirjadele erakonna nimi
erakondade_liikmed <- erakondade_liikmed_kokku %>%
    left_join(erakonnad %>% select(css, erakond), 
              by = c("erakonna_css" = "css"))

# salvesta erakonna liikmete nimekiri RData failiks
save(erakondade_liikmed, file = "data/erakondade_liikmes.RData")