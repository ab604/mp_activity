# Brexit Scraper Version 3
# A.Bailey 27th November 2017

# Set-up -----------------------------------------------------------------------
library(rvest)
library(tidyverse)
library(stringr)
library(plotly)
library(forcats)
library(tidytext)
library(RColorBrewer)

# URLs -------------------------------------------------------------------------
url_base <- "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/"
# Notices of Amendments: 23 November 2017
url_ext <- c("euwithdrawal_rm_cwh_1123.1-7.html",
             "euwithdrawal_rm_cwh_1123.8-14.html",
             "euwithdrawal_rm_cwh_1123.15-21.html",
             "euwithdrawal_rm_cwh_1123.22-28.html",
             "euwithdrawal_rm_cwh_1123.29-35.html",
             "euwithdrawal_rm_cwh_1123.36-42.html",
             "euwithdrawal_rm_cwh_1123.43-49.html",
             "euwithdrawal_rm_cwh_1123.50-56.html",
             "euwithdrawal_rm_cwh_1123.57-63.html",
             "euwithdrawal_rm_cwh_1123.64-70.html",
             "euwithdrawal_rm_cwh_1123.71-77.html",
             "euwithdrawal_rm_cwh_1123.78-84.html",
             "euwithdrawal_rm_cwh_1123.85-91.html",
             "euwithdrawal_rm_cwh_1123.92-98.html",
             "euwithdrawal_rm_cwh_1123.99-105.html",
             "euwithdrawal_rm_cwh_1123.106-112.html",
             "euwithdrawal_rm_cwh_1123.113-119.html",
             "euwithdrawal_rm_cwh_1123.120-126.html",
             "euwithdrawal_rm_cwh_1123.127-133.html",
             "euwithdrawal_rm_cwh_1123.134-140.html",
             "euwithdrawal_rm_cwh_1123.141-147.html",
             "euwithdrawal_rm_cwh_1123.148-154.html",
             "euwithdrawal_rm_cwh_1123.155-161.html",
             "euwithdrawal_rm_cwh_1123.162-165.html")

# Construct full urls
urls <- paste0(url_base,url_ext)

# Selector gadget CSS selections
mp_name <- ".paraProposerName3-only nobr , .paraProposerName2-only nobr , .paraProposerName1-only nobr , .paraProposerName-only nobr"
clause <- ".paraAmedTextCommitReport-only nobr"

# Get names and clauses from webpages ------------------------------------------
brexit_list <- map(1:length(urls), function(i) {
        # simple but effective progress indicator
        cat(".")
        
        base <- read_html(urls[i]) 
        list(MP = html_nodes(base, mp_name) %>% html_text(),
             Clauses = html_nodes(base, clause) %>% html_text())
})

save(brexit_list,file = "brexit_list.RData")

# Get amendment numbers only ---------------------------------------------------
amend_no <- ".paraAmendmentNumber-only nobr , .paraHeader2-only nobr"

amends <- map_df(1:length(urls), function(i) {
        # simple but effective progress indicator
        cat(".")
        
        base <- read_html(urls[i]) 
        tibble(Amend_number = html_nodes(base, amend_no) %>% html_text())
}) 

new_amends <- amends %>% filter(grepl("[^\\d]",Amend_number,perl = T))
ex_amends <- amends %>% filter(grepl("^\\d",Amend_number,perl = T)) %>%
        mutate(Amend_number = as.numeric(Amend_number)) %>% 
        arrange(desc(Amend_number))

# Number of amendments
n_amends <- nrow(distinct(ex_amends)) 

# Get amendments and MP names --------------------------------------------------
mp_amends <- map(1:length(urls), function(i) {
        # simple but effective progress indicator
        cat(".")
        base <- read_html(urls[i]) 
        list(MP = html_nodes(base, mp_name) %>% html_text(),
               Amend_number = html_nodes(base, amend_no) %>% html_text())
}) 

# Create a table of MP names and amend numbers
mp_amends_df <- map_df(1:length(urls), function(i) {
        tibble(MP = unlist(mp_amends[[i]][1]), 
               Amend_number = mp_amends[[i]][2]) %>% 
                filter(MP != "")})

# Creat df with MP names and the amendments to existing clauses they've tabled 
# or supported
mp_amends_df <- mp_amends_df %>% unnest(Amend_number) #%>% 
        #filter(grepl("^\\d",Amend_number,perl = T)) %>%
        # mutate(Amend_number = as.numeric(Amend_number)) #%>% 
        # arrange(desc(Amend_number))

mp_amends_df <- mp_amends_df %>% 
  mutate(MP = str_trim(mp_amends_df$MP, "both"))

unique(mp_amends_df$Amend_number)
# Unlist the data --------------------------------------------------------------
# Create a table of MP names and clauses
brexit_df <- map_df(1:length(urls), function(i) {
        tibble(MP = unlist(brexit_list[[i]][1]), 
               Clause = brexit_list[[i]][2]) %>% 
                filter(MP != "")})

brexit_df <- brexit_df %>% unnest(Clause) 

# Tidy up the clause names
pattern_2 <- "(^C|^S)[^.,]+"

brexit_df <- brexit_df %>% 
        mutate(Clause = str_extract(brexit_df$Clause,regex(pattern_2))) %>% 
        filter(Clause != is.na(Clause))
unique(brexit_df$Clause)

brexit_df <- brexit_df %>% 
        mutate(MP = str_trim(brexit_df$MP, "both"))

save(brexit_df,file = "brexit_df.RData")

# MP details -------------------------------------------------------------------
# Get MPs details
mp_base <- "https://hansard.parliament.uk/search/Members?currentFormerFilter=1&house=Commons&page="

mp_details <- map_df(1:33, function(i) {
        
        # simple but effective progress indicator
        cat(".")
        
        mp_page <- read_html(paste0(mp_base, i)) 
        tibble(MP = html_nodes(mp_page, ".search-results .two-lines span") 
               %>% html_text(),
               Party=html_nodes(mp_page, ".party") 
               %>% html_text())
        
})

#save(mp_details,file = "mp_details.RData")

# Create unified list of MPs details -------------------------------------------
mp_tabled <- mp_amends_df %>% distinct(MP)

mp_join <- mp_details %>% right_join(mp_tabled)

missing_mps <- which(is.na(mp_join[,2]))
mp_join[missing_mps,]

mp_join$Party[mp_join$MP == "Secretary David Davis"] <- "Conservative"
mp_join$Party[mp_join$MP == "Gavin Shuker"] <- "Labour"
mp_join$Party[mp_join$MP == "Pat McFadden"] <- "Labour"
mp_join$Party[mp_join$MP == "Alistair Carmichael"] <- "Liberal Democrat"
mp_join$Party[153] <- "Scottish National Party"
mp_join$Party[mp_join$MP == "Paul Farrell"] <- "Labour"


# Add Party to tables ----------------------------------------------------------
brexit_join <- brexit_df %>% left_join(mp_join)
mp_amends_join <- mp_amends_df %>% left_join(mp_join)

mp_amends_join %>% filter(is.na(Party))
brexit_join %>% filter(is.na(Party))

# Plot data --------------------------------------------------------------------
parties <- unique(brexit_join$Party)
party_colours <- c(Conservative = "#003d6d",
                   `Scottish National Party` = "#f5822a", 
                   `Green Party` = "#83de62", 
                   Labour = "#e23a3f",
                   `Labour (Co-op)` = "#e23a3f",
                   `Liberal Democrat` = "#ffac12", 
                   `Plaid Cymru`= "#5a5101", 
                   Independent = "#cccccc")

# Plot number of amendments tabled or supported per MP 
mp_amends_join %>% 
  group_by(Party,MP) %>% 
  summarise(count = n()) %>% 
  filter(count > 10) %>% 
  ggplot(aes(fct_reorder(MP, count),count, fill = Party)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = party_colours) +
  xlab("MP") +
  ylab("Number of amendments tabled or supported") +
  coord_flip() +
  theme_minimal() +
  facet_grid(~ Party)

