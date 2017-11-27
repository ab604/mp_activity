# Brexit v2
# A. Bailey 26th November 2017

# Set-up -----------------------------------------------------------------------
library(rvest)
library(tidyverse)
library(stringr)
library(plotly)
library(forcats)
library(tidytext)
library(RColorBrewer)

#.paraProposerName3-only nobr , .paraProposerName2-only nobr , .paraProposerName1-only nobr , .paraProposerName-only nobr
# .paraAmedTextCommitReport-only nobr
# Get ammendment bill MP names and clauses
url_base <- "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/"

url_ext <- c("euwithdrawal_rm_cwh_1117.1-7.html",
          "euwithdrawal_rm_cwh_1117.8-14.html",
          "euwithdrawal_rm_cwh_1117.15-21.html",
          "euwithdrawal_rm_cwh_1117.22-28.html",
          "euwithdrawal_rm_cwh_1117.29-35.html",
          "euwithdrawal_rm_cwh_1117.36-42.html",
          "euwithdrawal_rm_cwh_1117.43-49.html",
          "euwithdrawal_rm_cwh_1117.50-56.html",
          "euwithdrawal_rm_cwh_1117.57-63.html",
          "euwithdrawal_rm_cwh_1117.64-70.html",
          "euwithdrawal_rm_cwh_1117.71-77.html",
          "euwithdrawal_rm_cwh_1117.78-84.html",
          "euwithdrawal_rm_cwh_1117.85-91.html",
          "euwithdrawal_rm_cwh_1117.92-98.html",
          "euwithdrawal_rm_cwh_1117.99-105.html",
          "euwithdrawal_rm_cwh_1117.106-112.html",
          "euwithdrawal_rm_cwh_1117.113-119.html",
          "euwithdrawal_rm_cwh_1117.120-126.html",
          "euwithdrawal_rm_cwh_1117.127-133.html",
          "euwithdrawal_rm_cwh_1117.134-140.html",
          "euwithdrawal_rm_cwh_1117.141-147.html",
          "euwithdrawal_rm_cwh_1117.148-154.html",
          "euwithdrawal_rm_cwh_1117.155-161.html",
          "euwithdrawal_rm_cwh_1117.162-168.html",
          "euwithdrawal_rm_cwh_1117.162-168.html",
          "euwithdrawal_rm_cwh_1117.176-177.html")

urls <- paste0(url_base,url_ext)

mp_name <- ".paraProposerName3-only nobr , .paraProposerName2-only nobr , .paraProposerName1-only nobr , .paraProposerName-only nobr"
cl <- ".paraAmedTextCommitReport-only nobr"


# Get names and clauses from webpages ------------------------------------------
brexit_list <- map(1:26, function(i) {
        # simple but effective progress indicator
        cat(".")
        
        base <- read_html(urls[i]) 
        list(MP = html_nodes(base, mp_name) %>% html_text(),
               Clauses = html_nodes(base, cl) %>% html_text())
})

#save(brexit_list,file = "brexit_list.RData")
# Unlist the data --------------------------------------------------------------
# Create a table of MP names and clauses
brexit_df <- map_df(1:26, function(i) {
        tibble(MP = unlist(brexit_list[[i]][1]), 
                  Clause = brexit_list[[i]][2]) %>% 
                filter(MP != "")})

# Tidy up the clause names
pattern_2 <- "^[^.,]+" 
pattern_1 <- "^(?!P).+"

brexit_df <- brexit_df %>% 
        mutate(Clause = str_extract(brexit_df$Clause,regex(pattern_1)))
brexit_df <- brexit_df %>% 
        mutate(Clause = str_extract(brexit_df$Clause,regex(pattern_2)))
brexit_df <-   brexit_df %>% 
        filter(Clause != "character(0)") %>% 
        mutate(Clause = str_sub(Clause,4)) 

brexit_df <- brexit_df %>% 
        mutate(Clause = str_replace(brexit_df$Clause, "^use\\s\\s14", "Clause  14"))

brexit_df <- brexit_df %>% 
        mutate(MP =str_trim(brexit_df$MP, "both"))
        
#save(brexit_df,file = "brexit.RData")

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

# Join MP and bill data --------------------------------------------------------
# Join data --------------------------------------------------------------------
brexit_join <- brexit_df %>% left_join(mp_details)

# Manual curation of missing parties -------------------------------------------
brexit_missing <- brexit_join %>% filter(is.na(brexit_join$Party)) 
# [1] "Pat McFadden"          "Paul Farrell"          "Gavin Shuker"          "Pat McFadden"         
# [5] "Brendan O’Hara"        "Brendan O’Hara"        "Brendan O’Hara"        "Secretary David Davis"
# [9] "Secretary David Davis" "Secretary David Davis"

missing_parties <- c("Labour","Labour","Labour (Co-op)","Labour",
                     "Scottish National Party","Scottish National Party",
                     "Scottish National Party",
                     "Conservative","Conservative","Conservative")
        
brexit_missing$Party <- missing_parties
#brexit_missing_mp <- brexit_missing %>% select(MP,Party)

# Join data again --------------------------------------------------------------
# This creates a table of MPs names, the amendment clause and their party
brexit_complete <- bind_rows(brexit_join,brexit_missing) %>% 
        filter(!is.na(Party))

# Clauses ----------------------------------------------------------------------
# cl_sched <- c(`Clause  1` = "Repeal of the European Communities Act 1972",
#               `Clause  2` = "Saving for EU‐derived domestic legislation",
#               `Clause  3` = "Incorporation of direct EU legislation", 
#               `Clause  4` = "Saving for rights etc. under section 2(1) of the ECA",
#               `Clause  5` = "Exceptions to savings and incorporation",
#               `Clause  6` = "Interpretation of retained EU law",
#               `Clause  7` = "Dealing with deficiencies arising from withdrawal",
#               `Clause  8` = "Complying with international obligations",
#               `Clause  9`= "Implementing the withdrawal agreement",
#               `Clause  10` = "Corresponding powers involving devolved authorities",
#               `Clause  11`= "Retaining EU restrictions in devolution legislation etc.", 
#               `Clause  12`= "Financial provision", 
#               `Clause  13`= "Publication and rules of evidence", 
#               `Clause  14`= "Interpretation",
#               `Clause  15`= "Index of defined expressions", 
#               `Clause  16`= "Regulations", 
#               `Clause  17`= "Consequential and transitional provision", 
#               `Clause  18`= "Extent", 
#               `Clause  19`= "Commencement and short title", 
#               `Schedule  1`= "Further provision about exceptions to savings and incorporation", 
#               `Schedule  2`= "Corresponding powers involving devolved authorities",
#               `Schedule  3`= "Further amendments of devolution legislation",
#               `Schedule  4`= "Powers in connection with fees and charges",
#               `Schedule  5`= "Publication and rules of evidence",
#               `Schedule  6`= "Instruments which are exempt EU instruments",
#               `Schedule  7`= "Regulations",
#               `Schedule  8`= "Consequential, transitional, transitory and saving provision",
#               `Schedule  9`= "Additional repeals")

clause_sub <- c("C14:Interpretation",
  "C10:Corresponding powers involving devolved authorities",
  "C11:Retaining EU restrictions in devolution legislation etc.",
  "C12:Financial provision",
  "C13:Publication and rules of evidence",
  "C17:Consequential and transitional provision",
  "C5:Exceptions to savings and incorporation",
  "C7:Dealing with deficiencies arising from withdrawal",
  "C8:Complying with international obligations",
  "C9:Implementing the withdrawal agreement",
  "S1:Corresponding powers involving devolved authorities",
  "S2:Corresponding powers involving devolved authorities",
  "S7:Regulations",
  "S8:Consequential, transitional, transitory and saving provision")

# Select the clauses column and count how many unique 

clause_tally <- brexit_complete %>% group_by(Clause,MP) %>% tally()
clause_amend_tally <- clause_tally %>%  group_by(Clause) %>%  summarize(N_sigs = n())
# Number of uniques MPs signed to amend each clause
clause_amend_complete <- clause_amend_tally %>% mutate(Cl_name = clause_sub)

# Number of ammendments
mp_n_sigs <- brexit_complete %>% group_by(MP) %>% summarize(N_sigs = n())
bill_n_amends <- brexit_complete %>% group_by(MP,Party) %>%
        summarise(Amendments = n())
bill_amends <- bill_n_amends %>% group_by(Party) %>% tally()
sum(bill_amends$n)
# Plot data --------------------------------------------------------------------
parties <- unique(brexit_complete$Party)
party_colours <- c(Conservative = "#003d6d",
                   `Scottish National Party` = "#f5822a", 
                   `Green Party` = "#83de62", 
                   Labour = "#e23a3f",
                   `Labour (Co-op)` = "#e23a3f",
                   `Liberal Democrat` = "#ffac12", 
                   `Plaid Cymru`= "#5a5101", 
                   Independent = "#cccccc")


b.plot <- brexit_complete %>%
        group_by(MP,Party) %>% 
        summarize(N_sigs = n()) %>% 
        #top_n(20, Ammendments) %>%
        filter(N_sigs > 10) %>% 
        arrange(desc(N_sigs)) %>%
        ggplot(aes(fct_reorder(MP, N_sigs),N_sigs, fill = Party)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = party_colours) +
        xlab("MP") +
        ylab("Number of amendments tabled or supported") +
        coord_flip() +
        theme_minimal() +
        facet_grid(~ Party)
b.plot

p.plot <- brexit_complete %>%
        group_by(MP,Party) %>%
        summarise(Amendments = n()) %>%
        group_by(Party) %>% summarise(Amendments = n()) %>% 
        ggplot(aes(fct_reorder(Party, Amendments),Amendments,fill = Party)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = party_colours) +
        xlab("Party")+
        ylab("Amendments tabled") +
        theme_minimal()

p.plot

cols <- colorRampPalette(brewer.pal(12, "Set3"))
myPal <- cols(length(unique(clause_amend_complete$Clause)))
c.plot <- clause_amend_complete %>% 
        ggplot(aes(fct_reorder(Clause, N_sigs),N_sigs, fill = Cl_name)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = myPal) +
        coord_flip() +
        xlab("") +
        ylab("Number of sigs per clause") +
        theme_minimal() +
        theme(legend.title=element_blank()) 

c.plot
