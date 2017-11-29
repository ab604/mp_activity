# MPs ammendments
# Count number of ammendments from each MP

library(rvest)
library(tidyverse)
library(stringr)
library(plotly)
library(forcats)
library(tidytext)
library(RColorBrewer)

#.paraProposerName3-only nobr , .paraProposerName2-only nobr , .paraProposerName1-only nobr , .paraProposerName-only nobr
# .paraAmedTextCommitReport-only nobr
# Get MPs details
urls <- c("https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.1-7.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.8-14.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.15-21.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.22-28.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.29-35.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.36-42.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.43-49.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.50-56.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.57-63.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.64-70.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.71-77.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.78-84.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.85-91.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.92-98.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.99-105.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.106-112.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.113-119.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.120-126.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.127-133.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.134-140.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.141-147.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.148-154.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.155-161.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.162-168.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.162-168.html",
          "https://publications.parliament.uk/pa/bills/cbill/2017-2019/0005/amend/euwithdrawal_rm_cwh_1117.176-177.html")

mp_name <- ".paraProposerName3-only nobr , .paraProposerName2-only nobr , .paraProposerName1-only nobr , .paraProposerName-only nobr"
# Get names from webpages ------------------------------------------------------
# base <- read_html(urls)
# brexit <- tibble(MP = html_nodes(base, mp_name) %>% html_text())

brexit <- map_df(1:26, function(i) {
        # simple but effective progress indicator
        cat(".")
        
        base <- read_html(urls[i]) 
        tibble(MP = html_nodes(base, mp_name) %>% html_text())
})

brexit_count <- brexit %>% 
        filter(MP != "") %>%  group_by(MP) %>% summarize(Ammendments = n())

save(brexit,file = "brexit.RData")

# Count ammendments ------------------------------------------------------------
cl <- ".paraAmedTextCommitReport-only nobr"

clauses <- map_df(1:26, function(i) {
        # simple but effective progress indicator
        cat(".")
        
        base <- read_html(urls[i]) 
        tibble(Clauses = html_nodes(base, cl) %>% html_text())
}) 

pattern_2 <- "^[^.,]+" 
pattern_1 <- "^(?!P).+"

clauses_split <- str_extract(clauses$Clauses,regex(pattern_1)) %>% 
        tibble(clause_count = str_extract(.,regex(pattern_2))) %>%
        count(clause_count) %>% na.omit(.)

clauses_split %>% group_by(clause_count) %>%  tally()
# clauses_split %>% 
#         filter(clauses_count != str_extract(clauses_split$clause_count,regex(p2)))

save(clauses,file = "clauses.RData")

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

clause_sub <- c("Corresponding powers involving devolved authorities",
                "Retaining EU restrictions in devolution legislation etc.",
                "Financial provision",
                "Publication and rules of evidence",
                "Interpretation",
                "Consequential and transitional provision",
                "Exceptions to savings and incorporation",
                "Dealing with deficiencies arising from withdrawal",
                "Complying with international obligations",
                "Implementing the withdrawal agreement",
                "Further provision about exceptions to savings and incorporation", 
                "Corresponding powers involving devolved authorities",
                "Further amendments of devolution legislation",
                "Powers in connection with fees and charges",
                "Publication and rules of evidence",
                "Regulations",
                "Consequential, transitional, transitory and saving provision")



clause_complete <- clauses_split %>% mutate(Cl_name = clause_sub)

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

save(mp_details,file = "mp_details.RData")

# Join data --------------------------------------------------------------------
brexit_join <- brexit_count %>% left_join(mp_details)

# Manual curation of missing parties -------------------------------------------
brexit_missing <- brexit_join %>% filter(is.na(brexit_join$Party))
missing_parties <- c("Scottish National Party","Scottish National Party",
                     "Labour", "Scottish National Party","Labour (Co-op)",
                     "Labour","Labour","Scottish National Party", "Labour",
                     "Conservative","Scottish National Party")

brexit_missing$Party <- missing_parties
brexit_missing_mp <- brexit_missing %>% select(MP,Party)

# Join data again --------------------------------------------------------------
brexit_complete <- bind_rows(brexit_join,brexit_missing) %>% 
        filter(!is.na(Party))

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
        #top_n(20, Ammendments) %>%
        filter(Ammendments > 10) %>% 
        arrange(desc(Ammendments)) %>%
        ggplot(aes(fct_reorder(MP, Ammendments),Ammendments,fill = Party)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = party_colours) +
        xlab("MP") +
        ylab("Number of amendments tabled or supported") +
        coord_flip() +
        theme_minimal() +
        facet_grid(~ Party)
b.plot

#l <- plotly_build(b.plot)
#l$data[[1]]$orientation <- "h"
#l$data[[2]]$orientation <- "h"
l

b.plot.2 <- brexit_complete %>%
        filter(Party == "Conservative") %>% 
        arrange(desc(Ammendments)) %>%
        ggplot(aes(fct_reorder(MP, Ammendments),Ammendments,fill = Party)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = party_colours) +
        xlab("MP") +
        ylab("Number of amendments tabled or supported") +
        coord_flip()+
        theme_minimal()

b.plot.2

b.plot.3 <- brexit_complete %>%
        filter(Party == "Labour") %>% 
        arrange(desc(Ammendments)) %>%
        ggplot(aes(fct_reorder(MP, Ammendments),Ammendments,fill = Party)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = party_colours) +
        xlab("MP") +
        ylab("Number of amendments tabled or supported") +
        coord_flip()+
        theme_minimal()

b.plot.3

b.plot.4 <- brexit_complete %>%
        filter(Party == "Liberal Democrat") %>% 
        arrange(desc(Ammendments)) %>%
        ggplot(aes(fct_reorder(MP, Ammendments),Ammendments,fill = Party)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = party_colours) +
        xlab("MP") +
        ylab("Number of amendments tabled or supported") +
        coord_flip()+
        theme_minimal()

b.plot.4

p.plot <- brexit_complete %>%
        group_by(Party) %>%
        summarise(Amendments = n()) %>%
        ggplot(aes(fct_reorder(Party, Amendments),Amendments,fill = Party)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = party_colours) +
        xlab("Party")+
        ylab("Amendments tabled") +
        theme_minimal()

p.plot

cols <- colorRampPalette(brewer.pal(12, "Set3"))
myPal <- cols(length(unique(clause_complete$clause_count)))
c.plot <- clause_complete %>% 
ggplot(aes(fct_reorder(clause_count, n),n, fill = Cl_name)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = myPal) +
        coord_flip() +
        xlab("") +
        ylab("Amendments tabled") +
        theme_minimal() +
        theme(legend.title=element_blank()) 

c.plot

#cl <- ggplotly(c.plot)
#cl$data[[1]]$orientation <- "h"
#l$data[[2]]$orientation <- "h"
#cl
