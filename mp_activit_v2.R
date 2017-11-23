# MP activity plot
# A.Bailey October 11th 2017

library(rvest)
library(tidyverse)
library(stringr)
library(plotly)
# Current session began 21 June 2017
# Contribution, equates to an intervention in a debate - talking
# Written question and votes - parliamentary work 
# Talking vs. Doing
# mp_table <- read_csv("mp_table.csv")
# https://www.parliament.uk/business/committees/committees-a-z/commons-select/
# #content-small h3
# .membership-carousel
# ------------------------------------------------------------------------------
# Get MPs details
url_base <- "https://hansard.parliament.uk/search/Members?currentFormerFilter=1&house=Commons&page="
map_df(1:33, function(i) {
        
        # simple but effective progress indicator
        cat(".")
        
        pg <- read_html(paste0(url_base, i)) 
         data.frame(MP = html_nodes(pg, ".search-results .two-lines span") 
                    %>% html_text(),
                   Party=html_nodes(pg, ".party") 
                   %>% html_text(),
                   Spoken_contribution= html_nodes(pg ,"a.no-underline") 
                   %>% html_attr("href"),
                   stringsAsFactors=FALSE)
        
}) -> mp_details

dplyr::glimpse(mp_details)
mp_voting <- gsub("Spoken","Divisions",mp_details$Spoken_contribution)
# ------------------------------------------------------------------------------
qu_base <- "https://www.parliament.uk/business/publications/written-questions-answers-statements/written-questions-answers/?page="
# qu_html <- read_html(qu_url)
# qu_names <- qu_html %>% 
        # html_nodes(".qna-result-question-title") %>% html_text()

n <- round(12876/100)
map_df(1:n, function(i) {
        
        # simple but effective progress indicator
        cat(".")
        
        qu <- read_html(paste0(qu_base, i, "&max=100&house=commons")) 
        data.frame(Questions = html_nodes(qu, ".qna-result-question-title") %>% 
                           html_text() %>% 
                           str_replace_all("Asked by ", ""),
                   stringsAsFactors=FALSE)
        
}) -> mp_qu

map_df(1:n, function(i) {
        
        # simple but effective progress indicator
        cat(".")
        
        ans <- read_html(paste0(qu_base, i, "&max=100&house=commons")) 
        data.frame(Answers = html_nodes(ans, ".qna-result-answer-title") %>% 
                           html_text() %>% 
                           str_replace_all("Answered by:", ""),
                   stringsAsFactors=FALSE)
        
}) -> mp_ans


dplyr::glimpse(mp_qu)
dplyr::glimpse(mp_ans)
written_count <- mp_qu %>% group_by(Questions) %>% summarise(No_q = n())
written_count <- rename(written_count, mp_name = Questions)

# ------------------------------------------------------------------------------
# Commitee membership
# "h1" MP name
# "h3:nth-child(12) , .list-dates li" currently held offices
twy_base <- "https://www.theyworkforyou.com/mps/"

twy_pg <- read_html(twy_base) 
twy_dat <- data.frame(MP = html_nodes(twy_pg,".people-list__person__name") %>% 
                               html_text(trim = T),
                       urls = html_nodes(twy_pg,".people-list__person") 
           %>% html_attr("href"),
           stringsAsFactors=FALSE)

twy_dat <- twy_dat %>% 
        mutate(comm_url = paste0("https://www.theyworkforyou.com",twy_dat$urls))

mp_comms <- lapply(twy_dat$comm_url[1:33], function(x) read_html(x) %>%
                           html_nodes(".mp-name-and-position h1, .list-dates:nth-child(14)") %>% 
                           html_text(trim = T))

mp_comms <- map_df(1:33, function(j) {
        # simple but effective progress indicator
        cat(".")
        # Create list
        # MP <-  read_html(twy_dat$comm_url[j]) %>% 
        #                    html_nodes(".mp-name-and-position h1") %>% 
        #                    html_text(trim = T)
        data.frame(#MP = read_html(twy_dat$comm_url[j]) %>% 
                    #                          html_nodes(".mp-name-and-position h1") %>%
                     #                         html_text(trim = T),
                   MP = read_html(twy_dat$comm_url[j]) %>%
                           html_nodes(".mp-name-and-position h1") %>% 
                           html_text(trim = T), 
                  comms = if(length(read_html(twy_dat$comm_url[j]) %>%
                           html_nodes(".list-dates:nth-child(13) li,
                   .list-dates:nth-child(14) li, .list-dates:nth-child(15) li") %>% 
                           html_text(trim = T)) < 1) {
                           "None" } else { 
                           read_html(twy_dat$comm_url[j]) %>%
                                    html_nodes(".list-dates:nth-child(13) li,
                   .list-dates:nth-child(14) li") %>% 
                                    html_text(trim = T) } 
                  ,stringsAsFactors = F)
                   
}) 

glimpse(mp_comms)
# Commitees --------------------------------------------------------------------
comm_base <- "https://www.parliament.uk/business/committees/committees-a-z/commons-select/"
comm_url <- data.frame(urls = read_html(comm_base) %>%
                               html_nodes("#content-small h3 a") %>% 
                               html_attr("href"),
                       stringsAsFactors=FALSE)
full_urls <- paste0("https://www.parliament.uk",comm_url$urls)

comm_members <- map_df(1:46, function(i) {
        cat(".")
        data.frame(urls = read_html(full_urls[i]) %>% 
                                   html_nodes(".membership-carousel") %>% 
                           html_attr("href"),stringsAsFactors = F) 
})

# ------------------------------------------------------------------------------
mp_id <- mp_details %>% 
        mutate(ID = mp_details$Spoken_contribution %>% str_extract('([0-9]+)'))
mp_page_base <- "https://www.parliament.uk/biographies/Commons/member/"
commitees <- paste0(mp_page_base,mp_id$ID)

mp_list <- lapply(commitees[1:10], function(x) read_html(x) %>% 
                          html_nodes("#commons-addressas") %>% 
                          html_text(trim = T))

mp_post <- lapply(commitees[1:10], function(x) read_html(x) %>% 
                          html_nodes(".parliamentary-career-row:nth-child(2) .parliamentary-career-post") %>% 
                          html_text(trim = T))



mp_comms <- map_df(1:7, function(j) {
        # simple but effective progress indicator
        cat(".")
        #%>% 
        data.frame(mp_name = read_html(commitees[j]) %>% 
                           html_nodes("#commons-addressas") %>% 
                           html_text(trim = T),
                   post = read_html(commitees[j]) %>% 
                           html_nodes(".parliamentary-career-row:nth-child(2) .parliamentary-career-post") %>% 
                           html_text(trim = T),
                   post_date = read_html(commitees[j]) %>% 
                           html_nodes(".parliamentary-career-row:nth-child(2) .parliamentary-career-date") %>% 
                           html_text(trim = T),
                   comms_post = read_html(commitees[j]) %>% 
                           html_nodes(".committees-post") %>% 
                           html_text(trim = T), 
                   comm_date = read_html(commitees[j]) %>%
                           html_nodes(".committees-date") %>%
                           html_text(trim = T) ,
                   stringsAsFactors = FALSE)
        }) 

glimpse(mp_comms)
mp_comm_filter <- mp_comms %>%  filter(comm_date == "Sep 2017 -" | post_date == "2016 -" ) %>% 
        group_by(mp_name) %>% summarise(commitees = n())
# ------------------------------------------------------------------------------

spoken <- paste0("https://hansard.parliament.uk/",mp_details$Spoken_contribution,
                 "&startDate=2017-06-21")

voting <- paste0("https://hansard.parliament.uk/",mp_voting,
                 "&startDate=2017-06-21")

map_df(1:650, function(j) {
        # simple but effective progress indicator
        cat(".")
        #%>% 
        data.frame(mp_name = read_html(spoken[j]) %>% 
                html_nodes(".page-title a") %>% 
                html_text(),
        mp_party = read_html(spoken[j]) %>%
                html_nodes(".member-info :nth-child(1)") %>%
                html_text() %>% .[4],
        spoke_h =  read_html(spoken[j]) %>%
        html_node(".active a") %>%
        html_text() %>% str_extract(.,'([0-9]+)'),
        votes =  read_html(voting[j]) %>%
                html_node(".active a") %>%
                html_text() %>% str_extract(.,'([0-9]+)'),
        stringsAsFactors = FALSE)
                            }) -> mp_spoken

# Join data frames -------------------------------------------------------------
mp_table <- mp_spoken %>% left_join(written_count,by = "mp_name") %>% 
        replace_na(list(n = 0))
mp_table$spoke_h <- as.numeric(mp_table$spoke_h)
mp_table$votes <- as.numeric(mp_table$votes)

mp_qn <- (mp_table$n - mean(mp_table$n)) / sd(mp_table$n)
mp_vn <- (mp_table$votes - mean(mp_table$votes)) / sd(mp_table$votes)
mp_table <- mp_table %>% mutate(Activity_sum = mp_table$votes + 
                                        mp_table$Written_question)
#(myVar - mean(myVar)) / sd(myVar)
mp_activity <- mp_qn + mp_vn
mp_table <- mp_table %>% mutate(Activity = mp_activity)

mp_table <- rename(mp_table, MP = mp_name, Party = mp_party,
                   Intervention = spoke_h, 
                   Written_question = n)

# Remove the Speaker (speaks a lot!) and Sinn Fein (SF can't vote)
mp_table <- mp_table %>% filter(Party != "Speaker" & Party != "Sinn Féin")# &
                                       # MP != "Mrs Theresa May")
        

median_spoken <- median(mp_table$Intervention)
median_written <- median(mp_table$Activity_sum)

mp_filter <- mp_table %>% filter(Intervention < 300, Activity < 5)

medianf_spoken <- median(mp_filter$Intervention)
medianf_written <- median(mp_filter$Activity)

mp_table %>% filter(Intervention > 400)
mp_table %>% filter(Activity > 5)

mp_table <- mp_table %>% mutate(Activity_sum = mp_table$votes + 
                                        mp_table$Written_question)
#write_excel_csv(mp_table,"mp_table.csv")
# Plot -------------------------------------------------------------------------
ggplot(mp_table, aes(x = Intervention, y = Activity)) +
        geom_point(aes(colour = Party)) +
        geom_vline(aes(xintercept=median_spoken)) + 
        geom_hline(aes(yintercept=median_written)) +
        theme_minimal()

ggplot(mp_filter, aes(x = Intervention, y = Activity)) +
        geom_point(aes(colour = Party)) +
        geom_vline(aes(xintercept=medianf_spoken)) + 
        geom_hline(aes(yintercept=medianf_written)) +
        theme_minimal()

# ------------------------------------------------------------------------------
p <- ggplot(mp_table, aes(x = Intervention, y = Activity_sum, 
                          colour = Party, label = MP,alpha = 1/10)) +
        geom_point() +
        #geom_text(nudge_y = 0.4) +
        geom_vline(aes(xintercept=median_spoken)) + 
        geom_hline(aes(yintercept=median_written)) +
        geom_jitter() +
        scale_colour_manual(values = c("#003d6d","#f5822a", "#83de62", 
                                       "#000000", "#e23a3f", "#e23a3f", 
                                       "#ffac12", "#5a5101", "#cccccc")) +
        theme_minimal()
ggplotly(p)

chart_link = plotly_POST(p, filename="mp_activity")
chart_link
