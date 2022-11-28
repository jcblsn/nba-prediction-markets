

# setup -------------------------------------------------------------------

# devtools::install_github("rtelmore/ballr")
source("/Users/jacobeliason/Documents/Files/Code/Resources/manifold-markets-nba-api-key.R")

library(tidyverse)
library(manifoldr)
library(ballr)


# get teams ---------------------------------------------------------------

standings <- ballr::NBAStandingsByDate()
team_names <- c(
  standings$East$eastern_conference,
  standings$West$western_conference
) |> sort()


# how many markets can i make? --------------------------------------------

mana_balance <- manifoldr::get_my_balance()
floor(mana_balance / 50.1)


# get 538 probabilities ---------------------------------------------------

forecast <- read_csv("https://projects.fivethirtyeight.com/nba-model/nba_elo_latest.csv")
forecast

library(rvest)
url <- "https://projects.fivethirtyeight.com/2023-nba-predictions/"

page <- 
  read_html(url) |> 
  html_table() |> 
  pluck(1)

colnames(page) <- page[3,]

(forecast <- 
    page |> 
    janitor::clean_names() |> 
    slice_tail(n=-3) |> 
    mutate(team = str_remove_all(team, "[^a-zA-Z]") %>% 
             ifelse(.=="ers","76ers",.) %>%
             ifelse(.=="TrailBlazers","Trail Blazers",.)) |> 
    mutate_at(vars(starts_with("chance_of")), function(x) as.numeric(str_remove_all(x,"\\%|<|>"))) |> 
    select(team, starts_with("chance_of")) |> 
    arrange(desc(chance_of_making_playoffs_make_play_offs)))

# forecast |> 
#   write_csv(
#     str_c(getwd(), "/DATA/forecast probs ",fs::path_sanitize(lubridate::now()),".csv")
#   )

# pick markets to make ----------------------------------------------------

# get most uncertain teams

picked_teams <- forecast |> 
  filter(
    !(team %in% (forecast |> slice_head(n=9) |> pull(team))),
    !(team %in% (forecast |> slice_tail(n=12) |> pull(team)))
  ) |> 
  select(
    team, chance_make_playoffs = chance_of_making_playoffs_make_play_offs
  )

# set up transactions -----------------------------------------------------

# get group id
groups <- manifoldr::get_groups()
groups <- map_dfr(
  groups$content,
  function(x) x %>% pluck() |> unlist() |> t() |> data.frame()
) |>
  as_tibble() |>
  janitor::clean_names()

manifold_markets_nba_group_slug <- "80ZJOp4akW0uaVFLycd5"
april_15_2023_unix <- 1681513200000

for(i in 1:length(picked_teams$team)) {
  (full_name <- team_names[str_detect(team_names, "Lakers")])#picked_teams$team[i])])
  (q <- str_c("Will the ",full_name," make the playoffs for the 2022-2023 NBA season?"))
  (d <- str_c("This market will resolve 'YES' if the ",full_name," make the playoffs for the 2022-2023 NBA season. If the ",full_name," qualify for the Play-In Tournament but do not advance to the Playoffs, this market will resolve 'NO'. Starting odds are taken from [FiveThirtyEight's forecast](https://projects.fivethirtyeight.com/2023-nba-predictions/) on the date of this market's creation."))
  
  resp <- submit_market_create(
    mm_outcome_type = "BINARY",
    mm_question = q,
    mm_description_md = d,
    mm_close_time = april_15_2023_unix,
    mm_group_id = manifold_markets_nba_group_slug,
    mm_initial_prob = picked_teams$chance_make_playoffs[i],
  )
}
