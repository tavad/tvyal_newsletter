PecMemebers <- 
  read_excel("~/down/PecMembers―2023.xlsx")


PecMemebers %>% 
  set_names(c("marz", "hamaynq", "taracq", "tegamas", "andam", "vkayakan",
              "party", "pashton")) %>% 
  mutate(
    party = str_remove_all(party, "[\n\r]"),
    party = fct_relevel(party,  "ՔՊԿ", "ՀԱՅԱՍՏԱՆ", "ՊԱՏԻՎ ՈՒՆԵՄ", "ՏԸՀ"),
    pashton = fct_relevel(pashton, "նախագահ", "քարտուղար", "անդամ"),
  ) %>%
  count(party, pashton) %>% 
  group_by(party) %>% 
  mutate(
    pct = n/sum(n),
    pct_text = percent(pct, accuracy = 0.1)
  ) %>% 
  ggplot(aes(party, n, fill = pashton, label = pct_text)) +
  geom_col() +
  geom_text(position = position_stack(vjust = .5)) +
  scale_y_continuous(breaks = c(0,250,500,750,950)) +
  scale_fill_brewer(type = "qual")

##########################

election_names <- 
  c("district", "district_n", "n_in_lists", "n_additional", "n_travel", 
    "n_total", "n_participants", "n_stamp", "n_tickets", "n_ticket_tech",
    "n_ticket_numbered", "n_ticket_total", "n_ticket_unused", "n_stamps_unused",
    "n_ballot"
  )

members_2021 <- 
  read_excel("~/down/PecMembers.xlsx") %>% 
  set_names(c("marz", "hamaynq", "taracq", "tegamas", "andam", "vkayakan",
              "party", "pashton"))

members_2021_filtered <- 
  members_2021 %>% 
  filter(pashton == "նախագահ") %>% 
  transmute(marz, hamaynq, distrct = taracq, district_n = tegamas, leader = party)

ellections_2021 <- 
  read_excel(
    "~/down/20-06-2021, Ազգային Ժողովի Արտահերթ Ընտրություններ, 2021, Արդյունքներ.xlsx",
    skip = 2
  ) %>% 
  rename(
    setNames(names(.)[1:15], election_names),
    errors = names(.)[41]
  )

ellections_2021 %>% 
  pivot_longer(
    -c(election_names, errors), 
    names_to = "party", values_to = "party_votes"
  ) %>% 
  mutate(district = as.numeric(district)) %>% 
  left_join(members_2021_filtered) %>% 
  filter(
    # grepl("քաղաքացիական պա", tolower(party)),
    grepl("հայաստան դաշինք|քաղաքացիական պա|պատիվ ունեմ|բարգավաճ", tolower(party)),
  ) %>% 
  mutate(
    pct_participants = n_participants / n_total,
    votes_pct = party_votes / n_participants
  ) %>% 
  ggplot(aes(votes_pct, pct_participants, color = leader)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~party)


#########################################


ellections_2021 %>% 
  pivot_longer(
    -c(election_names, errors), 
    names_to = "party", values_to = "party_votes"
  ) %>% 
  mutate(district = as.numeric(district)) %>% 
  left_join(members_2021_filtered) %>% 
  filter(
    grepl("քաղաքացիական պա", tolower(party)),
  ) %>% 
  mutate(
    pct_participants = n_participants / n_total,
    votes_pct = party_votes / n_participants
  ) %>% 
  ggplot(aes(votes_pct, pct_participants, color = leader)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~marz)
