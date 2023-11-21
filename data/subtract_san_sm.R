# -------------------------------------------------------------------------


# -------------------------------------------------------------------------

source(here::here("data/prepare_data.R"))

sanitation_lvl <- sanitation |> 
  select(-varname_long) |> 
  pivot_wider(names_from = varname_short,
              values_from = percent) |> 
  mutate(san_bas = case_when(
    is.na(san_sm) == TRUE ~ san_bas,
    .default = san_bas - san_sm
  )) |> 
  pivot_longer(cols = san_bas:san_sm,
               names_to = "varname_short",
               values_to = "percent") |> 
  mutate(varname_short = factor(varname_short, levels = levels_varname_short)) |> 
  left_join(codebook_long, join_by("varname_short")) |> 
  mutate(varname_long = str_remove(object, " \\(.*\\)")) |> 
  relocate(varname_long, residence, .after = varname_short) |> 
  select(name:percent) |> 
  mutate(varname_long = factor(varname_long, levels = levels_varname_long),
         varname_short = factor(varname_short, levels = levels_varname_short),
         residence = factor(residence, levels = c("national", "rural", "urban"))
  )


# test plot ---------------------------------------------------------------

sanitation_uga <- sanitation_lvl |> 
  filter(iso3 == "UGA",
         year %in% c(2000, 2020))


ggplot(data = sanitation_uga,
       mapping = aes(x = residence, 
                     y = percent, 
                     fill = varname_long)) +
  geom_col() +
  facet_wrap(~year) +
  scale_fill_colorblind() +
  geom_text(aes(label = round(percent, 1)), 
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") 


