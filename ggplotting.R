
head(griis.other.filtered)
full.table = read.csv("WATCHLIST_OUTPUT.csv")

nrow(griis.other.filtered)
nrow(full.table)

# find which row numbers have NA
missing.rows = which(is.na(full.table$species))

missing.species = griis.other.filtered[missing.rows,]

write.csv(missing.species, "missing_species.csv", row.names = FALSE)

# set the threshold for the total proportion of shared KG climates
thresh = 95

# filter such that only species with 50 or more occurrence records are kept
# save this list
full.table.thresh = dplyr::filter(full.table, prop_records_in_kg >= thresh & 
                                    total_n >= 50)

# rename columns accordingly for optional ggplots
full.table.thresh.subset = full.table.thresh %>% 
  dplyr::select(species, contains("prop_records_in"), -prop_records_in_kg) %>%
  dplyr::rename(Af = prop_records_in_1,
                Am = prop_records_in_2,
                Aw = prop_records_in_3,
                Cfa = prop_records_in_14)

num.spp.thresh = nrow(full.table.thresh.subset)

# prepare for ggplot
full.table.thresh.long = reshape2::melt(full.table.thresh.subset) 

climate.bars = ggplot(data = full.table.thresh.long, 
                      # reorder puts them in descending order
                      aes(x = reorder(species, -value), y = value, fill = variable)) +
  # use dodge to not stack
  geom_bar(stat = "identity", position = "stack", colour = "black", alpha = 0.5) +
  scale_fill_manual(values = c("black", "royalblue", "yellow", 
                               "purple")) +
  labs(title = paste0("KG similarity over ", thresh, "% (n = ", num.spp.thresh, ")"),
       x = "Species",
       y = "Proportion of Records (%)",
       fill = "KG climate zone") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  theme(axis.text.x = element_text(face = "italic")) +
  coord_flip()

climate.bars

ggsave(plot = climate.bars, filename = "bars.png", dpi = 450, height = 7, width = 10)

