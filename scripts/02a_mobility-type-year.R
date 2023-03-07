#-------------------------------------------------------------------------------
# Stacked bar series looking at mobility type by year
#-------------------------------------------------------------------------------

# Prep data  -------------------------------------------------------------------

df <- allclean

# Make sure authored is in date format & floor the date
df$authored <- as.Date(df$authored, "%Y-%m-%d")
df$authored <- floor_date(df$authored, unit="year")

# Filter out NAs
df <- df %>% filter(!is.na(mobility_type))

# Do overall counts, merge & calculate percentages
df2 <- df %>% group_by(authored) %>% count(name="total")
df <- df %>% group_by(authored, mobility_type) %>% count(name="n")  
df <- merge(df, df2, by="authored")

df <- df %>% mutate(per = round(100/total * n, digits = 1))


# Draw the plot ----------------------------------------------------------------

print(
  ggplot(df, aes(x=authored, y=per)) + 
  geom_col(aes(fill=mobility_type)) + 
  scale_x_date(date_labels = "%Y")) +
  labs(title = "Percentage of calls by mobility type") +
  ylab("Percentage") + 
  xlab("Date") +

  # Remove x axis bottom space, and pad the top
  scale_y_continuous(expand = expansion(mult = c(0, 0))) + 
  
  # Use the OTM theme with some variables
  theme_otm(subtitle = FALSE, legend_show = TRUE) +
  
  # Use the OTM palette
  scale_fill_manual(values = c("#AA3377", "#AAAA00", "#6699CC"), na.value = "#BBBBBB") +
  
  # Padding the plot on the right
  theme(plot.margin = margin(.5, 2, .5, .5, "cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom") +
  
  # Reverse order of legend values
  guides(fill = guide_legend(reverse = TRUE,
                             override.aes = list())
)


# Create a table breakdown -----------------------------------------------------

# Clean up some values
df$n <- paste0(df$n, " (",df$per, "%)")
df$authored <- format(df$authored, "%Y")

# Create table and relabel
table <- df %>% pivot_wider(names_from = authored,
                            id_cols = mobility_type,
                            values_from = n) %>%
  arrange(desc(mobility_type)) %>%
  rename("Mobility type" = mobility_type)
