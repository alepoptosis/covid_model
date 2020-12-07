# METRIC COMPARISON VISUALISATION SCRIPT: for comparing multiple experiments

packages <- c("tidyverse", "RColorBrewer", "ggnewscale", "gridExtra")

for (pkg in packages){
  if (!require(package = pkg, character.only = TRUE)){
    install.packages(pkgs = pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

theme_set(theme_minimal(base_size = 40))
theme_update(panel.grid.major = element_line(colour = "grey95"))
pal = c("#B3DE69", "#FFD92F", "#BEBADA", "#FC8D62", "#80B1D3", "#B3B3B3")
metrics_pal = c("#666666", "#FB8072", "#80B1D3")

path = "results/metrics" # put all files needed in this folder
prefix = c("/", path, ".csv", "-only") # things to clean out from file names
g_width = 11.69     # size of plots
g_height = 8.27
dest_path = "visualisations"     # folder for visualisations
export_plots = TRUE     # export plots or just display them

# edit the order in which the runs go using the runs name (left to right)
run_order = c("no-controls", "all-controls", "ld", "sv", "pp", "tt", "is")
# edit the formatted/extended names of the runs (ordered as above)
extended_names = c("None", "All", "LD", "SV", "PP", "TT", "IS")


# obtain list of csvs
csvs = list.files(path = path, pattern = ".csv", full.names = TRUE)

# obtain string to use as regex to clean the file names
pattern = paste(unlist(prefix), collapse = "|")

# collate the csvs into one dataframe and clean the run_name column
raw = csvs %>%
  set_names() %>%
  map_dfr( ~ read_csv(.x, col_types = cols(), skip = 6),
           .id = "run_name", stringsAsFactors=FALSE, check.names = FALSE) %>%
  mutate_at("run_name", ~gsub(pattern, "", .))

names(raw) = gsub("run number", "run_num", names(raw))
names(raw) = gsub("\\[|\\]|", "", names(raw))
names(raw) = gsub("\\.", " ", names(raw))
names(raw) = gsub("\\-", "_", names(raw))

# subset containing only data on counts, run number and step
# and collapse deceased counts by age into a single `count deceased`
# along with infected count by age into a single `count infected`
data = raw %>%
  select(matches("run_name|run_num|step|count|deceased|infected")) %>%
  mutate(`count deceased` = select(., contains("deceased")) %>% 
           rowSums(na.rm = TRUE)) %>%
  mutate(`count infected` = select(., contains("\"infected\"")) %>% 
           rowSums(na.rm = TRUE)) %>%
  select(-contains("get_"))


# subset containing parameter information for each run
par = raw %>%
  select(-matches("^count|^num_contacts|^contacts|^step|^get|active")) %>%
  distinct()

pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1] # population size

##### symptomatic peak

# turn data into long format
data_long = data %>% pivot_longer (
  cols = starts_with("count"),
  names_to = "breed", 
  names_prefix = "count |count_",
  values_to = "count"
) %>%
  mutate_at("breed", ~gsub("^_", "", .)) %>%
  mutate_at("breed", ~gsub("_", " ", .))

peak_metrics = data_long %>%
  filter(breed == "symptomatics") %>%
  group_by(run_name, run_num) %>%
  summarise(sym_peak = max(count)) %>%
  group_by(run_name) %>%
  summarise(sympeak_mean = mean(sym_peak), sympeak_sd = sd(sym_peak))

##### total infections

# subset containing info on infected agents
infected = data %>%
  select(matches("run_num|run_name|step|count infected"))

infected_metrics = infected %>%
  group_by(run_name, run_num) %>%
  summarise(final_inf = max(`count infected`) / 10) %>%
  group_by(run_name) %>%
  summarise(totinfs_mean = mean(final_inf), totinfs_sd = sd(final_inf))

##### total contacts / 1000

# subset containing contact info only
contacts = raw %>%
  select(matches("run_name|run_num|step|num_contacts"))

contacts_metrics = contacts %>%
  group_by(run_name, run_num) %>%
  summarise(tot_contacts = sum(num_contacts)/10000) %>%
  group_by(run_name) %>%
  summarise(totcontacts_mean = mean(tot_contacts), 
            totcontacts_sd = sd(tot_contacts))

##### total deaths

death_metrics = data_long %>%
  filter(breed == "deceased") %>%
  group_by(run_name, run_num) %>%
  summarise(tot_deaths = max(count)) %>%
  group_by(run_name) %>%
  summarise(totdeaths_mean = mean(tot_deaths), totdeaths_sd = sd(tot_deaths))

##### MERGE

metrics = merge(peak_metrics, infected_metrics)
metrics = merge(metrics, contacts_metrics)
metrics = merge(metrics, death_metrics)

metrics = metrics %>%
  pivot_longer(
    cols = -run_name,
    names_to = c("metric", ".value"),
    names_pattern = "(.+)_(.+)"
  )

# order factors
metrics$metric = factor(metrics$metric, 
                        levels = c("sympeak", "totinfs", "totdeaths", "totcontacts"))

# order run names
metrics$run_name = factor(metrics$run_name,
                          levels = run_order)

# expanded labels for the metrics
met_lab = c("sympeak" = "Peak cases", "totinfs" = "10 Infections",
            "totdeaths" = "Death toll", "totcontacts" = "1,000 contacts")

##### barplot

ggplot(data = metrics,
       aes(x = "run_name", y = "mean", fill = "metric")) + 
  geom_col(aes(x = run_name, y = mean, fill = metric),  
           position=position_dodge(0.5), width = 0.5) +
  scale_fill_manual(name="Metric", labels = met_lab,
                    values = c("#FB8072", "#80B1D3", "#666666", "#FDB462")) +
  scale_x_discrete(limits = rev(levels(metrics$run_name)), labels = extended_names) +
  theme(axis.text.x = element_text(size = 16)) +
  labs(x = "", y = "")

if (export_plots) {
  ggsave(sprintf("%s/all-metrics.pdf", dest_path), 
         width = g_width, height = g_height)
}