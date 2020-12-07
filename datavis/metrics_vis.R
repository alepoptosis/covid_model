# METRIC COMPARISON VISUALISATION SCRIPT: for comparing multiple experiments

packages <- c("tidyverse", "RColorBrewer", "ggnewscale", "gridExtra")

for (pkg in packages){
  if (!require(package = pkg, character.only = TRUE)){
    install.packages(pkgs = pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

theme_set(theme_minimal(base_size = 25))

path = "results/metrics" # put all full files needed in this folder
prefix = c("2020-08-10_", "2020-07-24_", "/", path, # add all possible dates
           "_full.csv", "-opt", "-6mo")             # and extra descriptors
g_width = 11.69     # size of plots
g_height = 8.27
export_plots = TRUE     # export plots or just display them
separate_plots = FALSE     # whether each metric should have its own plot
dest_path = "visualisations"     # folder for visualisations

# obtain list of csvs
csvs = list.files(path = path, pattern = ".csv", full.names = TRUE)

# obtain string to use as regex to clean the file names
pattern = paste(unlist(prefix), collapse = "|")

# collate the csvs into one dataframe and clean the run_name column
raw = csvs %>%
  set_names() %>%
  map_dfr( ~ read_csv(.x, col_types = cols()),
           .id = "run_name", stringsAsFactors=FALSE, check.names = FALSE) %>%
  mutate_at("run_name", ~gsub(pattern, "", .))

# subset containing only data on counts, run number and step
data = raw[ ,grepl("^count |^step|^run_num|^run_name", names(raw))]
# subset containing parameter information for each run
par = unique(raw[ ,grepl("^(?!.*(count |count_|step|contacts|dead|currently))", 
                         names(raw), perl=TRUE)])

pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1] # population size

##### SYMPTOMATIC PEAK

data_long = data %>% pivot_longer (
    cols = starts_with("count"),
    names_to = "breed", 
    names_prefix = "count ",
    values_to = "count"
  ) 

peak_metrics = data_long %>%
  filter(breed == "symptomatics") %>%
  group_by(run_name, run_num) %>%
  summarise(sym_peak = max(count)) %>%
  group_by(run_name) %>%
  summarise(sympeak_mean = mean(sym_peak), sympeak_sd = sd(sym_peak))

##### TOTAL INFECTIONS

infs = raw[ ,grepl("^count_infecteds_|run_num|run_name|step", names(raw))]

infs_long = infs %>%
  group_by(run_name, run_num, step) %>%
  mutate(count_infecteds_total = sum(`count_infecteds_0_29`, 
                                     `count_infecteds_30_59`, 
                                     `count_infecteds_60+`)) %>% 
  pivot_longer (
                 cols = starts_with("count_infecteds_"),
                 names_to = "age",
                 names_prefix = "count_infecteds_",
                 values_to = "cum_count"
               ) 

infs_metrics = infs_long %>%
  filter(age == "total") %>%
  group_by(run_name, run_num) %>%
  summarise(tot_infs = max(cum_count)/10) %>%
  group_by(run_name) %>%
  summarise(totinfs_mean = mean(tot_infs), totinfs_sd = sd(tot_infs))

##### TOTAL CONTACTS

contact = raw[ ,grepl("run_num|step|num_contacts|run_name", names(raw))]

contact_metrics = contact %>%
  group_by(run_name, run_num) %>%
  summarise(tot_contacts = sum(num_contacts)/10000) %>%
  group_by(run_name) %>%
  summarise(totcontacts_mean = mean(tot_contacts), 
            totcontacts_sd = sd(tot_contacts))
 
##### DEATH TOLL

death_metrics = data_long %>%
  filter(breed == "deads") %>%
  group_by(run_name, run_num) %>%
  summarise(tot_deaths = max(count)) %>%
  group_by(run_name) %>%
  summarise(totdeaths_mean = mean(tot_deaths), totdeaths_sd = sd(tot_deaths))

##### MERGE

metrics = merge(peak_metrics, infs_metrics)
metrics = merge(metrics, contact_metrics)
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


# expanded labels for the metrics
met_lab = c("sympeak" = "Peak cases", "totinfs" = "10 infections",
            "totdeaths" = "Death toll", "totcontacts" = "10,000 contacts")

if (separate_plots) {
  for (m in unique(metrics$metric)) {
    if (m == "sympeak") {colour = "#FB8072"}
    if (m == "totinfs") {colour = "#80B1D3"}
    if (m == "totdeaths") {colour = "#666666"}
    if (m == "totcontacts") {colour = "#FDB462"}
    
    ggplot(data = subset(metrics, metric == m), 
           aes(x = "run_name", y = "mean", fill = "metric")) + 
      geom_col(aes(x = run_name, y = value, fill = metric),  
               position=position_dodge(0.5), width = 0.5) +
      scale_fill_manual(name="Metric", labels = met_lab, values = colour) +
      scale_x_discrete(limits = rev(levels(metrics$run_name))) +
      labs(x = "run name", y = "")
    
    if (export_plots) {
      ggsave(sprintf("%s/%s.pdf", dest_path, m), 
             width = g_width, height = g_height)
    }
  }
} else {
  # CHANGE LABELS ACCORDINGLY - HAS TO BE DONE MANUALLY
  # labels = c("All", "None", "IS", "LD", "PP", "SV", "TT")
  labels = c("All", "None", "IS+SV", "IS+SV\n+DLD", 
             "PP+TT", "PP+TT\n+DLD", "PP+TT\n+SV")
  
  ggplot(data = metrics,
         aes(x = "run_name", y = "mean", fill = "metric")) + 
    geom_col(aes(x = run_name, y = mean, fill = metric),  
             position=position_dodge(0.5), width = 0.5) +
    scale_fill_manual(name="Metric", labels = met_lab,
                      values = c("#FB8072", "#80B1D3", "#666666", "#FDB462")) +
    scale_x_discrete(limits = rev(levels(metrics$run_name))#) +
                     ,labels = labels) +
    theme(axis.text.x = element_text(size = 16)) +
    labs(x = "", y = "")
  
  if (export_plots) {
    ggsave(sprintf("%s/all-metrics.pdf", dest_path), 
           width = g_width, height = g_height)
  }
}
