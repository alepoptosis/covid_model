library(tidyverse)
library(RColorBrewer)
library(stringr)
library(ggnewscale)
library(viridis)
theme_set(theme_minimal())

# script options, change for different file, output options and plot size
run_name = "2020-06-30_vary-tt-threshold"
varying_par = "testtrace_threshold" # use version with _ instead of -
dest_path = "vis vary"
g_width = 11.69
g_height = 8.27
export_plots = TRUE

############################## DATA WRANGLING #################################
path = sprintf("results/%s", run_name)
pattern = sprintf("%s_", run_name) # date and test name

if (file.exists(sprintf("%s/%sfull.csv", path, pattern))) {
  
  raw = read.csv(sprintf("%s/%sfull.csv", path, pattern), 
                 stringsAsFactors=FALSE, check.names = FALSE)
  
} else {
  
  # get list of csvs
  csvs = list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # merge in one csv and add a run_num id while removing the useless one
  raw = csvs %>%
    set_names() %>%
    map_dfr( ~ read_csv(.x, col_types = cols(), skip = 6), 
             .id = "run_num", stringsAsFactors=FALSE, check.names = FALSE) %>% 
    select(-"[run number]") %>%
    mutate_at("run_num", ~gsub(sprintf("%s/%s|.csv", path, pattern), "", .)) %>%
    mutate_at("run_num", as.numeric) %>%
    mutate_at("run_num", ~ run_num + 1)
  
  # clean column names
  names(raw) = gsub("\\[|\\]|", "", names(raw))
  names(raw) = gsub("\\.", " ", names(raw))
  names(raw) = gsub("\\-", "_", names(raw))
  
  raw = raw %>%
    unite("run_num", c("run_num", sprintf("%s",varying_par)), remove = FALSE)
  
  write.csv(raw, sprintf("%s/%sfull.csv", path, pattern), row.names = FALSE)
}

# subset containing parameter information for each run
par = unique(raw[ ,grepl("^(?!.*(count |count_|step|contacts|dead|currently))", 
                         names(raw), perl=TRUE)])

summary = raw %>%
  group_by(run_num) %>%
  summarise(death_toll = max(`count deads`), 
            peak_size = max(`count symptomatics`)) %>%
  separate("run_num", sep = "_", remove = TRUE, 
           into = c("run_num", sprintf("%s", varying_par))) %>%
  mutate_at(c("run_num", sprintf("%s", varying_par)), as.numeric) %>%
  pivot_longer(c("death_toll", "peak_size"),
             names_to = "metric",
             values_to = "count")

if (length(varying_par) == 1) {
  ggplot(summary, 
         aes(x=get(varying_par), y=count, group = interaction(run_num, metric))) +
    geom_point(aes(color = metric)) + 
    geom_line(aes(color = metric)) +
    coord_cartesian(ylim = c(0, 35000)) +
    labs(x = varying_par)
  
  
  if (export_plots) {
    ggsave(sprintf("%s/%sdeath-peak.pdf", dest_path, pattern), 
           width = g_width, height = g_height)
  }
} 

if (length(varying_par) == 2) {
  summary_aggr = summary %>%
    group_by(asym_test_coverage, sym_test_coverage, metric) %>%
    summarise(mean = mean(count), stdev = sd(count))
  
  ggplot(subset(summary_aggr, metric == "death_toll"), 
         aes(x = get(varying_par[1]), y = get(varying_par[2]),
                      fill = mean)) +
    geom_tile(aes(fill = mean)) +
    geom_text(aes(label = sprintf("%s ± %s", round(mean, 0), round(stdev, 0)))) +
    scale_fill_viridis() +
    scale_y_continuous(breaks = seq(0, 100, by = 25)) +
    scale_x_continuous(breaks = seq(0, 100, by = 25)) +
    theme(panel.grid.minor = element_blank()) +
    labs(x = varying_par[1], y = varying_par[2], 
         fill = sprintf("mean count \n over %s runs \n", max(summary$run_num)))
  
  if (export_plots) {
    ggsave(sprintf("%s/%sdeath_toll.pdf", dest_path, pattern), 
           width = g_width, height = g_height)
  }
  
  ggplot(subset(summary_aggr, metric == "peak_size"), 
         aes(x = get(varying_par[1]), y = get(varying_par[2]),
             fill = mean)) +
    geom_tile(aes(fill = mean)) +
    geom_text(aes(label = sprintf("%s ± %s", round(mean, 0), round(stdev, 0)))) +
    scale_fill_viridis() +
    scale_y_continuous(breaks = seq(0, 100, by = 25)) +
    scale_x_continuous(breaks = seq(0, 100, by = 25)) +
    theme(panel.grid.minor = element_blank()) +
    labs(x = varying_par[1], y = varying_par[2], 
         fill = sprintf("mean count \n over %s runs \n", max(summary$run_num)))
  
  if (export_plots) {
    ggsave(sprintf("%s/%speak_size.pdf", dest_path, pattern), 
           width = g_width, height = g_height)
  }
  
}