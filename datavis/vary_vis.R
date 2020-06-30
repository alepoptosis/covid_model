library(tidyverse)
library(RColorBrewer)
library(stringr)
library(ggnewscale)
theme_set(theme_minimal())

# script options, change for different file, output options and plot size
run_name = "2020-06-26_vary-cm-strength"
varying_par = "protection-strength"
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
    mutate_at("run_num", ~ run_num + 1) %>%
    unite("run_num", c("run_num", sprintf("%s",varying_par)), remove = FALSE)
  
  # clean column names
  names(raw) = gsub("\\[|\\]|", "", names(raw))
  names(raw) = gsub("\\.", " ", names(raw))
  names(raw) = gsub("\\-", "_", names(raw))
  
  write.csv(raw, sprintf("%s/%sfull.csv", path, pattern), row.names = FALSE)
}

summary = raw %>%
  group_by(run_num) %>%
  summarise(death_toll = max(`count deads`), 
            peak_size = max(`count symptomatics`)) %>%
  separate("run_num", sep = "_", remove = TRUE, 
           into = c("run_num", "protection_strength")) %>%
  mutate_at(c("run_num", "protection_strength"), as.numeric) %>%
  pivot_longer(c("death_toll", "peak_size"),
             names_to = "metric",
             values_to = "count")

ggplot(summary, 
       aes(x=protection_strength, y=count, group = interaction(run_num, metric))) +
  geom_point(aes(color = metric)) + geom_line(aes(color = metric))

if (export_plots) {
  ggsave(sprintf("%s/%sdeath-peak.pdf", dest_path, pattern), 
         width = g_width, height = g_height)
}