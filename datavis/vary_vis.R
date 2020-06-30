library(tidyverse)
library(RColorBrewer)
library(stringr)
library(ggnewscale)
theme_set(theme_minimal())

par(mfrow=c(3,2))

run_names = c("2020-06-26_vary-cm-strength", "2020-06-26_vary-is-strictness",
              "2020-06-26_vary-ld-strictness", "2020-06-26_vary-ld-threshold",
              "2020-06-26_vary-sar-adherance", "2020-06-26_vary-tt-coverage")

varying_pars = c("protection_strength", "isolation_strictness",
                 "lockdown_strictness", "lockdown_threshold",
                 "shelter_adherance", "sym_test_coverage")

# script options, change for different file, output options and plot size
run_name = "2020-06-26_vary-tt-coverage"
varying_par = "sym_test_coverage" # use version with _ instead of -
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
par = unique(raw[ ,grepl("^(?!.*(count |step|contacts|dead|currently))", 
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