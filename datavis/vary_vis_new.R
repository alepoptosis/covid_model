# VARYING PARAMETERS VISUALISATION SCRIPT: for runs with 1-2 varying parameters

packages <- c("tidyverse", "RColorBrewer", "ggnewscale", "gridExtra", "viridis")

for (pkg in packages){
  if (!require(package = pkg, character.only = TRUE)){
    install.packages(pkgs = pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

theme_set(theme_minimal(base_size = 40))
pal = c("#B3DE69", "#FFD92F", "#BEBADA", "#FC8D62", "#80B1D3", "#B3B3B3")
metrics_pal = c("#666666", "#FB8072", "#80B1D3")

# script options, change for different file, output options and plot size

run_name = "2020-11-17_vary-lockdown-compliance-0100" # change name accordingly
varying_par = "lockdown_compliance" # use "_" instead of "-"
optimal_value = 0.25 # optimal value chosen for measure (if needed)
dest_path = "visualisations" # folder for visualisations
g_width = 22     # size of plots
g_height = 16

# choose which plots to make
metrics_plot = TRUE         # output metrics vs param value (1 or 2 params)
fix_metric_plot = FALSE     # fix one metric and vary the other (2 param only)
# MORE OPTIONS NEEDED FOR THIS (see its code)
breed_plots = TRUE          # one breed plot per parameter value (1 param only)
log_plots = FALSE           # for varying p-infect only
export_plots = TRUE         # export plots or just display them

############################## DATA WRANGLING #################################
path = sprintf("results/%s", run_name)
pattern = sprintf("%s_", run_name) # date and test name

raw = read.csv(sprintf("%s.csv", path), 
               skip = 6, stringsAsFactors=FALSE, check.names = FALSE)

names(raw) = gsub("run number", "run_num", names(raw))
names(raw) = gsub("\\[|\\]|", "", names(raw))
names(raw) = gsub("\\.", " ", names(raw))
names(raw) = gsub("\\-", "_", names(raw))

# subset containing parameter information for each run
par = raw %>%
  select(-matches("count|num_contacts|step|get_age_bracket_data|active")) %>%
  distinct()

num_ticks = max(raw$step) # number of ticks/steps for xaxis

raw = raw %>%
  mutate(`count deceased` = select(., contains("deceased")) %>% 
           rowSums(na.rm = TRUE)) %>%
  mutate(`count infected` = select(., contains("\"infected")) %>% 
           rowSums(na.rm = TRUE))

summary = raw %>%
  group_by(run_num, !!as.name(varying_par)) %>%
  summarise(death_toll = max(`count deceased`), 
            peak_size = max(`count symptomatics`)) %>%
  pivot_longer(c("death_toll", "peak_size"),
               names_to = "metric",
               values_to = "count")

if (num_ticks > 365) {
  first_year = raw %>%
    group_by(run_num, !!as.name(varying_par)) %>%
    filter(step < 366) %>%
    summarise(year1_deaths = max(`count deceased`)) %>%
    pivot_longer("year1_deaths",
                 names_to = "metric",
                 values_to = "count")
  
  summary = rbind(summary, first_year) 
}

# various plotting information
num_runs = max(summary$run_num) # number of runs for ylabel
pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1] # population size

# formatted information on parameters varying and metrics in place
formatted_par = str_to_sentence(str_replace_all(varying_par, "_", " "))
formatted_metrics = str_to_sentence(str_replace_all(unique((summary$metric)), "_", " "))

######### PARAMETER VALUES vs DEATH TOLL AND PEAK SIZE 

if (metrics_plot) {
  
  if (length(varying_par) == 1) {
    
    summary_aggr = summary %>%
      group_by(!!as.name(varying_par), metric) %>%
      summarise(mean = mean(count), max = max(count), min = min(count))
    
    tick_names = unique(pull(summary_aggr[1]))
    
    ggplot(summary_aggr,
           aes(x=get(varying_par), y=mean, group = metric)) +
      geom_point(aes(color = metric)) + 
      geom_line(aes(color = metric), size = 1) +
      geom_ribbon(aes(ymin = min, ymax = max, fill = metric), alpha = 0.2) +
      coord_cartesian(ylim = c(0, max(summary_aggr$max))) +
      scale_x_continuous(breaks = tick_names) +
      scale_fill_manual(values = metrics_pal, labels = formatted_metrics) +
      scale_color_manual(values = metrics_pal, labels = formatted_metrics) +
      labs(x = sprintf("%s (%%)", formatted_par), y = "Mean count",
           fill = "", color = "",
           caption = sprintf("Calculated over %s simulation", num_runs)) +
      theme(plot.caption = element_text(hjust = 0),
            legend.title = element_text(size = 50),
            legend.text = element_text(size = 40),
            legend.key.size = unit(3,"line"),
            legend.position="top") +
      geom_vline(xintercept = optimal_value, linetype="dashed", 
                 color = "#666666", size = 1.5)
    
    
    if (export_plots) {
      ggsave(sprintf("%s/%sdeath-peak.pdf", dest_path, pattern), 
             width = g_width, height = g_height)
    }
  }
}

