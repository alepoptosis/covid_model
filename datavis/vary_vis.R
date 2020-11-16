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

run_name = "2020-11-14_vary-ld-compliance" # change name accordingly
varying_par = "testtrace_threshold" # use "_" instead of "-"
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
single_csv = TRUE

############################## DATA WRANGLING #################################
path = sprintf("results/%s", run_name)
pattern = sprintf("%s_", run_name) # date and test name

if (single_csv) {
  
  raw = read.csv(sprintf("%s.csv", path), 
                 skip = 6, stringsAsFactors=FALSE, check.names = FALSE)
  
  names(raw) = gsub("run number", "run_num", names(raw))
  names(raw) = gsub("\\[|\\]|", "", names(raw))
  names(raw) = gsub("\\.", " ", names(raw))
  names(raw) = gsub("\\-", "_", names(raw))
  
} else {
  # if the collated csv already exists, use it
  if (file.exists(sprintf("%s/%sfull.csv", path, pattern))) {
    
    raw = read.csv(sprintf("%s/%sfull.csv", path, pattern), 
                   stringsAsFactors=FALSE, check.names = FALSE)
    
  } else {
    # otherwise, get list of csvs
    csvs = list.files(path = path, pattern = pattern, full.names = TRUE)
    
    # merge in one csv and add a run ID while removing the useless one
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
}

# subset containing parameter information for each run
par = unique(
  raw[ ,grepl("^(?!.*(count |count_|num_contacts|step|dead|get_age_bracket_data|active|currently))", 
              names(raw), perl=TRUE)])

num_ticks = max(raw$step) # number of ticks/steps for xaxis

# summary plot with death toll and peak size
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

# if experiment ran for more than a year, add death toll for year 1
if (num_ticks > 365) {
  first_year = raw %>%
    group_by(run_num) %>%
    filter(step < 366) %>%
    summarise(year1_deaths = max(`count deads`)) %>%
    separate("run_num", sep = "_", remove = TRUE, 
             into = c("run_num", sprintf("%s", varying_par))) %>%
    mutate_at(c("run_num", sprintf("%s", varying_par)), as.numeric) %>%
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
  
######### HEATMAPS FOR COMBINATION OF 2 VARYING PARAMETERS
  
  if (length(varying_par) == 2) {
    summary_aggr = summary %>%
      group_by(!!as.name(varying_par[1]), !!as.name(varying_par[2]), metric) %>%
      summarise(mean = mean(count), stdev = sd(count))
    
    x_tick_names = unique(pull(summary_aggr[1]))
    y_tick_names = unique(pull(summary_aggr[2]))
    
    ggplot(subset(summary_aggr, metric == "death_toll"), 
           aes(x = get(varying_par[1]), y = get(varying_par[2]),
                        fill = mean)) +
      geom_tile(aes(fill = mean)) +
      geom_text(aes(label = sprintf("%s ± %s", round(mean, 0), round(stdev, 0))),
                size = 10) +
      scale_fill_viridis(limits = c(1400, 1550)) +
      scale_y_continuous(breaks = y_tick_names) +
      scale_x_continuous(breaks = x_tick_names) +
      theme(panel.grid.minor = element_blank()) +
      labs(x = "Population tested per day (%)",# sprintf("%s (%%)", formatted_par[1]), 
           y = "Symptomatic cases tested per day (%)",# sprintf("%s (%%)", formatted_par[2]), 
           fill = "Mean death toll\n",
           # title = "Death toll",           
           caption = sprintf("Calculated over %s simulation", num_runs)) +
      theme(plot.caption = element_text(hjust = 0), 
            legend.title = element_text(size = 30),
            legend.text = element_text(size = 40),
            legend.key.size = unit(3,"line"))
    
    if (export_plots) {
      ggsave(sprintf("%s/%sdeath_toll.pdf", dest_path, pattern), 
             width = g_width, height = g_height)
    }
    
    ggplot(subset(summary_aggr, metric == "peak_size"), 
           aes(x = get(varying_par[1]), y = get(varying_par[2]),
               fill = mean)) +
      geom_tile(aes(fill = mean)) +
      geom_text(aes(label = sprintf("%s ± %s", round(mean, 0), round(stdev, 0))),
                size = 10) +
      # scale_fill_viridis() +
      scale_y_continuous(breaks = y_tick_names) +
      scale_x_continuous(breaks = x_tick_names) +
      scale_fill_viridis(limits = c(3000, 10000)) +
      theme(panel.grid.minor = element_blank()) +
      labs(x = "Population tested per day (%)",# sprintf("%s (%%)", formatted_par[1]), 
           y = "Symptomatic cases tested per day (%)",# sprintf("%s (%%)", formatted_par[2]), 
           fill = "Mean peak size\n",
           # title = "Peak size",           
           caption = sprintf("Calculated over %s simulation", num_runs)) +
      theme(plot.caption = element_text(hjust = 0),
            legend.title = element_text(size = 30),
            legend.text = element_text(size = 40),
            legend.key.size = unit(3,"line"))
    
    if (export_plots) {
      ggsave(sprintf("%s/%speak_size.pdf", dest_path, pattern), 
             width = g_width, height = g_height)
    }
    
  }

######### BREEDS PLOT (ONE PER PARAMETER VALUE)

if (breed_plots) {
  
  measures = paste(unlist(str_to_sentence(colnames(par[, 3:8])[par[1, 3:8] == "TRUE"])), collapse = ", ")
  measures = gsub("[^[:alnum:][:blank:]+\\,]", " ", measures)
  measures = gsub(" ,", ",", measures)
  measures = gsub("control measures", "personal protection", measures)
  if (measures == "") {measures = "None"}
  
  data = raw[ ,grepl("^count|^step|^run_num|^currently", names(raw))]
  
  data = data %>% 
    select(-c(starts_with("count_infecteds_"), "count locked")) %>%
    separate("run_num", sep = "_", remove = TRUE, 
             into = c("run_num", sprintf("%s", varying_par))) %>% 
    pivot_longer (
      cols = starts_with("count"),
      names_to = "breed", 
      names_prefix = "count ",
      values_to = "count"
    )
  
  order = c("susceptibles", "exposeds", "asymptomatics",
            "symptomatics", "recovereds", "deads")
  
  data$breed = factor(data$breed, levels=order)
  
  split_data = split(data, with(data, get(varying_par)), drop = TRUE)
  param_values = names(split_data)
  list2env(split_data,envir=.GlobalEnv)
  
  for (df in param_values) {
    
    ld = get(df) %>% select(c("run_num", "step", "currently_locked?"))
    
    data_aggr = get(df) %>%
      group_by(step, breed) %>%
      summarise(mean = mean(count), stdev = sd(count),
                max = max(count), min = min(count))
    
    ggplot(data_aggr, aes(x=step, y=mean, group=breed)) +
      geom_area(data = ld,
                aes(x = step, y = pop_size * `currently_locked?`, 
                    fill = as.factor(run_num)),
                inherit.aes = FALSE, position=position_dodge(0), 
                alpha = 0.1, show.legend = FALSE) +
      scale_fill_manual(values = rep("lightgrey", num_runs)) +
      new_scale_fill() +
      geom_ribbon(aes(ymin=min, ymax=max, fill = breed), alpha=0.2) +
      geom_line(aes(color=breed), size = 1) +
      coord_cartesian(ylim = c(0, pop_size), xlim = c(0, num_ticks)) +
      scale_color_manual(values = pal, 
                         labels = substr(str_to_sentence(order), 1, nchar(order)-1)) +
      scale_fill_manual(values = pal, 
                        labels = substr(str_to_sentence(order), 1, nchar(order)-1)) +
      scale_y_continuous(labels = scales::unit_format(unit = "K", sep = "", 
                                                      scale = 1e-3), 
                         breaks = seq(0, pop_size, by = 30000)) +
      scale_x_continuous(breaks = seq(0, num_ticks, by = 365)) +
      labs(x = "Day", y = "Mean count",
           title = sprintf("Control measures: %s", measures),
           fill = "Breed", color = "Breed") +
      theme(plot.caption = element_text(hjust = 0),
            legend.title = element_text(size = 50),
            legend.text = element_text(size = 40),
            legend.key.size = unit(3,"line"))
    
    if (export_plots) {
      ggsave(sprintf("%s/%s%s_breeds.pdf", dest_path, pattern, df), 
             width = g_width, height = g_height)
    }
  }
}

######### FIX ONE METRIC AND VARY THE OTHER

if (fix_metric_plot) {
  focus_par = "sym_test_coverage" # which one of the parameters to vary
  fixed_value = 0 # value to fix other parameter at
  focus_formatted = str_to_sentence(str_replace_all(focus_par, "_", " "))
  
  summary_aggr = summary %>%
    filter(asym_test_coverage == 0) %>%
    group_by(!!as.name(focus_par), metric) %>%
    summarise(mean = mean(count), max = max(count), min = min(count))
  
  tick_names = unique(pull(summary_aggr[1]))
  
  ggplot(summary_aggr,
         aes(x=get(focus_par), y=mean, group = metric)) +
    geom_point(aes(color = metric)) +
    geom_line(aes(color = metric), size = 1) +
    geom_ribbon(aes(ymin = min, ymax = max, fill = metric), alpha = 0.2) +
    coord_cartesian(ylim = c(0, max(summary_aggr$max))) +
    scale_x_continuous(breaks = tick_names) +
    scale_fill_manual(values = metrics_pal, labels = formatted_metrics) +
    scale_color_manual(values = metrics_pal, labels = formatted_metrics) +
    labs(x = sprintf("%s (%%)", focus_formatted), y = "Mean count",
         fill = "Metric", color = "Metric",
         caption = sprintf("Calculated over %s simulation", num_runs)) +
    theme(plot.caption = element_text(hjust = 0))
  
  
  if (export_plots) {
    ggsave(sprintf("%s/%sfocus-%s.pdf", dest_path, pattern, focus_par),
           width = g_width, height = g_height)
  }
}
######### LOG PLOTS

if (log_plots) {
  
  data = raw[ ,grepl("^count symptomatics|^step|^run_num", names(raw))]
  
  data = data %>%
    separate("run_num", sep = "_", remove = TRUE, 
             into = c("run_num", sprintf("%s", varying_par)))
  
  split_data = split(data, with(data, get(varying_par)), drop = TRUE)
  param_values = str_sort(names(split_data), numeric = TRUE)
  list2env(split_data,envir=.GlobalEnv)
  
  plot_log_cases = function(data) {
    
    data_aggr = get(data) %>%
      group_by(step) %>%
      summarise(mean = mean(`count symptomatics`),
                max = max(`count symptomatics`), 
                min = min(`count symptomatics`))
    
    g = ggplot(data_aggr, aes(x=step, y=mean)) + 
      geom_line(color = "coral") +
      scale_y_continuous(trans="log10", 
                         labels = scales::number_format(accuracy = 0.01),
                         limits = c(NA, 100000)) +
      geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2, fill = "coral") +
      coord_cartesian(xlim = c(0, 30)) +
      scale_x_continuous(breaks = seq(0, 30, by = 7)) +
      labs(x = "Day", y = "Log10 of mean cases",
           title = sprintf("%s value: %s", varying_par, data))
  }
  plot_grid <- lapply(param_values, plot_log_cases)
  
  if (export_plots) {
    
    pdf(sprintf("%s/%slog-grid.pdf", dest_path, pattern), 
        width = g_width, height = g_height) 
    grid.arrange(grobs = plot_grid, ncol = 3)
    dev.off()
  }
}

