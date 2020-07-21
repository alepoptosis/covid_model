library(tidyverse)
library(RColorBrewer)
library(stringr)
library(ggnewscale)
theme_set(theme_minimal())

# second_max <-  function(x) {
#   u <- unique(x)
#   sort(u, decreasing = TRUE)[2L]
# }

# various snippets

# add horizontal line for threshold
# geom_hline(aes(yintercept = lockdown_thr), color = "red") + 
# geom_text(aes(num_ticks / 2 - 10, lockdown_thr, label = "lockdown threshold"),
#               vjust = -1, size = 3, color = "red")

# TO-DO: 
# - find a way to add a legend clarifying lockdown colour

# script options, change for different file, output options and plot size
run_name = "2020-07-17_pp-tt-ld-a05"
dest_path = "visualisations/a05"
g_width = 11.69
g_height = 8.27
export_plots = TRUE
new_ld_vis = TRUE

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
  
  write.csv(raw, sprintf("%s/%sfull.csv", path, pattern), row.names = FALSE)
}

# subset containing only data on counts, run number and step
data = raw[ ,grepl("^count |^step|^run_num", names(raw))]

# turns data into long format for plotting
data_long = data %>% pivot_longer (
  cols = starts_with("count"),
  names_to = "breed", 
  names_prefix = "count ",
  values_to = "count"
)

# aggregates data from all runs into an average count and stdev, min and max
data_aggr = data_long %>%
  group_by(step, breed) %>%
  summarise(mean = mean(count), stdev = round(sd(count), 2),
            max = max(count), min = min(count))

# remove lockdown info from other datasets
data_long = data_long %>% filter(breed != "locked")
data_aggr = data_aggr %>% filter(breed != "locked")

# subset containing parameter information for each run
par = unique(raw[ ,grepl("^(?!.*(count |count_|step|contacts|dead|currently))", 
                          names(raw), perl=TRUE)])

# save parameter file first time
if (!file.exists(sprintf("%s/%sparameters.par", path, pattern))) {
  write.csv(raw, sprintf("%s/%sparameters.par", path, pattern), row.names = FALSE)
}

# subset containing lockdown info

if ("currently_locked?" %in% colnames(raw)) {
  ld = raw[ ,grepl("run_num|step|locked", names(raw))]
} else { # for old files where currently_locked wasn't tracked
  pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1]
  
  if (par$`imposed_lockdown?`[1] == TRUE) {
    lockdown_thr = par$lockdown_threshold[1] * pop_size / 100
  } else {lockdown_thr = Inf}
  
  ld = data_long %>% 
    filter(breed == "symptomatics") %>%
    mutate(`currently_locked?` = count > lockdown_thr) %>%
    select(-c("breed", "count"))
}

ld_aggr = ld %>%
  select(-`count locked`) %>%
  group_by(step) %>%
  summarise(locked_runs = sum(`currently_locked?` == TRUE),
            locked_runs_per = (locked_runs * 100) / num_runs)

# subset containing info on contacts
contact = raw[ ,grepl("run_num|step|num_contacts", names(raw))]

# aggregated version of contact info
cont_aggr = contact %>%
  group_by(step) %>%
  summarise(mean = mean(num_contacts), stdev = sd(num_contacts),
            max = max(num_contacts), min = min(num_contacts))

# subset containing info on dead agents
deads = raw[ ,grepl("^dead|run_num|step|deads", names(raw))]

# turns data into long format for plotting
deads_long = deads %>% pivot_longer (
  cols = contains("dead"),
  names_to = "age",
  names_prefix = "dead_",
  values_to = "cum_count"
) %>%
  mutate_at("age", ~gsub("count deads", "total", .))

# adds column with new deaths per step
deads_long = deads_long %>%
  group_by(run_num,age) %>%
  mutate(new_deaths = c(0,diff(cum_count)))

# aggregated version of deads info
deads_aggr = deads_long %>%
  group_by(step, age) %>%
  summarise(mean = mean(cum_count), stdev = round(sd(cum_count), 2),
            max = max(cum_count), min = min(cum_count),
            mean_new = mean(new_deaths), stdev_new = round(sd(new_deaths), 2),
            max_new = max(new_deaths), min_new = min(new_deaths))

if ("count_infecteds_0_29" %in% colnames(raw)) {
  # subset containing info on infected agents
  infs = raw[ ,grepl("^count_infecteds_|run_num|step", names(raw))]
  
  infs = infs %>%
    group_by(run_num, step) %>%
    mutate(count_infecteds_total = sum(count_infecteds_0_29, 
                                       count_infecteds_30_59, 
                                       `count_infecteds_60+`))
  
  # turns data into long format for plotting
  infs_long = infs %>% pivot_longer (
    cols = starts_with("count_infecteds_"),
    names_to = "age",
    names_prefix = "count_infecteds_",
    values_to = "cum_count"
  )
  
  # adds column with total and new infections per step
  infs_long = infs_long %>%
    group_by(run_num,age) %>%
    mutate(new_infs = c(0,diff(cum_count)))
  
  # aggregated version of infecteds info
  infs_aggr = infs_long %>%
    group_by(step, age) %>%
    summarise(mean = mean(cum_count), stdev = round(sd(cum_count), 2),
              max = max(cum_count), min = min(cum_count),
              mean_new = mean(new_infs), stdev_new = round(sd(new_infs), 2),
              max_new = max(new_infs), min_new = min(new_infs))
}

##### various plotting information

# list of measures used in run
measures = paste(unlist(colnames(par[, 3:8])[par[1, 3:8] == "TRUE"]), collapse = ", ")
measures = gsub("[^[:alnum:][:blank:]+\\,]", " ", measures)
measures = gsub(" ,", ",", measures)
measures = gsub("control measures", "personal protection", measures)

num_runs = max(data_long$run_num) # num runs for ylabel
num_ticks = max(data_long$step) # num ticks for xaxis
max_cont = max(cont_aggr$mean) # max avg contacts for ylim (contacts plot)
pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1] # population size
peak_sym = round(max(data_aggr[data_aggr$breed == "symptomatics",]$mean), 2)
tot_deaths = round(max(data_aggr[data_aggr$breed == "deads",]$mean), 2)
if (num_ticks >= 365) {
  year1_deaths = round(data_aggr %>% 
                         filter(step == 365, breed == "deads") %>% 
                         pull(mean), 2)
} else {
  year1_deaths = "n/a"
}
if ("count_infecteds_0_29" %in% colnames(raw)) {
  tot_infs = round(max(infs_aggr$mean), 2)
} else {
  tot_infs = "n/a"
}

# set order of breeds for legend
order = c("susceptibles", "exposeds", "asymptomatics",
          "symptomatics", "recovereds", "deads") #, "locked")

data_long$breed = factor(data_long$breed, levels=order)
data_aggr$breed = factor(data_aggr$breed, levels=order)

#################################### PLOTS ####################################

######### BREEDS AVERAGE COUNT OVER TIME PLOT, NON-AGGR LOCKDOWN

if (!new_ld_vis) {
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
    scale_color_brewer(palette="Set3") +
    scale_fill_brewer(palette="Set3") +
    scale_y_continuous(labels = scales::unit_format(unit = "K", sep = "", 
                                                    scale = 1e-3), 
                       breaks = seq(0, max_cont, by = 30000)) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    labs(x = "day", y = sprintf("mean count over %s runs", num_runs),
         title = sprintf("Control measures: %s", measures),
         caption = sprintf("Average total deaths: %s \nAverage deaths in first year: %s \nAverage size of symptomatic peak: %s\nAverage number of infections: %s", 
                           tot_deaths, year1_deaths, peak_sym, tot_infs)) +
    theme(plot.caption = element_text(hjust = 0))
                           
  if (export_plots) {
  ggsave(sprintf("%s/%sbreeds.pdf", dest_path, pattern), 
       width = g_width, height = g_height)
  }
} else {
  ggplot(data_aggr, aes(x=step, y=mean, group=breed)) +
    geom_ribbon(aes(ymin=min, ymax=max, fill = breed), alpha=0.2) +
    geom_area(data = ld_aggr, aes(x = step, y = locked_runs_per*900), 
              inherit.aes = FALSE, fill = "lightgrey") +
    geom_line(aes(color=breed), size = 1) +
    coord_cartesian(ylim = c(0, pop_size), xlim = c(0, num_ticks)) +
    scale_color_brewer(palette="Set3") +
    scale_fill_brewer(palette="Set3") +
    scale_y_continuous(labels = scales::unit_format(unit = "K", sep = "", 
                                                    scale = 1e-3),
                       breaks = seq(0, max_cont, by = 30000),
                       sec.axis = sec_axis(~./900, name = "% runs in lockdown")) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    labs(x = "day", y = sprintf("mean count over %s runs", num_runs),
         title = sprintf("Control measures: %s", measures),
         caption = sprintf("Average total deaths: %s \nAverage deaths in first year: %s \nAverage size of symptomatic peak: %s\nAverage number of infections: %s", 
                           tot_deaths, year1_deaths, peak_sym, tot_infs)) +
    theme(plot.caption = element_text(hjust = 0))
  
  if (export_plots) {
    ggsave(sprintf("%s/%sbreeds-newld.pdf", dest_path, pattern), 
           width = g_width, height = g_height)
  }
}

######### NON-AGGR NEW DEATHS PLOT

ggplot(subset(deads_aggr, age != "total"), aes(x=step, y=mean_new)) + 
  geom_area(data = ld,
            aes(x = step, y = (max(deads_aggr$mean_new) + 1) * `currently_locked?`,
                fill = as.factor(run_num)),
            inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
            show.legend = FALSE) +
  scale_fill_manual(values = rep("lightgrey", num_runs)) +
  new_scale_fill() +
  geom_ribbon(aes(ymin=min_new, ymax=max_new, fill = age), alpha=0.5) +
  geom_line(aes(color=age), size = 1) +
  scale_color_brewer(palette="Set3") +
  scale_fill_brewer(palette="Set3") +
  scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
  coord_cartesian(ylim = c(- min(deads_aggr$mean_new), 
                             max(deads_aggr$mean_new)),
                  xlim = c(0, num_ticks)) +
  labs(x = "day", y = sprintf("mean new deaths over %s runs", num_runs),
       title = sprintf("Control measures: %s", measures),
       caption = sprintf("Average total deaths: %s \nAverage deaths in first year: %s", 
                         tot_deaths, year1_deaths)) +
       theme(plot.caption = element_text(hjust = 0))

if (export_plots) {
ggsave(sprintf("%s/%sdeaths.pdf", dest_path, pattern), 
       width = g_width, height = g_height)
}

######### NON-AGGR NEW INFECTIONS PLOT

if ("count_infecteds_0_29" %in% colnames(raw)) {
  
  ggplot(subset(infs_aggr, age != "total"), aes(x=step, y=mean_new)) + 
    geom_area(data = ld,
              aes(x = step, y = (max(infs_aggr$mean_new) + 1) * `currently_locked?`,
                  fill = as.factor(run_num)),
              inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
              show.legend = FALSE) +
    scale_fill_manual(values = rep("lightgrey", num_runs)) +
    new_scale_fill() +
    geom_ribbon(aes(ymin=min_new, ymax=max_new, fill = age), alpha=0.5) +
    geom_line(aes(color=age), size = 1) +
    scale_color_brewer(palette="Set3") +
    scale_fill_brewer(palette="Set3") +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    coord_cartesian(ylim = c(- min(infs_aggr$mean_new),
                             max(infs_aggr$mean_new)),
                    xlim = c(0, num_ticks)) +
    labs(x = "day", y = sprintf("mean new infections over %s runs", num_runs),
         title = sprintf("Control measures: %s", measures),
         caption = sprintf("Average size of symptomatic peak: %s \nAverage number of infections: %s", 
                           peak_sym, tot_infs)) +
    theme(plot.caption = element_text(hjust = 0))

  if (export_plots) {
    ggsave(sprintf("%s/%sinfections.pdf", dest_path, pattern), 
           width = g_width, height = g_height)
  }
}

######### CONTACTS OVER TIME PLOT

ggplot(cont_aggr, aes(x=step, y=mean)) +
  geom_area(data = ld,
          aes(x = step, y = max_cont * `currently_locked?`,
              fill = as.factor(run_num)),
          inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
          show.legend = FALSE) + 
    geom_line(size = 1, color = "orange") +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2, fill = "orange") +
    coord_cartesian(ylim = c(0, max_cont), xlim = c(0, num_ticks)) +
    scale_y_continuous(labels = scales::unit_format(unit = "K", sep = "", 
                                                    scale = 1e-3), 
                       breaks = seq(0, max_cont, by = 100000)) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    scale_fill_manual(values = rep("lightgrey", num_runs)) +
    labs(x = "day", y = sprintf("mean contacts over %s runs", num_runs),
         title = sprintf("Control measures: %s", measures))

if (export_plots) {
ggsave(sprintf("%s/%scontacts.pdf", dest_path, pattern), 
       width = g_width, height = g_height)
}