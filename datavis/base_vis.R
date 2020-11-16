# BASIC RUN VISUALISATION SCRIPT: for runs with no parameter variations

packages <- c("tidyverse", "RColorBrewer", "ggnewscale")

for (pkg in packages){
  if (!require(package = pkg, character.only = TRUE)){
    install.packages(pkgs = pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

theme_set(theme_minimal(base_size = 40))
pal = c("#B3DE69", "#FFD92F", "#BEBADA", "#FC8D62", "#80B1D3", "#B3B3B3")

# script options, change for different file, output options and plot size

# names of experiments to visualise
# to_run = c(
#   "action-none-1y"
#   ,"action-all-1y")
# 
# # rest of the script is looped for each of the experiments
# for (run in to_run) {

run_name = sprintf("2020-11-15_no-controls")#, run) # change date accordingly
dest_path = "visualisations"             # folder for visualisations
g_width = 22                             # size of plots
g_height = 16
export_plots = TRUE                      # export plots or just display them
new_ld_vis = TRUE # updated way of showing proportion of runs in lockdown
single_csv = TRUE # whether the output was already collated (ran on one PC)

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
    
    write.csv(raw, sprintf("%s/%sfull.csv", path, pattern), row.names = FALSE)
  }
}

# subset containing only data on counts, run number and step
data = raw[ ,grepl("^count |^step|^run_num|deceased", names(raw))]

if (single_csv) { # new csvs with single output have separate deceased counts
  data = data %>%
    mutate(`count deceased` = select(., contains("deceased")) %>% 
             rowSums(na.rm = TRUE)) %>%
    select(-contains("get_age_bracket_data"))
}

# turn data into long format for plotting
data_long = data %>% pivot_longer (
  cols = starts_with("count"),
  names_to = "breed", 
  names_prefix = "count ",
  values_to = "count"
)
  

# aggregate data from all runs into an average count and stdev, min and max
data_aggr = data_long %>%
  group_by(step, breed) %>%
  summarise(mean = mean(count), stdev = round(sd(count), 2),
            max = max(count), min = min(count))

# remove lockdown info from other datasets
data_long = data_long %>% filter(breed != "locked")
data_aggr = data_aggr %>% filter(breed != "locked")

# subset containing parameter information for each run
par = unique(
  raw[ ,grepl("^(?!.*(count |count_|num_contacts|step|dead|get_age_bracket_data|active|currently))", 
                          names(raw), perl=TRUE)])

# save parameter file if it doesn't already exist
if (!single_csv & !file.exists(sprintf("%s/%sparameters.par", path, pattern))) {
  write.csv(raw, sprintf("%s/%sparameters.par", path, pattern), row.names = FALSE)
}

num_runs = max(data_long$run_num) # number of runs for ylabel

# subset containing lockdown info
if ("currently_locked?" %in% colnames(raw) | "lockdown_active?" %in% colnames(raw)) {
  ld = raw[ ,grepl("run_num|step|^count_locked|lockdown_active", names(raw))]
} else { 
  # for old files where currently_locked wasn't tracked
  pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1]
  
  if (par$`imposed_lockdown?`[1] == TRUE) {
    lockdown_thr = par$lockdown_threshold[1] * pop_size / 100
  } else {lockdown_thr = Inf}
  
  ld = data_long %>% 
    filter(breed == "symptomatics") %>%
    mutate(`currently_locked?` = count > lockdown_thr) %>%
    select(-c("breed", "count"))
}

# aggregate data from all runs into a percentage of locked runs per step

if (single_csv) {
  ld_aggr = ld %>%
    select(-`count_locked`) %>%
    group_by(step) %>%
    summarise(locked_runs = sum(`lockdown_active?` == TRUE),
              locked_runs_per = (locked_runs * 100) / num_runs)
} else {
  ld_aggr = ld %>%
    select(-`count_locked`) %>%
    group_by(step) %>%
    summarise(locked_runs = sum(`currently_locked?` == TRUE),
              locked_runs_per = (locked_runs * 100) / num_runs)
}


# subset containing info on contacts
contact = raw[ ,grepl("run_num|step|num_contacts", names(raw))]

# aggregated version of contact info
cont_aggr = contact %>%
  group_by(step) %>%
  summarise(mean = mean(num_contacts), stdev = sd(num_contacts),
            max = max(num_contacts), min = min(num_contacts))

# subset containing info on dead agents
deceased = raw[ ,grepl("^dead|run_num|step|deads|deceased", names(raw))]

# turn data into long format for plotting

if (single_csv) {
  deceased_long = deceased %>% pivot_longer (
    cols = contains("deceased"),
    names_to = "age",
    names_prefix = "get_age_bracket_data",
    values_to = "cum_count"
  ) %>%
    mutate_at("age", ~gsub("deceased", "", .)) %>%
    mutate_at("age", ~gsub("\"", "", .)) %>%
    mutate_at("age", ~gsub("_", "-", .))
  
} else {
  deceased_long = deceased %>% pivot_longer (
    cols = contains("dead"),
    names_to = "age",
    names_prefix = "dead_",
    values_to = "cum_count"
  ) %>%
  mutate_at("age", ~gsub("count deads", "total", .))
}

# add column with new deaths per step
deceased_long = deceased_long %>%
  group_by(run_num,age) %>%
  mutate(new_deaths = c(0,diff(cum_count)))

# aggregated version of deads info
deceased_aggr = deceased_long %>%
  group_by(step, age) %>%
  summarise(mean = mean(cum_count), stdev = round(sd(cum_count), 2),
            max = max(cum_count), min = min(cum_count),
            mean_new = mean(new_deaths), stdev_new = round(sd(new_deaths), 2),
            max_new = max(new_deaths), min_new = min(new_deaths))

if (single_csv) {
  
  # subset containing info on dead agents
  infs = raw[ ,grepl("run_num|step|\"infected", names(raw))]
  
  # turn data into long format for plotting
  infs_long = infs %>% pivot_longer (
    cols = contains("infected"),
    names_to = "age",
    names_prefix = "get_age_bracket_data",
    values_to = "cum_count"
  ) %>%
    mutate_at("age", ~gsub("infected", "", .)) %>%
    mutate_at("age", ~gsub("\"", "", .)) %>%
    mutate_at("age", ~gsub("_", "-", .))
  
  # add column with new deaths per step
  infs_long = infs_long %>%
    group_by(run_num,age) %>%
    mutate(new_infs = c(0,diff(cum_count)))
  
  # aggregated version of deads info
  infs_aggr = infs_long %>%
    group_by(step, age) %>%
    summarise(mean = mean(cum_count), stdev = round(sd(cum_count), 2),
              max = max(cum_count), min = min(cum_count),
              mean_new = mean(new_infs), stdev_new = round(sd(new_infs), 2),
              max_new = max(new_infs), min_new = min(new_infs))
  
} else {
  if ("count_infecteds_0_29" %in% colnames(raw)) {
    # if information is available, subset containing info on infected agents
    infs = raw[ ,grepl("^count_infecteds_|run_num|step", names(raw))]
    
    infs = infs %>%
      group_by(run_num, step) %>%
      mutate(count_infecteds_total = sum(count_infecteds_0_29, 
                                         count_infecteds_30_59, 
                                         `count_infecteds_60+`))
    
    # turn data into long format for plotting
    infs_long = infs %>% pivot_longer (
      cols = starts_with("count_infecteds_"),
      names_to = "age",
      names_prefix = "count_infecteds_",
      values_to = "cum_count"
    )
    
    # add column with total and new infections per step
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
}

##### various plotting information

# list of measures featured in experiment
measures = paste(unlist(str_to_sentence(colnames(par[, 3:8])[par[1, 3:8] == "TRUE"])), collapse = ", ")
measures = gsub("[^[:alnum:][:blank:]+\\,]", " ", measures)
measures = gsub(" ,", ",", measures)
measures = gsub("control measures", "personal protection", measures)
if (measures == "") {measures = "None"}

num_ticks = max(data_long$step) # number of ticks/steps for xaxis
max_cont = max(cont_aggr$mean) # max avg contacts for ylim (contacts plot)
pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1] # population size

##### CAPTION METRICS

# find out the size of the peak of known cases
peak_sym = pull(data_long %>%
  filter(breed == "symptomatics") %>%
  group_by(run_num) %>%
  summarise(sym_peak = max(count)) %>%
  group_by() %>%
  summarise(peak_sym = mean(sym_peak)))

# find out the total number of deaths
tot_deaths = pull(data_long %>%
  filter(breed == "deceased") %>%
  group_by(run_num) %>%
  summarise(tot_deaths = max(count)) %>%
  group_by() %>%
  summarise(tot_deaths = mean(tot_deaths)))

# find out the deaths in first year (if it ran for more than 1)
if (num_ticks >= 365) { 
  year1_deaths = round(data_aggr %>% 
                         filter(step == 365, breed == "deceased") %>% 
                         pull(mean), 2)
} else {
  year1_deaths = "n/a"
}

# if available, find out number of infections over entire run

if (single_csv) {
  tot_infs = round(max(infs_aggr$mean), 2)
} else {
  if ("count_infecteds_0_29" %in% colnames(raw)) {
    tot_infs = round(max(infs_aggr$mean), 2)
  } else {
    tot_infs = "n/a"
  }
}


# set order of breeds for legend
order = c("susceptibles", "exposeds", "asymptomatics",
          "symptomatics", "recovereds", "deceased") #, "locked")

data_long$breed = factor(data_long$breed, levels=order)
data_aggr$breed = factor(data_aggr$breed, levels=order)

#################################### PLOTS ####################################

######### AVERAGE COUNT PER BREED OVER TIME

if (!new_ld_vis) {
  # old version of lockdown visualisation
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
                       breaks = seq(0, max_cont, by = 30000)) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    labs(x = "Day", y = "Mean count",
         # title = sprintf("Control measures: %s", measures),
         fill = "Breed", color = "Breed",
         caption = sprintf("Average total deaths: %s \nAverage deaths in first year: %s \nAverage size of symptomatic peak: %s\nAverage number of infections: %s \nCalculated over %s simulations", 
                           tot_deaths, year1_deaths, peak_sym, tot_infs, num_runs)) +
    theme(plot.caption = element_text(hjust = 0),
          legend.title = element_text(size = 50),
          legend.text = element_text(size = 40),
          legend.key.size = unit(3,"line"))
                           
  if (export_plots) {
  ggsave(sprintf("%s/%sbreeds.pdf", dest_path, pattern), 
       width = g_width, height = g_height)
  }
} else {
  # new version of lockdown visualisation
  ggplot(data_aggr, aes(x=step, y=mean, group=breed)) +
    geom_ribbon(aes(ymin=min, ymax=max, fill = breed), alpha=0.2) +
    geom_area(data = ld_aggr, aes(x = step, y = locked_runs_per*900), 
              inherit.aes = FALSE, fill = "lightgrey") +
    geom_line(aes(color=breed), size = 1) +
    coord_cartesian(ylim = c(0, pop_size), xlim = c(0, num_ticks)) +
    scale_color_manual(values = pal, 
                       labels = order) +
    scale_fill_manual(values = pal, 
                      labels = order) +
    scale_y_continuous(labels = scales::unit_format(unit = "K", sep = "", 
                                                    scale = 1e-3), 
                       breaks = seq(0, max_cont, by = 30000)) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    labs(x = "Day", y = "Mean count",
         title = sprintf("Control measures: %s", measures),
         fill = "Breed", color = "Breed",
         caption = sprintf("Average total deaths: %s \nAverage deaths in first year: %s \nAverage size of symptomatic peak: %s\nAverage number of infections: %s \nCalculated over %s simulations", 
                           tot_deaths, year1_deaths, peak_sym, tot_infs, num_runs)) +
    theme(plot.caption = element_text(hjust = 0),
          legend.title = element_text(size = 50),
          legend.text = element_text(size = 40),
          legend.key.size = unit(3,"line"))
  
  if (export_plots) {
    ggsave(sprintf("%s/%sbreeds-newld.pdf", dest_path, pattern), 
           width = g_width, height = g_height)
  }
}

######### NEW DEATHS PER TICK PLOT, NON AGGREGATED

ggplot(subset(deceased_aggr, age != "total"), aes(x=step, y=mean_new)) + 
  geom_area(data = ld,
            aes(x = step, y = (max(deceased_aggr$mean_new) + 1) * `currently_locked?`,
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
  coord_cartesian(ylim = c(- min(deceased_aggr$mean_new), 
                             max(deceased_aggr$mean_new)),
                  xlim = c(0, num_ticks)) +
  labs(x = "Day", y = "Mean new deaths", fill = "Age range", color = "Age range",
       title = sprintf("Control measures: %s", measures),
       caption = sprintf("Average total deaths: %s \nAverage deaths in first year: %s\nCalculated over %s simulations", 
                         tot_deaths, year1_deaths, num_runs)) +
       theme(plot.caption = element_text(hjust = 0))

if (export_plots) {
ggsave(sprintf("%s/%sdeaths.pdf", dest_path, pattern), 
       width = g_width, height = g_height)
}

######### NEW INFECTIONS PER TICK PLOT, NON AGGREGATED

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
    labs(x = "Day", y = "Mean new infections", fill = "Age range", color = "Age range",
         title = sprintf("Control measures: %s", measures),
         caption = sprintf("Average size of symptomatic peak: %s \nAverage number of infections: %s\nCalculated over %s simulations", 
                           peak_sym, tot_infs, num_runs)) +
    theme(plot.caption = element_text(hjust = 0))

  if (export_plots) {
    ggsave(sprintf("%s/%sinfections.pdf", dest_path, pattern), 
           width = g_width, height = g_height)
  }
}

######### CONTACTS OVER TIME PLOT

ggplot(cont_aggr, aes(x=step, y=mean)) +
  geom_area(data = ld_aggr, aes(x = step, y = locked_runs_per*5500), 
            inherit.aes = FALSE, fill = "lightgrey", alpha = 0.7) +
    geom_line(size = 1, color = "orange") +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2, fill = "orange") +
    coord_cartesian(ylim = c(0, max_cont), xlim = c(0, num_ticks)) +
    scale_y_continuous(labels = scales::unit_format(unit = "K", sep = "", 
                                                    scale = 1e-3), 
                       breaks = seq(0, max_cont, by = 100000)) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    scale_fill_manual(values = rep("lightgrey", num_runs)) +
    labs(x = "Day", y = "Mean contacts",
         title = sprintf("Control measures: %s", measures),
         caption = sprintf("Calculated over %s simulations", num_runs)) +
    theme(plot.caption = element_text(hjust = 0))

if (export_plots) {
ggsave(sprintf("%s/%scontacts.pdf", dest_path, pattern), 
       width = g_width, height = g_height)
}
# }