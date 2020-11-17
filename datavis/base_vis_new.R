# BASIC RUN VISUALISATION SCRIPT: for runs with no parameter variations
# NEW: ONLY WORKS FOR REFACTORED MODEL

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

### UNCOMMENT THIS for a for loop and process multiple runs at once

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

############################## DATA WRANGLING #################################
path = sprintf("results/%s", run_name)
pattern = sprintf("%s_", run_name) # date and test name

# read csv and clean column names
raw = read.csv(sprintf("%s.csv", path), 
               skip = 6, stringsAsFactors=FALSE, check.names = FALSE)

names(raw) = gsub("run number", "run_num", names(raw))
names(raw) = gsub("\\[|\\]|", "", names(raw))
names(raw) = gsub("\\.", " ", names(raw))
names(raw) = gsub("\\-", "_", names(raw))

# subset containing only data on counts, run number and step
data = raw %>%
  select(matches("run_num|step|count|deceased"))

# collapse deceased counts by age into a single `count deceased`
data = data %>%
  mutate(`count deceased` = select(., contains("deceased")) %>% 
           rowSums(na.rm = TRUE)) %>%
  select(-contains("get_age_bracket_data"))

# turn data into long format for plotting
data_long = data %>% pivot_longer (
  cols = starts_with("count"),
  names_to = "breed", 
  names_prefix = "count |count_",
  values_to = "count"
)

# aggregate data from all runs into an average count and stdev, min and max
data_aggr = data_long %>%
  group_by(step, breed) %>%
  summarise(mean = mean(count), stdev = round(sd(count), 2),
            max = max(count), min = min(count))

# subset containing parameter information for each run
par = raw %>%
  select(-matches("count|num_contacts|step|get_age_bracket_data|active")) %>%
  distinct()

# subset containing lockdown info only
lockdown_info = raw %>%
  select(matches("run_num|step|lockdown_active"))

num_runs = max(data_long$run_num) # number of runs for ylabel

lockdown_aggr = lockdown_info %>%
  group_by(step) %>%
  summarise(locked_runs = sum(`lockdown_active?` == TRUE),
            locked_runs_per = (locked_runs * 100) / num_runs)

# subset containing contact info only
contacts = raw %>%
  select(matches("run_num|step|num_contacts"))

# aggregated version of contact info
contacts_aggr = contacts %>%
  group_by(step) %>%
  summarise(mean = mean(num_contacts), stdev = sd(num_contacts),
            max = max(num_contacts), min = min(num_contacts))

# subset containing info on dead agents
deceased = raw %>%
  select(matches("run_num|step|deceased"))

# turn data into long format for plotting
deceased_long = deceased %>% pivot_longer (
  cols = contains("deceased"),
  names_to = "age",
  names_prefix = "get_age_bracket_data",
  values_to = "cum_count"
) %>%
  mutate_at("age", ~gsub("deceased", "", .)) %>%
  mutate_at("age", ~gsub("\"", "", .)) %>%
  mutate_at("age", ~gsub("_", "-", .))

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

# subset containing info on dead agents
infected = raw %>%
  select(matches("run_num|step|\"infected"))

# turn data into long format for plotting
infected_long = infected %>% pivot_longer (
  cols = contains("infected"),
  names_to = "age",
  names_prefix = "get_age_bracket_data",
  values_to = "cum_count"
) %>%
  mutate_at("age", ~gsub("infected", "", .)) %>%
  mutate_at("age", ~gsub("\"", "", .)) %>%
  mutate_at("age", ~gsub("_", "-", .))

# add column with new deaths per step
infected_long = infected_long %>%
  group_by(run_num,age) %>%
  mutate(new_infs = c(0,diff(cum_count)))

# aggregated version of deads info
infected_aggr = infected_long %>%
  group_by(step, age) %>%
  summarise(mean = mean(cum_count), stdev = round(sd(cum_count), 2),
            max = max(cum_count), min = min(cum_count),
            mean_new = mean(new_infs), stdev_new = round(sd(new_infs), 2),
            max_new = max(new_infs), min_new = min(new_infs))

##### various plotting information

# list of measures featured in experiment
measures = paste(unlist(str_to_sentence(colnames(par[, 3:8])[par[1, 3:8] == "TRUE"])), collapse = ", ")
measures = gsub("[^[:alnum:][:blank:]+\\,]", " ", measures)
measures = gsub(" ,", ",", measures)
measures = gsub("control measures", "personal protection", measures)
if (measures == "") {measures = "None"}

num_ticks = max(data_long$step) # number of ticks/steps for xaxis
max_cont = max(contacts_aggr$mean) # max avg contacts for ylim (contacts plot)
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

# number of infections over entire run
tot_infs = round(max(infected_aggr$mean), 2)

# set order of breeds for legend
order = c("susceptibles", "exposeds", "asymptomatics",
          "symptomatics", "recovereds", "deceased", "locked")

data_long = data_long %>% filter(breed != "locked")
data_aggr = data_aggr %>% filter(breed != "locked")

data_long$breed = factor(data_long$breed, levels=order)
data_aggr$breed = factor(data_aggr$breed, levels=order)

######### AVERAGE COUNT PER BREED OVER TIME

ggplot(data_aggr, aes(x=step, y=mean, group=breed)) +
  geom_ribbon(aes(ymin=min, ymax=max, fill = breed), alpha=0.2) +
  geom_area(data = lockdown_aggr, aes(x = step, y = locked_runs_per*900), 
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

######### CONTACTS OVER TIME PLOT

ggplot(contacts_aggr, aes(x=step, y=mean)) +
  geom_area(data = lockdown_aggr, aes(x = step, y = locked_runs_per*5500), 
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

######### NEW DEATHS PER TICK PLOT, NON AGGREGATED

greens_sliced = brewer.pal(9, "Greens")[-seq_len(2)]

ggplot(deceased_aggr, aes(x=step, y=mean_new)) +
  geom_line(aes(color=age), size = 1) +
  scale_color_manual(values = greens_sliced) +
  scale_fill_manual(values = greens_sliced) +
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

reds_sliced = brewer.pal(9, "Reds")[-seq_len(2)]

ggplot(infected_aggr, aes(x=step, y=mean_new)) +
  geom_line(aes(color=age), size = 1) +
  scale_color_manual(values = reds_sliced) +
  scale_fill_manual(values = reds_sliced) +
  scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
  coord_cartesian(ylim = c(- min(infected_aggr$mean_new),
                           max(infected_aggr$mean_new)),
                  xlim = c(0, num_ticks)) +
  labs(x = "Day", y = "Mean new infections", fill = "Age range", color = "Age range",
       title = sprintf("Control measures: %s", measures),
       caption = sprintf("Average size of symptomatic peak: %s \nAverage number of infections: %s\nCalculated over %s simulations", 
                         peak_sym, tot_infs, num_runs)) +
  theme(plot.caption = element_text(hjust = 0))

if (export_plots) {
  ggsave(sprintf("%s/%sdeaths.pdf", dest_path, pattern), 
         width = g_width, height = g_height)
}