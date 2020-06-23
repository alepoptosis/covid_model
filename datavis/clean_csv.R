library(tidyverse)
library(RColorBrewer)
library(stringr)
library(ggnewscale)
theme_set(theme_minimal())

# various snippets

# final death count
# deads_aggr %>%
#   group_by(step) %>%
#   filter(step == 365) %>%
#   summarise(sum(mean))

# max count of a breed at any time
# max(data_aggr[data_aggr$breed == "symptomatics",]$mean)

# add horizontal line for threshold
# geom_hline(aes(yintercept = lockdown_thr), color = "red") + 
# geom_text(aes(num_ticks / 2 - 10, lockdown_thr, label = "lockdown threshold"),
#               vjust = -1, size = 3, color = "red")

# TO-DO: 
# - find a way to add a legend clarifying lockdown colour

############################## DATA WRANGLING #################################

# raw dataset (multiple csvs)
path = "results/2020-06-19_fast-weak"
pattern = "2020-06-19_fast-weak_" # date and test name

g_width = 11.69
g_height = 8.27

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
data = raw[ ,grepl("^count|^step|^run_num", names(raw))]

# subset containing parameter information for each run
par = unique(raw[ ,grepl("^(?!.*(count|step|contacts|dead))", 
                          names(raw), perl=TRUE)])

# thresholds information

if (par$`imposed_lockdown?`[1] == TRUE) {
  lockdown_thr = par$lockdown_threshold[1] * pop_size / 100
} else {lockdown_thr = Inf}

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
  summarise(mean = mean(count), stdev = sd(count),
            max = max(count), min = min(count))

# move lockdown info into its own df (non-aggregated)
ld = data_long %>% 
  filter(breed == "symptomatics") %>%
  mutate(lockdown = count > lockdown_thr) %>%
  select(-c("breed", "count"))

# remove lockdown info from other datasets
data_long = data_long %>% filter(breed != "lockdown")
data_aggr = data_aggr %>% filter(breed != "lockdown")

# subset containing info on contacts
contact = raw[ ,grepl("run_num|step|num_contacts", names(raw))]

# aggregated version of contact info
cont_aggr = contact %>%
  group_by(step) %>%
  summarise(mean = mean(num_contacts), stdev = sd(num_contacts),
            max = max(num_contacts), min = min(num_contacts))

# subset containing info on dead agents
deads = raw[ ,grepl("^dead|run_num|step", names(raw))]

# turns data into long format for plotting
deads_long = deads %>% pivot_longer (
  cols = starts_with("dead"),
  names_to = "age",
  names_prefix = "dead_",
  values_to = "count"
)

# adds column with new deaths per step
deads_long = deads_long %>%
  group_by(run_num,age) %>%
  mutate(new_deaths = c(0,diff(count)))

# aggregated version of deads info
deads_aggr = deads_long %>%
  group_by(step, age) %>%
  summarise(mean = mean(count), stdev = sd(count),
            max = max(count), min = min(count))

# various plotting information
pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1] # population size
num_runs = max(data_long$run_num) # num runs for ylabel
num_ticks = max(data_aggr$step) # num ticks for xaxis
max_cont = max(cont_aggr$mean) # max contacts for ylim (contacts plot)

# set order of breeds for legend
order = c("susceptibles", "latents", "asymptomatics",
          "symptomatics", "recovereds", "deads")

data_long$breed = factor(data_long$breed, levels=order)
data_aggr$breed = factor(data_aggr$breed, levels=order)

#################################### PLOTS ####################################

######### NON-AGGR BREED COUNTS OVER TIME

# non_aggr_plot = function(line_data, lock_data, breeds) {
#   ggplot(subset(line_data, breed %in% breeds),
#          aes(x=step, y=count)) +
#     geom_area(data = ld,
#               aes(x = step, y = pop_size * lockdown, fill = as.factor(run_num)),
#               inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
#               show.legend = FALSE) +
#     geom_line(aes(color=breed), alpha=0.7, size = 1) +
#     scale_color_brewer(palette="Set3") +
#     scale_fill_manual(values = rep("lightgrey", num_runs)) +
#     coord_cartesian(ylim = c(0, pop_size)) +
#     labs(x = "day", y = "count")
# }
# 
# non_aggr_plot(data_long, ld, c("deads", "recovereds", "susceptibles",
#                                "latents", "symptomatics", "asymptomatics"))
# non_aggr_plot(data_long, ld, c("deads", "recovereds", "susceptibles"))
# non_aggr_plot(data_long, ld, c("latents", "symptomatics", "asymptomatics"))

######### BREEDS AVERAGE COUNT OVER TIME PLOT, NON-AGGR LOCKDOWN

ggplot(data_aggr, aes(x=step, y=mean, group=breed)) +
  geom_area(data = ld,
            aes(x = step, y = pop_size * lockdown, 
                fill = as.factor(run_num)),
            inherit.aes = FALSE, position=position_dodge(0), 
            alpha = 0.1, show.legend = FALSE) +
    scale_fill_manual(values = rep("lightgrey", num_runs)) +
    new_scale_fill() +
    geom_ribbon(aes(ymin=min, ymax=max, fill = breed), alpha=0.2) +
    geom_line(aes(color=breed), size = 1) +
    coord_cartesian(ylim = c(0, pop_size), xlim = c(0, 182)) +
    scale_color_brewer(palette="Set3") +
    scale_fill_brewer(palette="Set3") +
    scale_y_continuous(labels = scales::unit_format(unit = "K", sep = "", 
                                                    scale = 1e-3), 
                       breaks = seq(0, max_cont, by = 30000)) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    labs(x = "day", y = sprintf("mean count over %s runs", num_runs))

ggsave(sprintf("vis scenarios/%sbreeds.pdf", pattern), 
       width = g_width, height = g_height)

######### NON-AGGR NEW DEATHS PLOT

g = ggplot(deads_long, aes(x=step, y=new_deaths)) + 
  geom_area(data = ld,
            aes(x = step, y = (max(deads_long$new_deaths) + 1) * lockdown,
                fill = as.factor(run_num)),
            inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
            show.legend = FALSE) +
    geom_line(aes(color=age), alpha=0.8, size = 1) +
    scale_color_brewer(palette="Set3") +
    scale_fill_manual(values = rep("lightgrey", num_runs)) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    coord_cartesian(ylim = c(- min(deads_long$new_deaths), 
                               max(deads_long$new_deaths)),
                    xlim = c(0, 182)) +
    labs(x = "day", y = "new deaths")

ggsave(sprintf("vis scenarios/%sdeaths.pdf", pattern), 
       width = g_width, height = g_height)

######### CONTACTS OVER TIME PLOT

g = ggplot(cont_aggr, aes(x=step, y=mean)) +
  geom_area(data = ld,
          aes(x = step, y = max_cont * lockdown,
              fill = as.factor(run_num)),
          inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
          show.legend = FALSE) + 
    geom_line(size = 1, color = "orange") +
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2, fill = "orange") +
    coord_cartesian(ylim = c(0, max_cont), xlim = c(0, 182)) +
    scale_y_continuous(labels = scales::unit_format(unit = "K", sep = "", 
                                                    scale = 1e-3), 
                       breaks = seq(0, max_cont, by = 100000)) +
    scale_x_continuous(breaks = seq(0, num_ticks, by = 30)) +
    scale_fill_manual(values = rep("lightgrey", num_runs)) +
    labs(x = "day", y = sprintf("mean contacts over %s runs", num_runs))

ggsave(sprintf("vis scenarios/%scontacts.pdf", pattern), 
       width = g_width, height = g_height)