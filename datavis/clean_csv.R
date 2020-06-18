library(tidyverse)
library(RColorBrewer)
library(stringr)
theme_set(theme_minimal())


# TO-DO: 
# - find a way to add a legend clarifying lockdown colour

############################## DATA WRANGLING #################################

# raw dataset (single csv)
# raw = read.csv("results/2020-06-12_sizetest2.csv", header = TRUE, skip = 6)

# raw dataset (multiple csvs)
path = "results"
pattern = "2020-06-15_sizetest2_" # date and test name

# get list of csvs
csvs = list.files(path = path, pattern = pattern, full.names = TRUE)

# merge in one csv and add a run_num id while removing the useless one
raw = csvs %>%
  set_names() %>%
  map_dfr( ~ read_csv(.x, col_types = cols(), skip = 6), 
           .id = "run_num", stringsAsFactors=FALSE) %>% 
  select(-"[run number]") %>%
  mutate_at("run_num", ~gsub(sprintf("%s/%s|.csv", path, pattern), "", .)) %>%
  mutate_at("run_num", as.numeric) %>%
  mutate_at("run_num", ~ run_num + 1)

# clean column names
names(raw) = gsub("\\[|\\]|", "", names(raw))
names(raw) = gsub("\\.", " ", names(raw))
names(raw) = gsub("\\-", "_", names(raw))

# subset containing only data on counts, run number and step
data = raw[ ,grepl("^count|^step|^run_num", names(raw))]

# subset containing parameter information for each run
par = unique(raw[ ,grepl("^(?!.*(count|step|contacts|dead))", 
                          names(raw), perl=TRUE)])

# turns data into long format for plotting
data_long = data %>% pivot_longer (
    cols = starts_with("count"),
    names_to = "breed", 
    names_prefix = "count ",
    values_to = "count"
    )

# set order of breeds for legend
order = c("susceptibles", "latents", "asymptomatics",
          "symptomatics", "recovereds", "deads", "lockdown")

data_long$breed = factor(data_long$breed, levels=order)

# aggregates data from all runs into an average count and stdev
data_aggr = data_long %>%
  group_by(step, breed) %>%
  summarise(mean = mean(count), stdev = sd(count)) %>%
  mutate(lower = mean - stdev, upper = mean + stdev)

# move lockdown info into its own df (non-aggregated)
ld = data_long %>% 
  filter(breed == "lockdown") %>%
  mutate(lockdown = count > 0) %>%
  select(-c("breed", "count"))

# move lockdown info into its own df (aggregated)
aggr_ld = data_aggr %>% 
  filter(breed == "lockdown") %>%
  mutate(lockdown = mean > 0) %>%
  select(c("step", "lockdown"))

# remove lockdown info from other datasets

data_long = data_long %>% filter(breed != "lockdown")
data_aggr = data_aggr %>% filter(breed != "lockdown")

# subset containing info on contacts
contact = raw[ ,grepl("run_num|step|num_contacts", names(raw))]

# aggregated version of contact info
cont_aggr = contact %>%
  group_by(step) %>%
  summarise(mean = mean(num_contacts), stdev = sd(num_contacts)) %>%
  mutate(lower = mean - stdev, upper = mean + stdev)

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
  summarise(mean = mean(count), stdev = sd(count)) %>%
  mutate(lower = mean - stdev, upper = mean + stdev)

# various plotting information
pop_size = ((par$max_pxcor + 1) * (par$max_pycor + 1))[1] # population size
num_runs = max(data_long$run_num) # num runs for ylabel
num_ticks = max(data_aggr$step) # num ticks for xaxis
max_cont = max(cont_aggr$mean) # max contacts for ylim (contacts plot)

########################### NON-AGGREGATED PLOTS ##############################

######### BREED COUNTS OVER TIME PLOT

non_aggr_plot = function(line_data, lock_data, breeds) {
  ggplot(subset(line_data, breed %in% breeds),
         aes(x=step, y=count)) +
    geom_area(data = ld,
              aes(x = step, y = pop_size * lockdown, fill = as.factor(run_num)),
              inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
              show.legend = FALSE) +
    geom_line(aes(color=breed), alpha=0.7, size = 1) +
    scale_color_brewer(palette="Set3") +
    scale_fill_manual(values = rep("lightgrey", num_runs)) +
    coord_cartesian(ylim = c(0, pop_size)) +
    labs(x = "day", y = "count")
}

non_aggr_plot(data_long, ld, c("deads", "recovereds", "susceptibles"))
non_aggr_plot(data_long, ld, c("latents", "symptomatics", "asymptomatics"))

# ggplot(subset(data_long, breed == "susceptibles"),
#        aes(x=step, y=count)) +
#   geom_area(data = ld,
#             aes(x = step, y = pop_size * lockdown, fill = as.factor(run_num)),
#             inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
#             show.legend = FALSE) +
#   geom_line(aes(color=breed), alpha=0.7, size = 1) +
#   scale_color_brewer(palette="Set3") +
#   scale_fill_manual(values = rep("lightgrey", num_runs)) +
#   coord_cartesian(ylim = c(0, pop_size))

######### NON-AGGR NEW DEATHS PLOT

ggplot(deads_long, aes(x=step, y=new_deaths)) +
  geom_area(data = ld,
            aes(x = step, y = max(deads_long$new_deaths) * lockdown,
                fill = as.factor(run_num)),
            inherit.aes = FALSE, position=position_dodge(0), alpha = 0.2,
            show.legend = FALSE) +
  geom_line(aes(color=age), alpha=0.8, size = 1) +
  scale_color_brewer(palette="Set3") +
  scale_fill_manual(values = rep("lightgrey", num_runs)) +
  labs(x = "day", y = "new deaths")

############################# AGGREGATED PLOTS ################################

######### BREEDS AVERAGE COUNT OVER TIME PLOT

ggplot(data_aggr, aes(x=step, y=mean, group=breed)) + 
  geom_area(data = aggr_ld, aes(x = step, y = pop_size * lockdown),
            inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
  geom_line(aes(color=breed), size = 1) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = breed), alpha=0.2) +
  coord_cartesian(ylim = c(0, pop_size)) +
  scale_color_brewer(palette="Set3") +
  scale_fill_brewer(palette="Set3") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, num_ticks, by = 365)) +
  labs(x = "day", y = sprintf("mean count over %s runs", num_runs))

######### AVERAGE CONTACTS OVER TIME PLOT

ggplot(cont_aggr, aes(x=step, y=mean)) +
  geom_area(data = aggr_ld, aes(x = step, y = max_cont * lockdown),
            inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
  geom_line(size = 1, color = "orange") +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, fill = "orange") +
  coord_cartesian(ylim = c(0, max_cont)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, num_ticks, by = 365)) +
  labs(x = "day", y = sprintf("mean contacts over %s runs", num_runs))

######### AVERAGE DEATH STATISTICS PLOT

ggplot(deads_aggr, aes(x=step, y=mean, group=age)) +
  geom_line(aes(color=age), size = 1) +
  geom_area(data = aggr_ld, aes(x = step, y = pop_size * lockdown),
            inherit.aes = FALSE, fill = "lightgray", alpha = 0.4) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = age), alpha=0.2) +
  coord_cartesian(ylim = c(0, max(deads_aggr$upper))) +
  scale_color_brewer(palette="Set3") +
  scale_fill_brewer(palette="Set3") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, num_ticks, by = 365)) +
  labs(x = "day", y = sprintf("mean death count over %s runs", num_runs))