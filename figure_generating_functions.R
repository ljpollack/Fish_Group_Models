
# this one needs some work, particularly in generating the initial conditions
marginal_effects_plot <- function(model, effects, color = effects[2]){
  initial_conditions = list()
  
  # loop through all of your listed effects, *inside* the model's data, and generate initial conditions based on the values in your data
  for (x in length(effects)) {
      initial_conditions[[effects[x]]] = unique(model$data[[effects[x]]])
    }
  
  # if you just have one effect, just use its name for effects
   if(length(effects) == 1){
    e <- marginal_effects(model, effects = effects, int_conditions = initial_conditions)
  } else {
  # otherwise, you have to paste your effect names together
  e <- marginal_effects(model, effects = paste0(effects[1],":",effects[2]), int_conditions = initial_conditions)
  
  # pull out the dataframe from the marginal effects call
  e <- e[[1]]
  e %>% 
    ggplot(aes(x= !!sym(effects[1]), y=estimate__, color= !!sym(color))) +
    geom_line() +
    geom_ribbon(aes(ymin=lower__, ymax=upper__, fill = !!sym(color)), color = "transparent", alpha = 0.1) +
    scale_fill_viridis_d() +
    scale_color_viridis_d()
  }
}

#p <- marginal_effects_plot(novel_fit, effects = c("treatment", "novel_food"))
#p + minimal_ggplot_theme()

marginal_effects_plot(model_6, effects = c("treatment", "trial"))

initial_conditions <- NULL
init_conditions <- NULL



param_estimate_plot <- function(model, num_params){
  p <- posterior_summary(novel_fit) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    head(n=num_params)
  p %>% 
    mutate(variable = rownames(p)) %>% 
    ggplot(aes(y=Estimate, x = variable))+
    geom_pointrange(aes(ymin=Q2.5, ymax=Q97.5, group=variable), size = 2/5, shape = 20) +
    geom_hline(yintercept=0, color = "gray25", alpha = 0.25) +
    coord_flip()
}

#p2 <- param_estimate_plot(novel_fit, 4)
#p2 + minimal_ggplot_theme()