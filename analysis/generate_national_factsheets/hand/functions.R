renderNationalMap <- function(category, selected, 
                              palette_selected = "YlOrBr",
                              output_asp_ratio = 0.45) {
  
  if(!is.character(category)) {
    stop("category must be a character string")
  }
  if(!is.character(selected)) {
    stop("selected must be a character string")
  }
  
  data <- eval(sym(str_remove(category, "^is_")))
  no_classes <- 4
  
  # p1
  legend_title <- paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1])
  
  quartiles <- quantile(data %>% filter(ABBR != "USA") %>% pull(!!sym(selected)),
                        probs = seq(0, 1, length.out = no_classes + 1),
                        na.rm = TRUE)
  
  labels <- setQuartileLabels(quartiles, 4, selected)
  
  # isCompVar
  display_type <- dict_vars %>% 
    filter(var_readable == selected, !!sym(category)) %>% 
    pull(display_type)
  
  is_comp <- ifelse(display_type == "comp", 
                    TRUE, FALSE)
  
  if(!is_comp){
    
    # single plot/standalone (plot_1)
    states_sf <- getUrbnGeo(data, selected, quartiles, 
                            labels, interactive = FALSE)
    makeGgplotObject(states_sf, legend_title, palette_selected) +
      theme(plot.background = element_rect(colour = "white"), 
            plot.title = element_text(face = "bold", 
                                      size = 10),
            panel.grid = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(t = 0, b = 0, 
                                 l = 2, r = 2, "cm"),
            aspect.ratio = output_asp_ratio,
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.spacing = unit(0L, "pt"),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", 
                                        vjust = 0.75),
            legend.text = element_text(vjust = .5),
            legend.key = element_rect(color = "black"),
            strip.text.x = element_text(size = 9L),
            text = element_text(size = 12))
    
    
  } else {
    
    # side-by-side plots/comp (plot_1, plot_2)
    # Define variables
    base_var <- dict_vars %>% 
      filter(var_readable == selected, !!sym(category)) %>% 
      pull(var_base)
    
    comp_var <- dict_vars %>%
      filter(var_base == base_var, var_readable != selected) %>%
      pull(var_readable)
    
    # Combine PWD and PWOD
    combined_var <- vctrs::vec_c(data %>% pull(!!sym(selected)),
                                 data %>% pull(!!sym(comp_var)))
    
    quartiles <- quantile(combined_var, 
                          probs = seq(0, 1, length.out = no_classes + 1),
                          na.rm = TRUE)
    
    labels <- setQuartileLabels(quartiles, 4, base_var)
    
    # Map title reworking
    legend_title_comp <- paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1])
    
    plot_1_title <- ifelse(str_detect(legend_title_comp,
                                      "People with Disabilities"),
                           "People with Disabilities",
                           "People without Disabilities")
    
    plot_2_title <- ifelse(plot_1_title == "People with Disabilities",
                           "People without Disabilities", 
                           "People with Disabilities")
    
    legend_title_comp <- str_replace_all(legend_title_comp,
                                         "People (with|without) Disabilities",
                                         "People with/without Disabilities")
    
    # plot_1
    states_sf <- getUrbnGeo(data, selected, quartiles, 
                            labels, interactive = FALSE)
    plot_1 <- makeGgplotObject(states_sf, 
                               legend_title_comp, 
                               palette_selected) +
      ggtitle(plot_1_title)
    
    # plot_2
    states_sf <- getUrbnGeo(data, comp_var, quartiles, 
                            labels, interactive = FALSE)
    plot_2 <- makeGgplotObject(states_sf, 
                               legend_title_comp, 
                               palette_selected) +
      ggtitle(plot_2_title)
    
    # Combine plots, legend elements
    plot_1 +
      plot_2 +
      plot_layout(ncol = 2, widths = 1,
                  guides = "collect") &
      theme(plot.background = element_rect(colour = "white"), 
            plot.title = element_text(face = "bold", 
                                      size = 10),
            panel.grid = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(t = 0, b = 0, 
                                 l = 2, r = 2, "cm"),
            aspect.ratio = output_asp_ratio,
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.spacing = unit(0L, "pt"),
            legend.position = "bottom",
            legend.title = element_text(face = "bold", 
                                        vjust = 0.75),
            legend.text = element_text(vjust = .5),
            legend.key = element_rect(color = "black"),
            strip.text.x = element_text(size = 9L),
            text = element_text(size = 12))
  }
}

getUrbnGeo <- function(data, selected, quartiles = NULL, labels = NULL, interactive = T) {
  
  df <- urbnmapr::get_urbn_map("territories_states", sf = TRUE) %>% 
    filter(!state_fips %in% c("60", "66", "69", "78")) %>% 
    select("ABBR" = state_abbv) %>% 
    inner_join(data %>% 
                 select(ABBR, !!sym(selected)) %>%
                 filter(!is.na(!!sym(selected))),
               by = "ABBR")
  
  if(interactive) {
    df %>%
      rowwise() %>% 
      mutate("hover_text" := ifelse(grepl("_pct$", 
                                          selected),
                                    paste0(round(!!sym(selected), 1), "%"),
                                    abbreviate_number(!!sym(selected))))
  } else if(!is.null(quartiles) & !is.null(labels)) {
    df %>%
      rowwise() %>% 
      mutate("quartile_fill" = cut(!!sym(selected), 
                                   breaks = quartiles, 
                                   labels = labels, 
                                   include.lowest = TRUE))
  } else {
    stop("Static plots require quartile and label values")
  }
  
}

makeGgplotObject <- function(states_sf, legend_title, palette_selected) {
  ggplot(states_sf) +
    geom_sf(aes(fill = quartile_fill),
            color = "black", size = 0.25) +
    scale_fill_brewer(palette = palette_selected,
                      limits = levels(states_sf$quartile_fill),
                      na.value = "grey") +
    # Text
    geom_sf_text(data = urbnmapr::get_urbn_labels(map = "territories_states", 
                                                  sf = TRUE) %>% 
                   filter(!state_fips %in% c("60", "66", "69", "78")),
                 aes(label = state_abbv),
                 size = 3, fontface = "bold", check_overlap = TRUE) +
    # Labels
    labs(x = "", y = "", 
         # title = title,
         fill = legend_title) +
    theme_void() +
    # Theme, removes all of the grid elements that we don't need
    theme(plot.background = element_rect(colour = "white"), 
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.spacing = unit(0L, "pt"),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", vjust = 0.75),
          legend.text = element_text(vjust = .5),
          legend.key = element_rect(color = "black"),
          strip.text.x = element_text(size = 9L),
          text = element_text(size = 12)) +
    guides(fill = guide_legend(label.position = "bottom"))
  
}

setQuartileLabels <- function(quartiles, no_classes, selected) {
  
  labels <- c()
  
  # Custom labels based on percent or value
  for(idx in 1:length(quartiles)){
    if(grepl("pct", selected)) {
      
      # Percent, add divide by 100, add symbol to text
      labels <- c(labels, paste0(scales::percent(quartiles[idx] / 100), 
                                 "-", 
                                 scales::percent(quartiles[idx + 1] / 100)))
      
    } else {
      
      # Values
      labels <- c(labels, paste0(scales::comma(quartiles[idx]), 
                                 "-", 
                                 scales::comma(quartiles[idx + 1])))
    }
  }
  # Remove last label which will have NA
  labels <- labels[1:length(labels)-1]
}

# Abbreviates values for large numbers in render_tile_map
abbreviateNumber <- function(x)
{
  x <- x / 1000000
  # print(x)
  
  if (x >= 1) {
    return(paste0(round(x, 1), "M"))
  } else {
    x <- x * 1000
    return(paste0(round(x, 0), "K"))
  }
}

altText <- function(data, variable) {
  
  # Selected data, format min/max for summary
  df <- data %>%
    select(NAME, ABBR, sym(variable)) %>%
    filter(ABBR != "USA", NAME != "United States")
  
  # Min
  text_min <- df %>%
    mutate("State" = paste0(NAME, " (", ABBR, ")")) %>% 
    select(State, sym(variable)) %>%
    filter(!is.na(!!sym(variable))) %>% 
    filter(!!sym(variable) == min(!!sym(variable))) %>% 
    slice(1) %>% 
    mutate(across(-State & -ends_with("_pct"),
                  ~scales::comma(.x))) %>% 
    mutate(across(ends_with("_pct"),
                  ~scales::percent(.x, 
                                   accuracy = 0.1,
                                   scale = 1))) %>% 
    mutate("summary_text" = paste0(" The lowest state or territory was ",
                                   State, " at ", 
                                   !!sym(variable), ".")) %>% 
    pull(summary_text)
  
  # Max
  text_max <- df %>%
    mutate("State" = paste0(NAME, " (", ABBR, ")")) %>% 
    select(State, sym(variable)) %>%
    filter(!is.na(!!sym(variable))) %>% 
    filter(!!sym(variable) == max(!!sym(variable))) %>% 
    slice(1) %>% 
    mutate(across(-State & -ends_with("_pct"),
                  ~scales::comma(.x))) %>% 
    mutate(across(ends_with("_pct"),
                  ~scales::percent(.x, 
                                   accuracy = 0.1,
                                   scale = 1))) %>% 
    mutate("summary_text" = paste0(" The highest state or territory was ",
                                   State, " at ", 
                                   !!sym(variable), ".")) %>% 
    pull(summary_text)
  
  # Title, vars_pretty field for variable
  title <- dict_vars %>% 
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>% 
    head(1) %>% 
    pull(national_dropdown_label)
  
  # Summary text for variable
  summary_text <- dict_vars %>% 
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>% 
    head(1) %>% 
    pull(national_summary_text)
  
  # Text for summary
  paste0(
    # "<b>", title, "</b><br>",
    summary_text, " ",
    # Min/Max
    text_min, text_max
  )
  
}


