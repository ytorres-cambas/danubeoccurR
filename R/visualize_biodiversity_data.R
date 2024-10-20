#' Visualize Biodiversity Data
#'
#' This function provides multiple visualizations for biodiversity occurrence data,
#' including occurrence maps, species richness by region, temporal trends,
#' species composition, and more. The user can specify the type of analysis and
#' the type of plot for visualization (e.g., points vs. heatmap, bar vs. map).
#'
#' @param data A data frame containing biodiversity occurrence data.
#' @param analysis_type The type of analysis to perform. Options include:
#'   \itemize{
#'     \item "map_occurrences" - Map occurrences as points or heatmap.
#'     \item "species_richness" - Species richness by region as bar chart or map.
#'     \item "temporal_trends" - Temporal trends in species occurrences as line or area chart.
#'     \item "species_composition" - Species composition by family/genus as bar or pie chart.
#'     \item "sampling_effort" - Sampling effort by location as histogram or heatmap.
#'     \item "species_abundance" - Species abundance across sites as boxplot or violin plot.
#'     \item "environment_vs_occurrence" - Scatterplot or smoothed relationship between environmental variables and species occurrence.
#'     \item "coverage_plot" - Data completeness or coverage by region over time as tile or line plot.
#'     \item "sampling_method_breakdown" - Breakdown of sampling methods as bar or pie chart.
#'     \item "rank_abundance" - Rank-abundance plot or abundance histogram.
#'   }
#' @param plot_type The type of plot to generate for the chosen analysis. Options depend on the `analysis_type`:
#'   \itemize{
#'     \item "map_occurrences" - Options: "points", "heatmap".
#'     \item "species_richness" - Options: "bar", "map".
#'     \item "temporal_trends" - Options: "line", "area".
#'     \item "species_composition" - Options: "bar", "pie".
#'     \item "sampling_effort" - Options: "histogram", "heatmap".
#'     \item "species_abundance" - Options: "boxplot", "violin".
#'     \item "environment_vs_occurrence" - Options: "scatter", "smooth".
#'     \item "coverage_plot" - Options: "tile", "line".
#'     \item "sampling_method_breakdown" - Options: "bar", "pie".
#'     \item "rank_abundance" - Options: "rank", "histogram".
#'   }
#' @param additional arguments depending on the specific analysis type. These could include column names for latitude, longitude, species, region, date, etc.
#'
#' @return A `ggplot2` object representing the requested visualization.
#'
#' @examples
#' \dontrun{
#' # Example: Map occurrences as points
#' visualize_biodiversity_data(data, analysis_type = "map_occurrences", plot_type = "points")
#'
#' # Example: Species richness as a bar chart
#' visualize_biodiversity_data(data, analysis_type = "species_richness", plot_type = "bar")
#' }
#'
#' @export
visualize_biodiversity_data <- function(data, analysis_type, plot_type, ...) {
  switch(analysis_type,
         "map_occurrences" = {
           map_occurrences(data, plot_type = plot_type, ...)
         },
         "species_richness" = {
           species_richness_by_region(data, plot_type = plot_type, ...)
         },
         "temporal_trends" = {
           temporal_trends(data, plot_type = plot_type, ...)
         },
         "species_composition" = {
           species_composition(data, plot_type = plot_type, ...)
         },
         "sampling_effort" = {
           sampling_effort(data, plot_type = plot_type, ...)
         },
         "species_abundance" = {
           species_abundance(data, plot_type = plot_type, ...)
         },
         "environment_vs_occurrence" = {
           environment_vs_occurrence(data, plot_type = plot_type, ...)
         },
         "coverage_plot" = {
           coverage_plot(data, plot_type = plot_type, ...)
         },
         "sampling_method_breakdown" = {
           sampling_method_breakdown(data, plot_type = plot_type, ...)
         },
         "rank_abundance" = {
           rank_abundance_plot(data, plot_type = plot_type, ...)
         },
         stop("Invalid analysis_type. Please provide a valid type.")
  )
}

# Sub-functions for each visualization

map_occurrences <- function(data, plot_type = "points", lat_col = "latitude", lon_col = "longitude") {
  if (plot_type == "points") {
    ggplot(data, aes_string(x = lon_col, y = lat_col)) +
      geom_point(color = "blue", alpha = 0.5) +
      theme_minimal() +
      labs(title = "Species Occurrence Map (Points)")
  } else if (plot_type == "heatmap") {
    ggplot(data, aes_string(x = lon_col, y = lat_col)) +
      stat_density2d(aes(fill = ..level..), geom = "polygon") +
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(title = "Species Occurrence Map (Heatmap)")
  } else {
    stop("Invalid plot_type. Use 'points' or 'heatmap'.")
  }
}

species_richness_by_region <- function(data, plot_type = "bar", region_col = "region", species_col = "species") {
  richness_data <- data %>%
    group_by(!!sym(region_col)) %>%
    summarise(species_richness = n_distinct(!!sym(species_col)))

  if (plot_type == "bar") {
    ggplot(richness_data, aes_string(x = region_col, y = "species_richness")) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      labs(title = "Species Richness by Region (Bar Chart)")
  } else if (plot_type == "map") {
    ggplot(richness_data, aes_string(fill = "species_richness")) +
      geom_sf() +
      theme_minimal() +
      labs(title = "Species Richness by Region (Map)")
  } else {
    stop("Invalid plot_type. Use 'bar' or 'map'.")
  }
}

temporal_trends <- function(data, plot_type = "line", date_col = "date") {
  data <- data %>%
    mutate(year = lubridate::year(!!sym(date_col)))

  trends_data <- data %>%
    group_by(year) %>%
    summarise(occurrences = n())

  if (plot_type == "line") {
    ggplot(trends_data, aes(x = year, y = occurrences)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(title = "Temporal Trends (Line Plot)")
  } else if (plot_type == "area") {
    ggplot(trends_data, aes(x = year, y = occurrences)) +
      geom_area(fill = "lightblue") +
      theme_minimal() +
      labs(title = "Temporal Trends (Area Plot)")
  } else {
    stop("Invalid plot_type. Use 'line' or 'area'.")
  }
}

species_composition <- function(data, plot_type = "bar", group_col = "family") {
  composition_data <- data %>%
    group_by(!!sym(group_col)) %>%
    summarise(count = n())

  if (plot_type == "bar") {
    ggplot(composition_data, aes_string(x = group_col, y = "count")) +
      geom_bar(stat = "identity", fill = "cyan") +
      theme_minimal() +
      labs(title = "Species Composition (Bar Chart)")
  } else if (plot_type == "pie") {
    ggplot(composition_data, aes(x = "", y = count, fill = !!sym(group_col))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Species Composition (Pie Chart)") +
      scale_fill_viridis_d()
  } else {
    stop("Invalid plot_type. Use 'bar' or 'pie'.")
  }
}

sampling_effort <- function(data, plot_type = "histogram", lat_col = "latitude", lon_col = "longitude") {
  if (plot_type == "histogram") {
    ggplot(data, aes_string(x = lon_col)) +
      geom_histogram(binwidth = 1, fill = "lightblue") +
      theme_minimal() +
      labs(title = "Sampling Effort (Histogram of Longitude)")
  } else if (plot_type == "heatmap") {
    ggplot(data, aes_string(x = lon_col, y = lat_col)) +
      stat_density2d(aes(fill = ..level..), geom = "polygon") +
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(title = "Sampling Effort (Heatmap)")
  } else {
    stop("Invalid plot_type. Use 'histogram' or 'heatmap'.")
  }
}

species_abundance <- function(data, plot_type = "boxplot", site_col = "site", species_col = "species") {
  if (plot_type == "boxplot") {
    ggplot(data, aes_string(x = site_col, y = species_col)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Species Abundance (Boxplot)")
  } else if (plot_type == "violin") {
    ggplot(data, aes_string(x = site_col, y = species_col)) +
      geom_violin() +
      theme_minimal() +
      labs(title = "Species Abundance (Violin Plot)")
  } else {
    stop("Invalid plot_type. Use 'boxplot' or 'violin'.")
  }
}

environment_vs_occurrence <- function(data, plot_type = "scatter", env_col = "temperature", species_col = "species") {
  if (plot_type == "scatter") {
    ggplot(data, aes_string(x = env_col, y = species_col)) +
      geom_point(alpha = 0.5, color = "darkblue") +
      theme_minimal() +
      labs(title = "Environment vs Species Occurrence (Scatterplot)")
  } else if (plot_type == "smooth") {
    ggplot(data, aes_string(x = env_col, y = species_col)) +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal() +
      labs(title = "Environment vs Species Occurrence (Smoothed Relationship)")
  } else {
    stop("Invalid plot_type. Use 'scatter' or 'smooth'.")
  }
}

coverage_plot <- function(data, plot_type = "tile", region_col = "region", year_col = "year") {
  coverage_data <- data %>%
    group_by(!!sym(region_col), !!sym(year_col)) %>%
    summarise(occurrences = n())

  if (plot_type == "tile") {
    ggplot(coverage_data, aes_string(x = region_col, y = year_col, fill = "occurrences")) +
      geom_tile() +
      theme_minimal() +
      labs(title = "Coverage Plot (Tile Plot)")
  } else if (plot_type == "line") {
    ggplot(coverage_data, aes_string(x = year_col, y = "occurrences", group = region_col, color = region_col)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Coverage Plot (Line Plot)")
  } else {
    stop("Invalid plot_type. Use 'tile' or 'line'.")
  }
}

sampling_method_breakdown <- function(data, plot_type = "bar", method_col = "method") {
  method_data <- data %>%
    group_by(!!sym(method_col)) %>%
    summarise(count = n())

  if (plot_type == "bar") {
    ggplot(method_data, aes_string(x = method_col, y = "count")) +
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_minimal() +
      labs(title = "Sampling Method Breakdown (Bar Chart)")
  } else if (plot_type == "pie") {
    ggplot(method_data, aes(x = "", y = count, fill = !!sym(method_col))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Sampling Method Breakdown (Pie Chart)") +
      scale_fill_viridis_d()
  } else {
    stop("Invalid plot_type. Use 'bar' or 'pie'.")
  }
}

rank_abundance_plot <- function(data, plot_type = "rank", species_col = "species") {
  rank_data <- data %>%
    group_by(!!sym(species_col)) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  if (plot_type == "rank") {
    ggplot(rank_data, aes(x = reorder(!!sym(species_col), -count), y = count)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      labs(title = "Rank-Abundance Plot")
  } else if (plot_type == "histogram") {
    ggplot(rank_data, aes(x = count)) +
      geom_histogram(binwidth = 1, fill = "cyan") +
      theme_minimal() +
      labs(title = "Abundance Histogram")
  } else {
    stop("Invalid plot_type. Use 'rank' or 'histogram'.")
  }
}
