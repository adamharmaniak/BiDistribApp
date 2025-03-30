source("global.R")

# Funkcie

identify_variables <- function(data) {
  variable_types <- sapply(data, function(col) {
    if (is.factor(col) || is.character(col) || (is.numeric(col) && length(unique(col)) < 10)) {
      return("Diskretna")
    } else {
      return("Spojita")
    }
  })
  
  discrete_vars <- names(variable_types[variable_types == "Diskretna"])
  continuous_vars <- names(variable_types[variable_types == "Spojita"])
  
  return(list(Diskretne = discrete_vars, Spojite = continuous_vars))
}

printVariables <- function(data) {
  num_variables <- length(colnames(data))
  variables <- identify_variables(data)
  
  all_variables <- c(variables$Diskretne, variables$Spojite)
  
  variable_types <- c(rep("Discrete", length(variables$Diskretne)),
                      rep("Continuous", length(variables$Spojite)))
  
  counts <- sapply(all_variables, function(var) sum(!is.na(data[[var]])))
  
  tibble(
    Index = seq_len(num_variables),
    Variable_Name = all_variables,
    Variable_Type = variable_types,
    Pocet_pozorovani = counts
  )
}

mixture_joint_distribution <- function(data, discrete_vars, continuous_vars, model_type, bw, plot_type) {
  
  # Modelovanie hustoty pouzitim jadroveho vyhladzovania
  if (model_type == "kernel") {
    
    # Konverzia diskretnej premennej na faktor a zoradenie kategorii
    data[[discrete_vars]] <- factor(data[[discrete_vars]])
    categories <- levels(data[[discrete_vars]])
    
    # Vytvorenie farebnej mapy pre kategorie
    category_colors <- setNames(RColorBrewer::brewer.pal(length(categories), "Set1"), categories)
    
    # Vypocet pravdepodobnosti pre kazdu kategoriu (váhy v zmesi)
    category_probs <- prop.table(table(data[[discrete_vars]]))
    
    # Vypocet globalneho bw (pre 2D aj 3D)
    if (is.null(bw)) {
      global_bw <- bw.nrd0(data[[continuous_vars]])
    } else {
      global_bw <- bw
    }
    
    # Funkcia na vypocet hustoty pre 3D s normovanim podľa vahy triedy
    calculate_density_3D <- function(data, category) {
      sub_data <- filter(data, .data[[discrete_vars]] == category)
      
      if (nrow(sub_data) > 1) {
        
        # Vypocet lokalneho bw pre kazdu kategoriu (ak nie je dane globalne)
        if (is.null(bw)) {
          bw_cat <- bw.nrd0(sub_data[[continuous_vars]])
        } else {
          bw_cat <- bw
        }
        
        kde_x <- density(sub_data[[continuous_vars]], bw = bw_cat)
        kde_x_fun <- approxfun(kde_x$x, kde_x$y, rule = 2)
        
        x <- seq(min(data[[continuous_vars]]), max(data[[continuous_vars]]), length.out = 100)
        x_density <- kde_x_fun(x)
        
        # Normovanie hustoty podla pravdepodobnosti kategorie
        weighted_density <- x_density * category_probs[[category]]
        
        return(data.frame(Weight = x, Density = weighted_density, Category = category))
      } else {
        return(data.frame(Weight = numeric(0), Density = numeric(0), Category = character(0)))
      }
    }
    
    # Spojenie hustot pre vsetky kategorie
    density_data <- bind_rows(lapply(categories, function(cat) calculate_density_3D(data, cat)))
    
    # Celkove overenie suctu plochy pod hustotou
    # Trapezove pravidlo na vypocet integralu pre kazdu kategoriu
    integral_check <- sapply(categories, function(cat) {
      sub_density <- filter(density_data, Category == cat)
      if (nrow(sub_density) > 1) {
        dx <- diff(sub_density$Weight)[1]
        sum(sub_density$Density) * dx
      } else {
        0
      }
    })
    
    # Sucet plosnych integralov
    total_integral <- sum(integral_check)
    
    message("Kontrola normovania zmesi hustoty: Celkovy integral = ", round(total_integral, 4))
    
    
    if (plot_type == "3D") {
      # 3D Vizualizacia
      fig_3d <- plot_ly(
        density_data,
        x = ~Weight, y = ~Category, z = ~Density,
        type = 'scatter3d', mode = 'lines',
        color = ~Category, colors = category_colors, line = list(width = 4)
      ) %>%
        layout(scene = list(
          xaxis = list(title = continuous_vars),
          yaxis = list(title = discrete_vars),
          zaxis = list(title = 'Hustota')
        ))
      
      return(fig_3d)
      
    } else if (plot_type == "2D") {
      # 2D Vizualizacia
      data[[discrete_vars]] <- factor(data[[discrete_vars]], levels = categories)
      
      scatter_plot <- ggplot(data, aes_string(x = continuous_vars, y = discrete_vars, color = discrete_vars)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(x = continuous_vars, y = discrete_vars) +
        scale_color_manual(values = category_colors) +
        theme_minimal() +
        theme(legend.position = "right")
      
      # Prepocet, lebo geom_density() neberie priamo bw
      reference_bw <- bw.nrd0(data[[continuous_vars]])
      adjust_factor <- global_bw / reference_bw
      
      x_density <- ggplot(data, aes_string(x = continuous_vars, fill = discrete_vars, color = discrete_vars)) +
        geom_density(alpha = 0.5, adjust = adjust_factor) +
        scale_fill_manual(values = category_colors) +
        scale_color_manual(values = category_colors) +
        labs(x = NULL, y = paste("Hustota (", continuous_vars, ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "right")
      
      y_bar <- ggplot(data, aes_string(x = discrete_vars, fill = discrete_vars)) +
        geom_bar(alpha = 0.7) +
        coord_flip() +
        scale_fill_manual(values = category_colors) +
        labs(x = NULL, y = paste("P(", discrete_vars, ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "right")
      
      final_plot_2d <- (x_density + plot_spacer()) /
        (scatter_plot + y_bar) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d)
      
    }
  }
  
  # Modelovanie hustoty ako hustoty normalneho rozdelenia (dnorm)
  if (model_type == "parametric") {
    
    # Konverzia diskretnej premennej na faktor a zoradenie kategorii
    data[[discrete_vars]] <- factor(data[[discrete_vars]])
    categories <- levels(data[[discrete_vars]])
    
    # Pravdepodobnosti vyskytu pre jednotlive kategorie
    category_probs <- prop.table(table(data[[discrete_vars]]))
    
    # Vytvorenie farebnej mapy pre kategorie
    category_colors <- RColorBrewer::brewer.pal(length(categories), "Set1")
    names(category_colors) <- categories
    
    # Vypocet vazenej hustoty pre kazdu kategoriu
    calculate_density_3D <- function(data, category) {
      sub_data <- filter(data, .data[[discrete_vars]] == category)
      
      if (nrow(sub_data) > 1) {
        mu <- mean(sub_data[[continuous_vars]])
        sigma <- sd(sub_data[[continuous_vars]])
        
        x <- seq(min(data[[continuous_vars]]), max(data[[continuous_vars]]), length.out = 100)
        density <- dnorm(x, mean = mu, sd = sigma)
        
        # Normovanie hustoty podla vahy kategorie
        weighted_density <- density * category_probs[[category]]
        
        return(data.frame(
          Continuous_Var = x,
          Density = weighted_density,
          Discrete_Var = factor(category, levels = categories)
        ))
      } else {
        return(data.frame(
          Continuous_Var = numeric(0),
          Density = numeric(0),
          Discrete_Var = factor(character(0), levels = categories)
        ))
      }
    }
    
    # Spojenie hustot pre vsetky kategorie
    density_data <- bind_rows(lapply(categories, function(cat) calculate_density_3D(data, cat)))
    
    # Kontrola celkoveho integralu
    integral_check <- sapply(categories, function(cat) {
      sub_density <- filter(density_data, Discrete_Var == cat)
      if (nrow(sub_density) > 1) {
        dx <- diff(sub_density$Continuous_Var)[1]
        sum(sub_density$Density) * dx
      } else {
        0
      }
    })
    
    total_integral <- sum(integral_check)
    
    message("Kontrola normovania zmesi hustoty: Celkovy integral = ", round(total_integral, 4))
    
    if (plot_type == "3D") {
      # 3D Vizualizacia
      fig_3d <- plot_ly(
        density_data,
        x = ~Continuous_Var,
        y = ~Discrete_Var,
        z = ~Density,
        type = 'scatter3d',
        mode = 'lines',
        color = ~Discrete_Var,
        colors = category_colors,
        line = list(width = 4)
      ) %>%
        layout(scene = list(
          xaxis = list(title = continuous_vars),
          yaxis = list(title = discrete_vars),
          zaxis = list(title = 'Hustota')
        ))
      
      return(fig_3d)
      
    } else if (plot_type == "2D") {
      # 2D Vizualizacia
      grouped_stats <- data %>%
        group_by(.data[[discrete_vars]]) %>%
        summarise(
          mean_x = mean(.data[[continuous_vars]], na.rm = TRUE),
          sd_x = sd(.data[[continuous_vars]], na.rm = TRUE),
          .groups = "drop"
        )
      
      x_vals <- seq(min(data[[continuous_vars]], na.rm = TRUE),
                    max(data[[continuous_vars]], na.rm = TRUE),
                    length.out = 100)
      
      # Hustoty pre kazdu kategoriu s vazenim pravdepodobnosti
      density_data_2d <- grouped_stats %>%
        mutate(prob = category_probs[as.character(.data[[discrete_vars]])]) %>%
        expand_grid(x = x_vals) %>%
        mutate(y = dnorm(x, mean = mean_x, sd = sd_x) * prob)
      
      scatter_plot <- ggplot(data, aes_string(x = continuous_vars, y = discrete_vars, color = discrete_vars)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(x = continuous_vars, y = discrete_vars) +
        theme_minimal() +
        theme(legend.position = "right") +
        scale_color_manual(values = category_colors)
      
      x_density <- ggplot(density_data_2d,
                          aes(x = x, y = y,
                              color = as.factor(.data[[discrete_vars]]),
                              group = .data[[discrete_vars]])) +
        geom_line(size = 1) +
        scale_color_manual(values = category_colors) +
        labs(x = NULL,
             y = paste("Hustota (", continuous_vars, ")", sep = ""),
             color = discrete_vars) +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.position = "right")
      
      y_bar <- ggplot(data, aes_string(x = discrete_vars, fill = discrete_vars)) +
        geom_bar(alpha = 0.7) +
        coord_flip() +
        scale_fill_manual(values = category_colors) +
        labs(x = NULL, y = paste("P(", discrete_vars, ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "right")
      
      final_plot_2d <- (x_density + plot_spacer()) /
        (scatter_plot + y_bar) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d)
      
    }
  }
  
  # Modelovanie hustoty ako hustoty Studentovho t-rozdelenia (dt)
  if (model_type == "t") {
    
    # Konverzia diskretnej premennej na faktor a zoradenie kategorii
    data[[discrete_vars]] <- factor(data[[discrete_vars]])
    categories <- levels(data[[discrete_vars]])
    
    # Pravdepodobnosti vyskytu pre jednotlive kategorie
    category_probs <- prop.table(table(data[[discrete_vars]]))
    
    # Vytvorenie farebnej mapy pre kategorie
    category_colors <- RColorBrewer::brewer.pal(length(categories), "Set1")
    names(category_colors) <- categories
    
    # Vypocet vazenej hustoty pre kazdu kategoriu
    calculate_density_3D_t <- function(data, category) {
      sub_data <- filter(data, .data[[discrete_vars]] == category)
      
      if (nrow(sub_data) > 2) {
        mu <- mean(sub_data[[continuous_vars]])
        sigma <- sd(sub_data[[continuous_vars]])
        df_t <- nrow(sub_data) - 1  # stupne volnosti pre t-rozdelenie
        
        x <- seq(min(data[[continuous_vars]]), max(data[[continuous_vars]]), length.out = 100)
        
        # Hustota t-rozdelenia a jej normovanie podla smerodajnej odchylky
        density <- dt((x - mu) / sigma, df = df_t) / sigma
        
        # Vazenie hustoty pravdepodobnostou vyskytu kategorie
        weighted_density <- density * category_probs[[category]]
        
        return(data.frame(
          Continuous_Var = x,
          Density = weighted_density,
          Discrete_Var = factor(category, levels = categories)
        ))
      } else {
        return(data.frame(
          Continuous_Var = numeric(0),
          Density = numeric(0),
          Discrete_Var = factor(character(0), levels = categories)
        ))
      }
    }
    
    # Spojenie hustot pre vsetky kategorie
    density_data_t <- bind_rows(lapply(categories, function(cat) calculate_density_3D_t(data, cat)))
    
    # Kontrola normovania zmesi
    integral_check_t <- sapply(categories, function(cat) {
      sub_density <- filter(density_data_t, Discrete_Var == cat)
      if (nrow(sub_density) > 1) {
        dx <- diff(sub_density$Continuous_Var)[1]
        sum(sub_density$Density) * dx
      } else {
        0
      }
    })
    
    total_integral_t <- sum(integral_check_t)
    
    message("Kontrola normovania zmesi t-hustoty: Celkovy integral = ", round(total_integral_t, 4))
    
    
    if (plot_type == "3D") {
      # 3D Vizualizacia
      fig_3d_t <- plot_ly(
        density_data_t,
        x = ~Continuous_Var,
        y = ~Discrete_Var,
        z = ~Density,
        type = 'scatter3d',
        mode = 'lines',
        color = ~Discrete_Var,
        colors = category_colors,
        line = list(width = 4)
      ) %>%
        layout(scene = list(
          xaxis = list(title = continuous_vars),
          yaxis = list(title = discrete_vars),
          zaxis = list(title = 'Hustota')
        ))
      
      return(fig_3d_t)
      
    } else if (plot_type == "2D") {
      # 2D Vizualizacia
      grouped_stats_t <- data %>%
        group_by(.data[[discrete_vars]]) %>%
        summarise(
          mean_x = mean(.data[[continuous_vars]], na.rm = TRUE),
          sd_x = sd(.data[[continuous_vars]], na.rm = TRUE),
          df_x = n() - 1,
          .groups = "drop"
        )
      
      x_vals <- seq(min(data[[continuous_vars]], na.rm = TRUE),
                    max(data[[continuous_vars]], na.rm = TRUE),
                    length.out = 100)
      
      density_data_t_2d <- grouped_stats_t %>%
        mutate(prob = category_probs[as.character(.data[[discrete_vars]])]) %>%
        expand_grid(x = x_vals) %>%
        mutate(y = dt((x - mean_x) / sd_x, df = df_x) / sd_x * prob)
      
      scatter_plot_t <- ggplot(data, aes_string(x = continuous_vars, y = discrete_vars, color = discrete_vars)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(x = continuous_vars, y = discrete_vars) +
        theme_minimal() +
        theme(legend.position = "right") +
        scale_color_manual(values = category_colors)
      
      x_density_t <- ggplot(density_data_t_2d,
                            aes(x = x, y = y,
                                color = as.factor(.data[[discrete_vars]]),
                                group = .data[[discrete_vars]])) +
        geom_line(size = 1) +
        scale_color_manual(values = category_colors) +
        labs(x = NULL,
             y = paste("Hustota (", continuous_vars, ")", sep = ""),
             color = discrete_vars) +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.position = "right")
      
      y_bar_t <- ggplot(data, aes_string(x = discrete_vars, fill = discrete_vars)) +
        geom_bar(alpha = 0.7) +
        coord_flip() +
        scale_fill_manual(values = category_colors) +
        labs(x = NULL, y = paste("P(", discrete_vars, ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "right")
      
      final_plot_2d_t <- (x_density_t + plot_spacer()) /
        (scatter_plot_t + y_bar_t) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d_t)
      
    }
  }
}

continuous_joint_distribution <- function(data, continuous_vars, model_type, plot_type) {
  
  # Modelovanie hustoty pomocou jadroveho vyhladzovania
  if (model_type == "kernel"){
    kde_result <- kde2d(
      x = data[[continuous_vars[1]]], 
      y = data[[continuous_vars[2]]], 
      n = 100
    )
    
    if (plot_type == "3D") {
      fig_3d <- plot_ly(
        x = ~kde_result$x, y = ~kde_result$y, z = ~kde_result$z,
        type = "surface",
        colors = colorRamp(c("blue", "cyan", "yellow", "red"))
      ) %>% layout(
        title = paste("Združená hustota", continuous_vars[1], "a", continuous_vars[2], "(jadrové   vyhladzovanie)"),
        scene = list(
          xaxis = list(title = continuous_vars[1]),
          yaxis = list(title = continuous_vars[2]),
          zaxis = list(title = "Hustota")
        )
      )
      
      return(fig_3d)
      
    } else if (plot_type == "2D") {
      kde_df <- data.frame(
        expand.grid(x = kde_result$x, y = kde_result$y),
        z = as.vector(kde_result$z)
      )
      
      scatter_plot <- ggplot(data, aes_string(x = continuous_vars[1], y = continuous_vars[2])) +
        geom_point(size = 3, alpha = 0.7, aes_string(color = continuous_vars[2])) +  
        geom_contour(data = kde_df, aes(x = x, y = y, z = z), color = "black") +
        labs(
          x = continuous_vars[1],
          y = continuous_vars[2],
          title = paste("Scatter plot s vrstevnicami (jadrové vyhladzovanie)")
        ) +
        scale_color_gradient(low = "blue", high = "red") +
        theme_minimal()
      
      density_x <- ggplot(data, aes_string(x = continuous_vars[1])) +
        geom_density(fill = "blue", alpha = 0.5) +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[1], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      density_y <- ggplot(data, aes_string(x = continuous_vars[2])) +
        geom_density(fill = "red", alpha = 0.5) +
        coord_flip() +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[2], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      
      final_plot_2d <- (density_x + plot_spacer()) /
        (scatter_plot + density_y) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d)
    }
  }
  
  # Modelovanie zdruzenej hustoty ako hustoty normalneho rozdelenia
  if (model_type == "parametric") {
    # Vypocet zakladnych parametrov (momentov)
    mean_x <- mean(data[[continuous_vars[1]]], na.rm = TRUE)
    sd_x <- sd(data[[continuous_vars[1]]], na.rm = TRUE)
    mean_y <- mean(data[[continuous_vars[2]]], na.rm = TRUE)
    sd_y <- sd(data[[continuous_vars[2]]], na.rm = TRUE)
    
    # Korelacia medzi premennymi
    cor_val <- cor(data[[continuous_vars[1]]], data[[continuous_vars[2]]], method = "pearson", use = "complete.obs")
    
    # Funkcia na vypocet bivariatnej normalnej hustoty (kvazi dnorm() ale v 2D)
    bivariate_normal_density <- function(x, y) {
      rho <- cor_val
      z_x <- (x - mean_x) / sd_x
      z_y <- (y - mean_y) / sd_y
      
      exponent <- -1 / (2 * (1 - rho^2)) * (z_x^2 + z_y^2 - 2 * rho * z_x * z_y)
      density <- (1 / (2 * pi * sd_x * sd_y * sqrt(1 - rho^2))) * exp(exponent)
      
      return(density)
    }
    
    # Siet pre vypocet hustoty
    x_vals <- seq(min(data[[continuous_vars[1]]], na.rm = TRUE), max(data[[continuous_vars[1]]], na.rm = TRUE), length.out = 100)
    y_vals <- seq(min(data[[continuous_vars[2]]], na.rm = TRUE), max(data[[continuous_vars[2]]], na.rm = TRUE), length.out = 100)
    
    grid <- expand.grid(x = x_vals, y = y_vals)
    
    # Vypocet hustoty na sieti
    grid$z <- mapply(bivariate_normal_density, grid$x, grid$y)
    z_matrix <- matrix(grid$z, nrow = 100, byrow = FALSE)
    
    
    if (plot_type == "3D") {
      # 3D Vizualizacia
      fig_3d <- plot_ly(
        x = ~x_vals, y = ~y_vals, z = ~z_matrix,
        type = "surface",
        colors = colorRamp(c("blue", "cyan", "yellow", "red"))
      ) %>% layout(
        title = paste("Združená hustota", continuous_vars[1], "a", continuous_vars[2], "(bivariátne normálne rozdelenie)"),
        scene = list(
          xaxis = list(title = continuous_vars[1]),
          yaxis = list(title = continuous_vars[2]),
          zaxis = list(title = "Hustota")
        )
      )
      
      return(fig_3d)
      
    } else if (plot_type == "2D") {
      contour_df <- grid
      
      # 2D Vizualizacia
      scatter_plot <- ggplot(data, aes_string(x = continuous_vars[1], y = continuous_vars[2])) +
        geom_point(size = 3, alpha = 0.7, aes_string(color = continuous_vars[2])) +
        geom_contour(data = contour_df, aes(x = x, y = y, z = z), color = "black") +
        labs(
          x = continuous_vars[1],
          y = continuous_vars[2],
          title = paste("Scatter plot s vrstevnicami (bivariátne normálne)")
        ) +
        scale_color_gradient(low = "blue", high = "red") +
        theme_minimal()
      
      density_x <- ggplot(data, aes_string(x = continuous_vars[1])) +
        stat_function(fun = dnorm, args = list(mean = mean_x, sd = sd_x), fill = "blue", geom = "area", alpha = 0.5) +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[1], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      density_y <- ggplot(data, aes_string(x = continuous_vars[2])) +
        stat_function(fun = dnorm, args = list(mean = mean_y, sd = sd_y), fill = "red", geom = "area", alpha = 0.5) +
        coord_flip() +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[2], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      
      final_plot_2d <- (density_x + plot_spacer()) /
        (scatter_plot + density_y) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d)
    }
  }
  
  # Modelovanie zdruzenej hustoty ako hustoty Studentovho t-rozdelenia
  if (model_type == "t") {
    # Vypocet zakladnych parametrov (momentov)
    mean_x <- mean(data[[continuous_vars[1]]], na.rm = TRUE)
    sd_x <- sd(data[[continuous_vars[1]]], na.rm = TRUE)
    mean_y <- mean(data[[continuous_vars[2]]], na.rm = TRUE)
    sd_y <- sd(data[[continuous_vars[2]]], na.rm = TRUE)
    
    # Korelacia medzi premennymi (Pearson)
    cor_val <- cor(data[[continuous_vars[1]]], data[[continuous_vars[2]]], method = "pearson", use = "complete.obs")
    
    # Kovariancna matica (smerodajne odchylky/rozptyly a korelacia)
    Sigma <- matrix(c(sd_x^2, cor_val * sd_x * sd_y,
                      cor_val * sd_x * sd_y, sd_y^2), nrow = 2)
    
    # Inverzna kovariancná matica
    Sigma_inv <- solve(Sigma)
    
    # Determinant kovariancnej matice
    det_Sigma <- det(Sigma)
    
    # Stupne volnosti
    df <- nrow(data) - 1
    
    # Bivariatna Studentova t-hustota
    bivariate_t_density <- function(x, y) {
      z <- c(x - mean_x, y - mean_y)
      quad_form <- t(z) %*% Sigma_inv %*% z
      
      coeff <- gamma((df + 2) / 2) / (gamma(df / 2) * (df * pi) * sqrt(det_Sigma))
      exponent <- (1 + quad_form / df) ^ (-(df + 2) / 2)
      
      return(as.numeric(coeff * exponent))
    }
    
    # Siet pre vypocet hustoty
    x_vals <- seq(min(data[[continuous_vars[1]]], na.rm = TRUE), max(data[[continuous_vars[1]]], na.rm = TRUE), length.out = 100)
    y_vals <- seq(min(data[[continuous_vars[2]]], na.rm = TRUE), max(data[[continuous_vars[2]]], na.rm = TRUE), length.out = 100)
    
    grid <- expand.grid(x = x_vals, y = y_vals)
    
    # Vypocet hustoty na sieti
    grid$z <- mapply(bivariate_t_density, grid$x, grid$y)
    z_matrix <- matrix(grid$z, nrow = 100, byrow = FALSE)
    
    
    if (plot_type == "3D") {
      # 3D Vizualizacia
      fig_3d <- plot_ly(
        x = ~x_vals, y = ~y_vals, z = ~z_matrix,
        type = "surface",
        colors = colorRamp(c("blue", "cyan", "yellow", "red"))
      ) %>% layout(
        title = paste("Združená hustota", continuous_vars[1], "a", continuous_vars[2], "(bivariátne t-rozdelenie)"),
        scene = list(
          xaxis = list(title = continuous_vars[1]),
          yaxis = list(title = continuous_vars[2]),
          zaxis = list(title = "Hustota")
        )
      )
      
      return(fig_3d)
      
    } else if (plot_type == "2D") {
      contour_df <- grid
      
      # 2D Vizualizacia
      scatter_plot <- ggplot(data, aes_string(x = continuous_vars[1], y = continuous_vars[2])) +
        geom_point(size = 3, alpha = 0.7, aes_string(color = continuous_vars[2])) +
        geom_contour(data = contour_df, aes(x = x, y = y, z = z), color = "black") +
        labs(
          x = continuous_vars[1],
          y = continuous_vars[2],
          title = paste("Scatter plot s vrstevnicami (bivariátne t-rozdelenie)")
        ) +
        scale_color_gradient(low = "blue", high = "red") +
        theme_minimal()
      
      density_x <- ggplot(data, aes_string(x = continuous_vars[1])) +
        stat_function(fun = function(x) dt((x - mean_x) / sd_x, df = df) / sd_x, fill = "blue", geom = "area", alpha = 0.5) +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[1], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      density_y <- ggplot(data, aes_string(x = continuous_vars[2])) +
        stat_function(fun = function(y) dt((y - mean_y) / sd_y, df = df) / sd_y, fill = "red", geom = "area", alpha = 0.5) +
        coord_flip() +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[2], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      
      final_plot_2d <- (density_x + plot_spacer()) /
        (scatter_plot + density_y) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d)
      
    }
  }
}

continuous_joint_distribution_copula <- function(data, continuous_vars, model_type, copula_type, marginal_densities, plot_type) {
  
  # Modelovanie hustoty pomocou jadroveho vyhladzovania a kopuly (rozklad na marginaly)
  if (model_type == "kernel") {
    kde_x <- density(data[[continuous_vars[1]]], n = 512)
    kde_y <- density(data[[continuous_vars[2]]], n = 512)
    
    # Funkcie pre hustoty a distribucne funkcie KDE
    kde_x_density <- approxfun(kde_x$x, kde_x$y, rule = 2)
    kde_x_cdf <- approxfun(kde_x$x, cumsum(kde_x$y) / sum(kde_x$y), rule = 2)
    
    kde_y_density <- approxfun(kde_y$x, kde_y$y, rule = 2)
    kde_y_cdf <- approxfun(kde_y$x, cumsum(kde_y$y) / sum(kde_y$y), rule = 2)
    
    # Transformacia dat na jednotkovy interval
    u1 <- kde_x_cdf(data[[continuous_vars[1]]])
    u2 <- kde_y_cdf(data[[continuous_vars[2]]])
    
    empirical_data <- pobs(cbind(u1, u2))  # zoradenie do [0, 1]
    
    if (copula_type == "empirical") {
      copula_model <- empCopula(empirical_data, smoothing = "beta")
    } else {
      stop("Zadaný 'copula_type' nie je podporovaný. Použi: 'empirical'.")
    }
    
    # Vytvorenie siete pre vypocet hustoty
    x_vals <- seq(min(data[[continuous_vars[1]]], na.rm = TRUE), max(data[[continuous_vars[1]]], na.rm = TRUE), length.out = 100)
    y_vals <- seq(min(data[[continuous_vars[2]]], na.rm = TRUE), max(data[[continuous_vars[2]]], na.rm = TRUE), length.out = 100)
    
    grid <- expand.grid(x = x_vals, y = y_vals)
    
    # Funkcia pre vypocet zdruzenej hustoty KDE + kopula
    copula_density_function <- function(x, y) {
      u1 <- kde_x_cdf(x)
      u2 <- kde_y_cdf(y)
      
      # Hustota kopuly v bode (u1, u2)
      copula_density <- dCopula(cbind(u1, u2), copula = copula_model)
      
      marginal_x <- kde_x_density(x)
      marginal_y <- kde_y_density(y)
      
      copula_density * marginal_x * marginal_y
    }
    
    # Vypocet hustoty na sieti
    grid$z <- mapply(copula_density_function, grid$x, grid$y)
    z_matrix <- matrix(grid$z, nrow = 100, byrow = FALSE)
    
    
    if (plot_type == "3D") {
      # 3D Vizualizacia
      fig_3d <- plot_ly(
        x = ~x_vals, y = ~y_vals, z = ~z_matrix,
        type = "surface",
        colors = colorRamp(c("blue", "cyan", "yellow", "red"))
      ) %>% layout(
        title = paste("Združená hustota", continuous_vars[1], "a", continuous_vars[2], paste0(" (KDE + ", copula_type, " kopula)")),
        scene = list(
          xaxis = list(title = continuous_vars[1]),
          yaxis = list(title = continuous_vars[2]),
          zaxis = list(title = "Hustota")
        )
      )
      
      return(fig_3d)
      
    } else if (plot_type == "2D") {
      # 2D Vizualizacia
      contour_df <- grid
      
      scatter_plot <- ggplot(data, aes_string(x = continuous_vars[1], y = continuous_vars[2])) +
        geom_point(size = 3, alpha = 0.7, aes_string(color = continuous_vars[2])) +
        geom_contour(data = contour_df, aes(x = x, y = y, z = z), color = "black", size = 0.6) +
        labs(
          x = continuous_vars[1],
          y = continuous_vars[2],
          title = paste("Scatter + vrstevnice (KDE +", copula_type, "kopula)")
        ) +
        scale_color_gradient(low = "blue", high = "red") +
        theme_minimal()
      
      density_x <- ggplot(data, aes_string(x = continuous_vars[1])) +
        geom_density(fill = "blue", alpha = 0.5) +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[1], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      density_y <- ggplot(data, aes_string(x = continuous_vars[2])) +
        geom_density(fill = "red", alpha = 0.5) +
        coord_flip() +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[2], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      
      final_plot_2d <- (density_x + plot_spacer()) /
        (scatter_plot + density_y) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d)
    }
  }
  
  # Modelovanie zdruzenej hustoty ako hustoty normalneho rozdelenia a pomocou kopuly (rozklad na marginaly)
  if (model_type == "parametric") {
    # Vypocet marginalnych parametrov
    mean_x <- mean(data[[continuous_vars[1]]], na.rm = TRUE)
    sd_x <- sd(data[[continuous_vars[1]]], na.rm = TRUE)
    mean_y <- mean(data[[continuous_vars[2]]], na.rm = TRUE)
    sd_y <- sd(data[[continuous_vars[2]]], na.rm = TRUE)
    
    # Vypocet hodnoty marginalnych rozdeleni pre vstup do kopuly
    u1 <- pnorm(data[[continuous_vars[1]]], mean = mean_x, sd = sd_x)
    u2 <- pnorm(data[[continuous_vars[2]]], mean = mean_y, sd = sd_y)
    
    if (copula_type == "Clayton") {
      copula_model <- claytonCopula(param = 2, dim = 2)
    } else if (copula_type == "Gumbel") {
      copula_model <- gumbelCopula(param = 2, dim = 2)
    } else if (copula_type == "Frank") {
      copula_model <- frankCopula(param = 5, dim = 2)
    } else {
      stop("Zadaný 'copula_type' nie je podporovaný. Použi: 'Clayton', 'Gumbel', 'Frank'.")
    }
    
    # Fitovanie kopuly na marginálne transformované dáta
    copula_fit <- fitCopula(copula_model, pobs(cbind(u1, u2)), method = "ml")
    copula_model_fitted <- copula_fit@copula
    
    # Vytvorenie gridu pre hustotu
    x_vals <- seq(min(data[[continuous_vars[1]]], na.rm = TRUE), max(data[[continuous_vars[1]]], na.rm = TRUE), length.out = 100)
    y_vals <- seq(min(data[[continuous_vars[2]]], na.rm = TRUE), max(data[[continuous_vars[2]]], na.rm = TRUE), length.out = 100)
    
    grid <- expand.grid(x = x_vals, y = y_vals)
    
    # Funkcia na vypocet marginalnej hustoty podla vyberu
    marginal_density_function <- function(value, mean, sd, index) {
      density_type <- marginal_densities[index]
      if (density_type == "dnorm") {
        return(dnorm(value, mean = mean, sd = sd))
      } else if (density_type == "log_dnorm") {
        return(exp(dnorm(value, mean = mean, sd = sd, log = TRUE)))
      } else {
        stop(paste("Neznáma hustota:", density_type))
      }
    }
    
    # Vypocet zdruzenej hustoty cez rozklad na marginaly + kopula
    copula_density_function <- function(x, y) {
      u1 <- pnorm(x, mean = mean_x, sd = sd_x)
      u2 <- pnorm(y, mean = mean_y, sd = sd_y)
      
      copula_part <- dCopula(cbind(u1, u2), copula = copula_model_fitted)
      
      marginal_x <- marginal_density_function(x, mean_x, sd_x, 1)
      marginal_y <- marginal_density_function(y, mean_y, sd_y, 2)
      
      copula_part * marginal_x * marginal_y
    }
    
    # Vypocet hustoty na sieti
    grid$z <- mapply(copula_density_function, grid$x, grid$y)
    z_matrix <- matrix(grid$z, nrow = 100, byrow = FALSE)
    
    
    if (plot_type == "3D") {
      # 3D Vizualizacia
      fig_3d <- plot_ly(
        x = ~x_vals, y = ~y_vals, z = ~z_matrix,
        type = "surface",
        colors = colorRamp(c("blue", "cyan", "yellow", "red"))
      ) %>% layout(
        title = paste0("Združená hustota ", continuous_vars[1], " a ", continuous_vars[2], " (dnorm + ", copula_type, " kopula)"),
        scene = list(
          xaxis = list(title = continuous_vars[1]),
          yaxis = list(title = continuous_vars[2]),
          zaxis = list(title = "Hustota")
        )
      )
      
      return(fig_3d)
      
    } else if (plot_type == "2D") {
      # 2D Vizualizacia
      scatter_plot <- ggplot(data, aes_string(x = continuous_vars[1], y = continuous_vars[2])) +
        geom_point(size = 3, alpha = 0.7, aes_string(color = continuous_vars[2])) +
        geom_contour(
          data = grid,
          aes(x = x, y = y, z = z),
          bins = 10,
          color = "black",
          size = 0.6
        ) +
        labs(
          x = continuous_vars[1],
          y = continuous_vars[2],
          title = paste("Scatter + vrstevnice (dnorm +", copula_type, "kopula)")
        ) +
        scale_color_gradient(low = "blue", high = "red") +
        theme_minimal()
      
      density_x <- ggplot(data, aes_string(x = continuous_vars[1])) +
        stat_function(fun = dnorm, args = list(mean = mean_x, sd = sd_x), fill = "blue", geom = "area", alpha = 0.5) +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[1], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      density_y <- ggplot(data, aes_string(x = continuous_vars[2])) +
        stat_function(fun = dnorm, args = list(mean = mean_y, sd = sd_y), fill = "red", geom = "area", alpha = 0.5) +
        coord_flip() +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[2], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      
      final_plot_2d <- (density_x + plot_spacer()) /
        (scatter_plot + density_y) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d)
      
    }
  }
  
  # Modelovanie zdruzenej hustoty ako hustoty Studentovho t-rozdelenia a pomocou kopuly (rozklad na marginaly)
  if (model_type == "t") {
    # Vypocet zakladnych parametrov marginalnych Studentovych t-rozdeleni
    mean_x <- mean(data[[continuous_vars[1]]], na.rm = TRUE)
    sd_x <- sd(data[[continuous_vars[1]]], na.rm = TRUE)
    mean_y <- mean(data[[continuous_vars[2]]], na.rm = TRUE)
    sd_y <- sd(data[[continuous_vars[2]]], na.rm = TRUE)
    
    # Stupne volnosti marginal
    df <- nrow(data) - 1
    if (df < 2) df <- 2
    
    # Hodnoty marginalnych distribucnych funkcii
    u1 <- pt((data[[continuous_vars[1]]] - mean_x) / sd_x, df = df)
    u2 <- pt((data[[continuous_vars[2]]] - mean_y) / sd_y, df = df)
    
    if (copula_type == "Clayton") {
      copula_model <- claytonCopula(param = 2, dim = 2)
    } else if (copula_type == "Gumbel") {
      copula_model <- gumbelCopula(param = 2, dim = 2)
    } else if (copula_type == "Joe") {
      copula_model <- joeCopula(param = 2, dim = 2)
    } else {
      stop("Zadaný 'copula_type' nie je podporovaný. Použi: 'Clayton', 'Gumbel', 'Joe'.")
    }
    
    # Fitovanie kopuly na marginalne transformovane data
    copula_fit <- fitCopula(copula_model, pobs(cbind(u1, u2)), method = "ml")
    copula_model_fitted <- copula_fit@copula
    
    # Vytvorenie siete pre vypocet hustoty
    x_vals <- seq(min(data[[continuous_vars[1]]], na.rm = TRUE), max(data[[continuous_vars[1]]], na.rm = TRUE), length.out = 100)
    y_vals <- seq(min(data[[continuous_vars[2]]], na.rm = TRUE), max(data[[continuous_vars[2]]], na.rm = TRUE), length.out = 100)
    
    grid <- expand.grid(x = x_vals, y = y_vals)
    
    # Funkcia pre vypocet zdruzenej hustoty
    copula_density_function <- function(x, y) {
      u1 <- pt((x - mean_x) / sd_x, df = df)
      u2 <- pt((y - mean_y) / sd_y, df = df)
      
      copula_part <- dCopula(cbind(u1, u2), copula = copula_model_fitted)
      
      marginal_x <- dt((x - mean_x) / sd_x, df = df) / sd_x
      marginal_y <- dt((y - mean_y) / sd_y, df = df) / sd_y
      
      copula_part * marginal_x * marginal_y
    }
    
    # Vypocet hustoty na sieti
    grid$z <- mapply(copula_density_function, grid$x, grid$y)
    z_matrix <- matrix(grid$z, nrow = 100, byrow = FALSE)
    
    
    if (plot_type == "3D") {
      # 3D Vizualizacia
      fig_3d <- plot_ly(
        x = ~x_vals, y = ~y_vals, z = ~z_matrix,
        type = "surface",
        colors = colorRamp(c("blue", "cyan", "yellow", "red"))
      ) %>% layout(
        title = paste("Združená hustota", continuous_vars[1], "a", continuous_vars[2], paste0("(t-rozdelenie + ", copula_type, " kopula)")),
        scene = list(
          xaxis = list(title = continuous_vars[1]),
          yaxis = list(title = continuous_vars[2]),
          zaxis = list(title = "Hustota")
        )
      )
      
      return(fig_3d)
      
    } else if (plot_type == "2D") {
      # 2D Vizualizacia
      scatter_plot <- ggplot(data, aes_string(x = continuous_vars[1], y = continuous_vars[2])) +
        geom_point(size = 3, alpha = 0.7, aes_string(color = continuous_vars[2])) +
        geom_contour(
          data = grid,
          aes(x = x, y = y, z = z),
          bins = 10,
          color = "black",
          size = 0.6
        ) +
        labs(
          x = continuous_vars[1],
          y = continuous_vars[2],
          title = paste("Scatter + vrstevnice (t-rozdelenie +", copula_type, "kopula)")
        ) +
        scale_color_gradient(low = "blue", high = "red") +
        theme_minimal()
      
      density_x <- ggplot(data, aes_string(x = continuous_vars[1])) +
        stat_function(fun = function(x) dt((x - mean_x) / sd_x, df = df) / sd_x, fill = "blue", geom = "area", alpha = 0.5) +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[1], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      
      density_y <- ggplot(data, aes_string(x = continuous_vars[2])) +
        stat_function(fun = function(y) dt((y - mean_y) / sd_y, df = df) / sd_y, fill = "red", geom = "area", alpha = 0.5) +
        coord_flip() +
        labs(x = NULL, y = paste("Hustota (", continuous_vars[2], ")", sep = "")) +
        theme_minimal() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      
      final_plot_2d <- (density_x + plot_spacer()) /
        (scatter_plot + density_y) +
        plot_layout(widths = c(4, 1), heights = c(1, 4))
      
      return(final_plot_2d)
    }
  }
}

multi_joint_distribution <- function(data, discrete_vars, continuous_vars) {
  
  # Vytvorenie tabulky s frekvenciami pre diskretne premenne
  tab <- as.data.frame(table(data[, discrete_vars]))
  
  # Pre kazdu spojitu premennu vypocitame priemerne hodnoty pre kombinacie diskretnych premennych
  for (cont_var in continuous_vars) {
    mean_values <- data %>%
      group_by(across(all_of(discrete_vars))) %>%
      summarise(Mean_Value = mean(.data[[cont_var]], na.rm = TRUE), .groups = "drop")
    
    # mean_values neobsahuje ziadne riadky => preskocime tuto premennu
    if (nrow(mean_values) == 0) {
      warning(paste("Žiadne dáta na výpočet priemeru pre premennú:", cont_var))
      next
    }
    
    # Overenie, ktore diskretne premenne existuju v tab a mean_values
    common_vars <- intersect(names(tab), discrete_vars)
    common_vars_mv <- intersect(names(mean_values), discrete_vars)
    
    # Existuju spolocne premenne => prevedenie na character
    if (length(common_vars) > 0) {
      tab <- tab %>%
        mutate(across(all_of(common_vars), as.character))
    }
    if (length(common_vars_mv) > 0) {
      mean_values <- mean_values %>%
        mutate(across(all_of(common_vars_mv), as.character))
    }
    
    # Spojenie priemernych hodnot so zakladnou tabulkou
    tab <- full_join(tab, mean_values, by = common_vars)
    
    # Premenovanie stlpca Mean_Value na nazov spojitej premennej
    colnames(tab)[which(colnames(tab) == "Mean_Value")] <- cont_var
  }
  
  # Náhrada NA vo Frequency nulami
  tab$Freq[is.na(tab$Freq)] <- 0
  
  # Vypocet pravdepodobnosti
  tab$Probability <- tab$Freq / sum(tab$Freq)
  
  # Odstranenie riadkov s nulovou frekvenciou
  tab <- tab[tab$Freq > 0, ]
  
  return(tab)
}

discrete_joint_distribution <- function(data, discrete_vars, plot_type) {
  
  tab <- as.data.frame(table(data[, discrete_vars]))
  tab$Probability <- tab$Freq / sum(tab$Freq)
  
  tab[[discrete_vars[1]]] <- as.numeric(as.character(tab[[discrete_vars[1]]]))
  tab[[discrete_vars[2]]] <- as.numeric(as.character(tab[[discrete_vars[2]]]))
  
  
  if (plot_type == "2D") {
    marginal_x <- tab %>%
      group_by(!!sym(discrete_vars[1])) %>%
      summarise(Prob_X = sum(Probability))
    
    marginal_y <- tab %>%
      group_by(!!sym(discrete_vars[2])) %>%
      summarise(Prob_Y = sum(Probability))
    
    # 2D Vizualizacia
    scatter_plot <- ggplot(tab, aes_string(x = discrete_vars[1], y = discrete_vars[2], fill =   "Probability")) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(x = discrete_vars[1], y = discrete_vars[2], fill = "Pravdepodobnosť") +
      theme_minimal()
    
    marginal_x_plot <- ggplot(marginal_x, aes_string(x = discrete_vars[1], y = "Prob_X")) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
      labs(x = NULL, y = "P(X)") +
      theme_minimal()
    
    marginal_y_plot <- ggplot(marginal_y, aes_string(x = discrete_vars[2], y = "Prob_Y")) +
      geom_bar(stat = "identity", fill = "red", alpha = 0.6) +
      coord_flip() +
      labs(x = NULL, y = "P(Y)") +
      theme_minimal()
    
    final_plot_2d <- (marginal_x_plot + plot_spacer()) /
      (scatter_plot + marginal_y_plot) +
      plot_layout(widths = c(4, 1), heights = c(1, 4))
    
    return(final_plot_2d)
    
  } else if (plot_type == "3D") {
    # 3D Vizualizacia
    fig_3d <- plot_ly()
    for (i in 1:nrow(tab)) {
      fig_3d <- fig_3d %>% add_trace(
        x = rep(tab[[discrete_vars[1]]][i], 2),
        y = rep(tab[[discrete_vars[2]]][i], 2),
        z = c(0, tab$Probability[i]),
        type = "scatter3d",
        mode = "lines",
        line = list(color = "blue"),
        showlegend = FALSE
      )
    }
    
    fig_3d <- fig_3d %>% add_trace(
      x = tab[[discrete_vars[1]]],
      y = tab[[discrete_vars[2]]],
      z = tab$Probability,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 5, color = "red"),
      name = "Pravdepodobnosti"
    )
    
    fig_3d <- fig_3d %>% layout(
      scene = list(
        xaxis = list(title = paste0(discrete_vars[1], " (X)")),
        yaxis = list(title = paste0(discrete_vars[2], " (Y)")),
        zaxis = list(title = "P(X, Y)")
      )
    )
    
    return(fig_3d)
  }
}

model_joint_distribution_density <- function(data, selected_variables, model_type = NULL, bw = NULL, use_copula = FALSE, copula_type = NULL, marginal_densities = c("dnorm", "dnorm"), plot_type = NULL) {
  # Identifikacia typov premennych
  variable_types <- identify_variables(data)
  discrete_vars <- intersect(variable_types$Diskretne, selected_variables)
  continuous_vars <- intersect(variable_types$Spojite, selected_variables)
  variables_count <- length(discrete_vars) + length(continuous_vars)
  
  if (length(selected_variables) == 2 && length(continuous_vars) == 0) {
    if (!is.null(model_type) || use_copula != FALSE || !is.null(copula_type) || !is.null(bw)) {
      stop("Argumenty model_type, 'use_copula', 'copula_type' a 'bw' sa nepouzivaju pri modelovani dvoch diskretnych premennych.")
    }
    if (is.null(plot_type)){
      stop("Nebol zadany typ grafu na vykreslenie.")
    }
    result <- discrete_joint_distribution(data, discrete_vars, plot_type)
    print(result)
  } else if (variables_count > 2) {
    if (!is.null(model_type) || use_copula != FALSE || !is.null(copula_type) || !is.null(bw)) {
      message("Argumenty model_type, 'use_copula', 'copula_type' a 'bw' sa nepouzivaju pri modelovani vacsieho poctu premennych ako 2.")
      
      result <- multi_joint_distribution(data, discrete_vars, continuous_vars)
      print(result)
    }
    
    result <- multi_joint_distribution(data, discrete_vars, continuous_vars)
    print(result)
  } else if (length(selected_variables) == 2 && length(discrete_vars) == 0) {
    if (use_copula == FALSE) {
      if (!is.null(copula_type)) {
        stop("Argument 'copula_type' mozno pouzit len ked 'use_copula' = 'TRUE'.")
      }
      else {
        if (is.null(plot_type)){
          stop("Nebol zadany typ grafu na vykreslenie.")
        }
        result <- continuous_joint_distribution(data, continuous_vars, model_type, plot_type)
        print(result)
      }
    }
    else {
      if (is.null(copula_type)) {
        stop("Ak je 'use_copula' = 'TRUE', musi byt specifikovany aj parameter 'copula_type'.")
      }
      else {
        if (model_type != "parametric" && !is.null(marginal_densities)){
          stop("Parameter marginal_densities sa zadava iba pri model_type = 'parametric' s kopulou")
        }
        if (is.null(plot_type)){
          stop("Nebol zadany typ grafu na vykreslenie.")
        }
        result <- continuous_joint_distribution_copula(data, continuous_vars, model_type, copula_type, marginal_densities, plot_type)
        print(result)
      }
    }
  } else if (length(selected_variables) == 2 && length(discrete_vars) == 1) {
    if (use_copula != FALSE || !is.null(copula_type)) {
      stop("Argumenty 'copula' a 'copula_type' sa pouzivaju iba pri modelovani dvoch spojitych premennych.")
    }
    if (is.null(plot_type)){
      stop("Nebol zadany typ grafu na vykreslenie.")
    }
    result <- mixture_joint_distribution(data, discrete_vars, continuous_vars, model_type, bw, plot_type)
    print(result)
  }
}

model_conditional_mean <- function(data, selected_variables, mean_method = "linear", poly_mean_degree = NULL, specific_x = NULL) {
  response_name <- selected_variables[1]
  predictor_name <- selected_variables[2]
  
  variable_types <- identify_variables(data)
  
  response <- data[[response_name]]
  predictor <- data[[predictor_name]]
  
  x_seq <- seq(min(predictor), max(predictor), length.out = 200)
  
  # Fitovanie modelu pre E[Y|X]
  if (mean_method == "loess") {
    fit <- loess(response ~ predictor, span = 0.75)
  } else if (mean_method == "gam") {
    fit <- gam(response ~ s(predictor))
  } else if (mean_method == "spline") {
    fit <- lm(response ~ bs(predictor, df = 5))
  } else if (mean_method == "linear") {
    fit <- lm(response ~ predictor)
  } else if (mean_method == "poly") {
    fit <- lm(response ~ poly(predictor, degree = poly_mean_degree, raw = TRUE))
  } else if (mean_method == "exp") {
    fit <- nls(response ~ a * exp(b * predictor), start = list(a = 1, b = 0.01))
  } else {
    stop("Neplatný mean_method!")
  }
  
  mean_pred <- predict(fit, newdata = data.frame(predictor = x_seq))
  
  # Konkrétny bod na x
  if (is.null(specific_x)) {
    specific_x <- median(predictor, na.rm = TRUE)
  }
  specific_mean <- predict(fit, newdata = data.frame(predictor = specific_x))
  
  # Vizualizácia
  p <- ggplot(data, aes_string(x = predictor_name, y = response_name)) +
    geom_point(alpha = 0.4, color = "darkgreen") +
    geom_line(aes(x = x_seq, y = mean_pred), color = "blue", size = 1.2) +
    geom_point(aes(x = specific_x, y = specific_mean), color = "blue", size = 3) +
    annotate("text", x = specific_x, y = specific_mean,
             label = paste0("E[Y|X=", round(specific_x, 2), "]=", round(specific_mean, 2)),
             hjust = -0.1, color = "blue") +
    labs(
      title = "Podmienená stredná hodnota",
      x = paste0(predictor_name, " (Prediktor)"),
      y = paste0(response_name, " (Odozva)")
    ) +
    theme_minimal()
  
  return(list(
    plot = p,
    conditional_mean = data.frame(X = x_seq, E_Y_given_X = mean_pred),
    specific_x = specific_x,
    specific_mean = specific_mean
  ))
}

model_conditional_quantiles <- function(data, selected_variables, quantile_method = "linear", poly_quant_degree = NULL, quantiles = 0.5, specific_x = NULL) {
  
  response_name <- selected_variables[1]
  predictor_name <- selected_variables[2]
  
  variable_types <- identify_variables(data)
  
  response <- data[[response_name]]
  predictor <- data[[predictor_name]]
  
  x_seq <- seq(min(predictor), max(predictor), length.out = 200)
  
  # Vypocet kvantilov
  quantile_preds <- list()
  
  for (q in quantiles) {
    if (quantile_method == "linear") {
      rq_fit <- rq(response ~ predictor, tau = q)
    } else if (quantile_method == "poly") {
      rq_fit <- rq(response ~ poly(predictor, degree = poly_quant_degree, raw = TRUE), tau = q)
    } else if (quantile_method == "spline") {
      rq_fit <- rq(response ~ ns(predictor, df = 4), tau = q)
    } else {
      stop("Neplatný quantile_method!")
    }
    
    quantile_preds[[as.character(q)]] <- predict(rq_fit, newdata = data.frame(predictor = x_seq))
  }
  
  if (is.null(specific_x)) {
    specific_x <- median(predictor, na.rm = TRUE)
  }
  
  # Konkretne kvantily pre specific_x
  specific_quantiles <- list()
  
  for (q in quantiles) {
    if (quantile_method == "linear") {
      rq_fit <- rq(response ~ predictor, tau = q)
    } else if (quantile_method == "poly") {
      rq_fit <- rq(response ~ poly(predictor, degree = poly_quant_degree, raw = TRUE), tau = q)
    } else if (quantile_method == "spline") {
      rq_fit <- rq(response ~ ns(predictor, df = 4), tau = q)
    }
    
    specific_quantiles[[as.character(q)]] <- predict(rq_fit, newdata = data.frame(predictor = specific_x))
  }
  
  plot_data <- data.frame(X = x_seq)
  for (q in quantiles) {
    plot_data[[paste0("Quantile_", q)]] <- quantile_preds[[as.character(q)]]
  }
  
  # Vizualizacia
  p <- ggplot(data, aes_string(x = predictor_name, y = response_name)) +
    geom_point(alpha = 0.4, color = "darkgreen") +
    labs(
      title = "Podmienené kvantilové funkcie",
      x = paste0(predictor_name, " (Prediktor)"),
      y = paste0(response_name, " (Odozva)")
    ) +
    theme_minimal()
  
  for (q in quantiles) {
    p <- p + geom_line(
      data = plot_data,
      aes_string(x = "X", y = paste0("Quantile_", q)),
      size = 1,
      linetype = "dashed",
      color = "red"
    )
    
    p <- p + geom_point(data = data.frame(specific_x = specific_x, y_val = specific_quantiles[[as.character(q)]]),
                        mapping = aes(x = specific_x, y = y_val),
                        color = "red", size = 3)
    
    q_label <- paste0("Q_", q, "(Y|X=", round(specific_x, 2), ")=", round(specific_quantiles[[as.character(q)]], 2))
    p <- p + annotate("text", x = specific_x, y = specific_quantiles[[as.character(q)]],
                      label = q_label, hjust = -0.1, color = "red")
  }
  
  return(list(
    plot = p,
    conditional_quantiles = dplyr::select(plot_data, X, dplyr::starts_with("Quantile_")),
    specific_x = specific_x,
    specific_quantiles = specific_quantiles
  ))
}

combine_conditional_models <- function(data, selected_variables, mean_method = "linear", quantile_method = "linear", poly_mean_degree = NULL, poly_quant_degree = NULL, quantiles = 0.5, specific_x = NULL) {
  if (length(selected_variables) != 2) {
    stop("Zadavaju sa dve premenne: odozva a prediktor.")
  }
  
  response_name <- selected_variables[1]
  predictor_name <- selected_variables[2]
  
  if (mean_method == "poly" && is.null(poly_mean_degree)) {
    stop("Pri mean_method = 'poly' je treba zadat aj poly_mean_degree.")
  }
  
  if (!is.null(poly_mean_degree) && mean_method != "poly") {
    stop("poly_mean_degree sa pouziva iba pri mean_method = 'poly'.")
  }
  
  if (quantile_method == "poly" && is.null(poly_quant_degree)) {
    stop("Pri quantile_method = 'poly' je treba zadat poly_quant_degree.")
  }
  
  if (!is.null(poly_quant_degree) && quantile_method != "poly") {
    stop("poly_quant_degree sa pouziva iba pri quantile_method = 'poly'.")
  }
  
  variable_types <- identify_variables(data)
  
  if (!(response_name %in% colnames(data))) {
    stop(paste("Premenna", response_name, "nie je v datach."))
  }
  
  if (!(predictor_name %in% colnames(data))) {
    stop(paste("Premenna", predictor_name, "nie je v datach."))
  }
  
  if (!(predictor_name %in% variable_types$Spojite)) {
    stop(paste("Prediktor", predictor_name, "musi byt spojita premenna."))
  }
  
  if (response_name %in% variable_types$Diskretne) {
    
    # Ak je odozva typu faktor alebo character => pretypovanie na cislo
    if (is.factor(data[[response_name]])) {
      message(paste("Premenna", response_name, "je faktor. Pretypovanie na cislo."))
      data[[response_name]] <- as.numeric(data[[response_name]])
    }
    
    if (is.character(data[[response_name]])) {
      message(paste("Premenna", response_name, "je character. Pretypovanie na cislo."))
      data[[response_name]] <- as.numeric(as.factor(data[[response_name]]))
    }
  }
  
  mean_result <- model_conditional_mean(
    data = data,
    selected_variables = selected_variables,
    mean_method = mean_method,
    poly_mean_degree = poly_mean_degree,
    specific_x = specific_x
  )
  
  quantile_result <- model_conditional_quantiles(
    data = data,
    selected_variables = selected_variables,
    quantile_method = quantile_method,
    poly_quant_degree = poly_quant_degree,
    quantiles = quantiles,
    specific_x = specific_x
  )
  
  p <- ggplot(data, aes_string(x = predictor_name, y = response_name)) +
    geom_point(alpha = 0.4, color = "darkgreen") +
    geom_line(data = mean_result$conditional_mean, aes(x = X, y = E_Y_given_X),
              color = "blue", size = 1.2, linetype = "solid") +
    geom_point(data = data.frame(specific_x = mean_result$specific_x, specific_mean = mean_result$specific_mean),
               mapping = aes(x = specific_x, y = specific_mean),
               color = "blue", size = 3) +
    annotate("text",
             x = mean_result$specific_x,
             y = mean_result$specific_mean,
             label = paste0("E[Y|X=", round(mean_result$specific_x, 2), "]=", round(mean_result$specific_mean, 2)),
             hjust = -0.1, color = "blue") +
    labs(
      title = "Podmienená stredná hodnota a vybrané kvantilové funkcie",
      x = paste0(predictor_name, " (Prediktor)"),
      y = paste0(response_name, " (Odozva)")
    ) +
    theme_minimal()
  
  for (q in quantiles) {
    quant_data <- quantile_result$conditional_quantiles
    col_name <- paste0("Quantile_", q)
    
    p <- p + geom_line(
      data = quant_data,
      aes_string(x = "X", y = col_name),
      size = 1,
      linetype = "dashed",
      color = "red"
    )
    
    # Bod na konkretnom specific_x
    p <- p + geom_point(data = data.frame(specific_x = quantile_result$specific_x,
                                          y_val = quantile_result$specific_quantiles[[as.character(q)]]),
                        mapping = aes(x = specific_x, y = y_val),
                        color = "red", size = 3)
    
    p <- p + annotate("text",
                      x = quantile_result$specific_x,
                      y = quantile_result$specific_quantiles[[as.character(q)]],
                      label = paste0("Q_", q, "(Y|X=", round(quantile_result$specific_x, 2), ")=", round(quantile_result$specific_quantiles[[as.character(q)]], 2)),
                      hjust = -0.1, color = "red")
  }
  
  return(list(
    combined_plot = p,
    mean_result = mean_result,
    quantile_result = quantile_result
  ))
}

plot_decision_boundary <- function(data, response_name, predictor_names, model, method) {
  
  if (!is.factor(data[[response_name]])) {
    data[[response_name]] <- as.factor(data[[response_name]])
  }
  
  # Grid pre vizualizaciu rozhodovacich hranic
  x_seq <- seq(min(data[[predictor_names[1]]]), max(data[[predictor_names[1]]]), length.out = 200)
  y_seq <- seq(min(data[[predictor_names[2]]]), max(data[[predictor_names[2]]]), length.out = 200)
  
  grid <- expand.grid(X1 = x_seq, X2 = y_seq)
  names(grid) <- predictor_names
  
  # Predikcia modelu na grid
  if (method == "logistic") {
    response <- data[[response_name]]
    
    if (length(levels(as.factor(response))) == 2) {
      # Binarna logisticka regresia
      probs <- predict(model, newdata = grid, type = "response")
      grid$pred <- ifelse(probs > 0.5,
                          levels(as.factor(response))[2],
                          levels(as.factor(response))[1])
    } else {
      # Multinomicka logisticka regresia
      grid$pred <- predict(model, newdata = grid, type = "class")
    }
    
  } else if (method == "lda") {
    grid$pred <- predict(model, newdata = grid)$class
  } else if (method == "qda") {
    grid$pred <- predict(model, newdata = grid)$class
  } else if (method == "knn") {
    grid$pred <- class::knn(
      train = model$train_data,
      test = grid,
      cl = model$train_response,
      k = model$k
    )
  }
  
  p <- ggplot() +
    # Rozhodovacie oblasti (predikcie modelu)
    geom_tile(data = grid, aes_string(x = predictor_names[1], y = predictor_names[2], fill = "pred"), alpha = 0.3) +
    
    # Skutocne data (odozvy)
    geom_point(data = data, aes_string(x = predictor_names[1], y = predictor_names[2], color = response_name, shape = response_name), size = 3) +
    
    labs(
      title = paste0("Predikčný model metódy: ", method),
      subtitle = paste("Prediktory:", paste(predictor_names, collapse = " a "), "| Odozva:", response_name),
      x = paste0(predictor_names[1], " (Prediktor)"),
      y = paste0(predictor_names[2], " (Prediktor)"),
      fill = "Predikcia modelu",
      color = "Skutočná odozva",
      shape = "Skutočná odozva"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  return(p)
}

plot_classification_1D_combined <- function(data, response_name, predictor_name, model, method) {
  
  p1 <- NULL
  
  if (!is.factor(data[[response_name]])) {
    data[[response_name]] <- as.factor(data[[response_name]])
  }
  
  data[[response_name]] <- factor(data[[response_name]], levels = sort(as.numeric(levels(data[[response_name]]))))
  classes <- levels(data[[response_name]])
  n_classes <- length(classes)
  
  # Paleta farieb a tvarov
  my_colors <- c("#F8766D", "#DAA520", "#00BA38", "#619CFF", "#C77CFF", "#FF61C3")[1:n_classes]
  my_shapes <- c(16, 17, 15, 3, 4, 8)[1:n_classes]
  
  # Urcenie typu prediktora
  predictor_type <- if (is.numeric(data[[predictor_name]]) && length(unique(data[[predictor_name]])) > 5) {
    "continuous"
  } else {
    "discrete"
  }
  
  # Vytvorenie gridu
  grid_x <- if (predictor_type == "continuous") {
    seq(min(data[[predictor_name]], na.rm = TRUE),
        max(data[[predictor_name]], na.rm = TRUE),
        length.out = 300)
  } else {
    sort(unique(data[[predictor_name]]))
  }
  
  grid <- setNames(data.frame(grid_x), predictor_name)
  
  probs_long <- NULL
  
  if (method == "logistic" && length(classes) > 2) {
    probs <- predict(model, newdata = grid, type = "probs") %>% as.data.frame()
    grid$pred_class <- apply(probs, 1, function(row) names(row)[which.max(row)])
    grid$pred_class <- factor(grid$pred_class, levels = classes)
    
    probs[[predictor_name]] <- grid[[predictor_name]]
    probs_long <- probs %>%
      tidyr::pivot_longer(cols = all_of(classes), names_to = "Hodnota_odozvy", values_to = "Pravdepodobnost")
    probs_long$Hodnota_odozvy <- factor(probs_long$Hodnota_odozvy, levels = classes)
    
  } else if (method == "logistic" && length(classes) == 2) {
    p <- predict(model, newdata = grid, type = "response")
    
    probs <- data.frame(grid_x)
    colnames(probs) <- predictor_name
    probs[[as.character(classes[1])]] <- 1 - p
    probs[[as.character(classes[2])]] <- p
    
    grid$pred_class <- ifelse(p > 0.5, as.character(classes[2]), as.character(classes[1])) %>%
      factor(levels = classes)
    probs_long <- probs %>%
      tidyr::pivot_longer(cols = all_of(classes), names_to = "Hodnota_odozvy", values_to = "Pravdepodobnost")
    probs_long$Hodnota_odozvy <- factor(probs_long$Hodnota_odozvy, levels = classes)
    
  } else if (method %in% c("lda", "qda")) {
    probs <- predict(model, newdata = grid)$posterior %>% as.data.frame()
    probs <- probs[, classes, drop = FALSE]
    grid$pred_class <- apply(probs, 1, function(row) names(row)[which.max(row)])
    grid$pred_class <- factor(grid$pred_class, levels = classes)
    probs[[predictor_name]] <- grid[[predictor_name]]
    probs_long <- probs %>%
      tidyr::pivot_longer(cols = all_of(classes), names_to = "Hodnota_odozvy", values_to = "Pravdepodobnost")
    probs_long$Hodnota_odozvy <- factor(probs_long$Hodnota_odozvy, levels = classes)
    
  } else if (method == "knn") {
    grid_matrix <- as.matrix(grid[, predictor_name, drop = FALSE])
    preds <- class::knn(
      train = model$train_data,
      test = grid_matrix,
      cl = model$train_response,
      k = model$k
    )
    grid$pred_class <- factor(preds, levels = classes)
  } else {
    stop("Neznama metoda!")
  }
  
  present_levels <- levels(droplevels(grid$pred_class))
  present_indices <- match(present_levels, classes)
  colors_used <- my_colors[present_indices]
  
  # Pravdepodobnosti
  if (!is.null(probs_long)) {
    probs_long <- probs_long %>% filter(Hodnota_odozvy %in% present_levels)
    
    if (predictor_type == "continuous") {
      p1 <- ggplot(probs_long, aes(x = .data[[predictor_name]], y = Pravdepodobnost, color = Hodnota_odozvy)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = setNames(colors_used, present_levels)) +
        labs(
          title = paste0("Pravdepodobnosti predikcie - ", method),
          x = paste0(predictor_name, " (Prediktor)"),
          y = "Pravdepodobnosť",
          color = "Hodnota odozvy"
        ) +
        theme_minimal()
    } else {
      p1 <- ggplot(probs_long, aes(x = factor(.data[[predictor_name]]), y = Pravdepodobnost, fill = Hodnota_odozvy)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = setNames(colors_used, present_levels)) +
        labs(
          title = paste0("Pravdepodobnosti predikcie - ", method),
          x = predictor_name,
          y = "Pravdepodobnosť",
          fill = "Hodnota odozvy"
        ) +
        theme_minimal()
    }
  }
  
  data[[response_name]] <- factor(data[[response_name]], levels = sort(as.numeric(levels(data[[response_name]]))))
  # Skutocne hodnoty + rozhodovacia hranica
  p2 <- ggplot() +
    scale_y_discrete(limits = classes) +
    geom_tile(
      data = grid,
      aes(x = .data[[predictor_name]], y = pred_class, fill = pred_class),
      height = 0.5, alpha = 0.3
    ) +
    geom_jitter(
      data = data,
      aes(x = .data[[predictor_name]], y = .data[[response_name]],
          color = .data[[response_name]],
          shape = .data[[response_name]]),
      width = 0.1, height = 0.1, size = 2
    ) +
    scale_fill_manual(values = setNames(colors_used, present_levels)) +
    scale_color_manual(values = my_colors) +
    scale_shape_manual(values = my_shapes) +
    labs(
      title = "Hodnoty odozvy + rozhodovacia hranica",
      x = paste0(predictor_name, " (Prediktor)"),
      y = paste0(response_name, " (Odozva)"),
      fill = "Predikcia",
      color = "Skutočná hodnota",
      shape = "Skutočná hodnota"
    ) +
    theme_minimal()
  
  if (!is.null(p1)) {
    return(p1 / p2)
  } else {
    return(p2)
  }
}

classification_model <- function(data, response_name, predictor_names, method = "logistic", k = NULL) {
  
  if (length(predictor_names) > 2) {
    stop("Pocet prediktorov nesmie byt vacsi ako 2.")
  }
  
  if (!(response_name %in% colnames(data))) {
    stop(paste("Premenna", response_name, "nie je v datach!"))
  }
  
  predictors_exist <- predictor_names %in% colnames(data)
  if (!all(predictors_exist)) {
    stop(paste("Tieto prediktory chybaju v datach:", paste(predictor_names[!predictors_exist], collapse = ", ")))
  }
  
  response <- data[[response_name]]
  
  if (!is.factor(response)) {
    unique_values <- length(unique(response))
    if (is.numeric(response) && unique_values <= 10) {
      message(paste("Odozva", response_name, "ma", unique_values, "unikatnych hodnot. Konverzia na faktor..."))
      response <- as.factor(response)
      data[[response_name]] <- response
    } else {
      stop(paste("Odozva", response_name, "nie je diskretna! Musi byt faktor alebo mat konecny pocet kategorii."))
    }
  }
  
  if (is.numeric(response) && length(unique(response)) == 2) {
    message("Konvertovanie ciselnej odozvy na faktor.")
    response <- as.factor(response)
    data[[response_name]] <- response
  }
  
  # Osetrenie kategorii s malym poctom pozorovani pre QDA
  if (method == "qda") {
    class_sizes <- table(response)
    too_small_classes <- names(class_sizes[class_sizes < 4])
    
    if (length(too_small_classes) > 0) {
      warning(paste("Tieto triedy maju menej ako 4 pozorovania a budu odstranene:", paste(too_small_classes, collapse = ", ")))
      
      data <- data[!(response %in% too_small_classes), ]
      response <- data[[response_name]]
    }
  }
  
  # Zostavenie formuly pre modelovanie
  formula_str <- paste(response_name, "~", paste(predictor_names, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Trenovanie modelu
  if (method == "logistic") { # logisticka regresia
    
    if (length(levels(response)) == 2) {
      # Klasicka binarna logisticka regresia
      model <- glm(formula, data = data, family = binomial)
      probs <- predict(model, newdata = data, type = "response")
      preds <- ifelse(probs > 0.5, levels(response)[2], levels(response)[1])
    } else {
      # Multinomicka logisticka regresia pre viac ako dve triedy
      model <- nnet::multinom(formula, data = data, trace = FALSE)
      preds <- predict(model, newdata = data)
    }
    
  } else if (method == "lda") { # linearna diskriminacna analyza
    
    model <- MASS::lda(formula, data = data)
    
  } else if (method == "qda") { # kvadraticka diskriminacna analyza
    
    model <- MASS::qda(formula, data = data)
    
  } else if (method == "knn") { # metoda k-najblizsich susedov
    train_data <- as.matrix(data[, predictor_names, drop = FALSE])
    train_response <- data[[response_name]]
    
    if (is.null(k)) {
      message("Optimalizujem parameter 'k' pre k-NN...")
      
      accuracy_scores <- numeric(20)
      
      for (i in 1:20) {
        preds_i <- class::knn(
          train = train_data,
          test = train_data,
          cl = train_response,
          k = i
        )
        acc_i <- mean(preds_i == train_response)
        accuracy_scores[i] <- acc_i
      }
      
      # Najlepsie k (ak je viac rovnakych, berieme najmensie)
      k <- which.max(accuracy_scores)
      message(paste("Optimalne k je:", k, "s presnostou:", round(accuracy_scores[k], 4)))
    }
    
    model <- list(
      train_data = train_data,
      train_response = train_response,
      k = k
    )
    
  }
  else {
    stop("Neplatna metoda. Vyber: 'logistic', 'lda', 'qda' alebo 'knn'.")
  }
  
  # Predikcie
  if (method %in% c("logistic", "lda", "qda")) {
    
    if (method == "logistic") {
      
      if (length(levels(response)) == 2) {
        # Klasicka binarna logisticka regresia
        probs <- predict(model, newdata = data, type = "response")
        preds <- ifelse(probs > 0.5, levels(response)[2], levels(response)[1])
        
      } else {
        # Multinomicka logisticka regresia
        preds <- predict(model, newdata = data, type = "class")
      }
      
    } else if (method == "lda") {
      preds <- predict(model, newdata = data)$class
      
    } else if (method == "qda") {
      preds <- predict(model, newdata = data)$class
    }
    
  } else if (method == "knn") {
    
    preds <- class::knn(
      train = model$train_data,
      test = model$train_data,
      cl = model$train_response,
      k = model$k
    )
  }
  
  # Vypocet presnosti
  accuracy <- mean(preds == response)
  confusion_mat <- table(Predikovane = preds, Skutocne = response)
  
  result <- list(
    model = model,
    predictions = preds,
    accuracy = accuracy,
    confusion_matrix = confusion_mat
  )
  
  if (length(predictor_names) == 1) {
    result$decision_plot <- plot_classification_1D_combined(
      data, response_name, predictor_names[1], model, method
    )
    
  } else if (length(predictor_names) == 2) {
    # Ak su 2 prediktory => graf rozhodovacich hranic
    
    result$decision_plot <- plot_decision_boundary(
      data, response_name, predictor_names, model, method
    )
  }
  
  return(result)
}

plot_conditional_continuous_densities <- function(df, n_breaks, density_scaling, mean_curve, quantiles, mean_poly_degree, quantile_poly_degree, normal_density, empirical_density, bw_scale) {
  
  response_name <- attr(df, "response_var")
  predictor_name <- attr(df, "predictor_var")
  
  # Rozdelenie na sekcie pre podmienene hustoty
  breaks <- seq(min(df$predictor, na.rm = TRUE), max(df$predictor, na.rm = TRUE), length.out = n_breaks + 1)
  df$section <- cut(df$predictor, breaks = breaks, include.lowest = TRUE)
  
  # Rezidua zo zakladneho modelu
  lm_fit <- lm(response ~ predictor, data = df)
  df$residuals <- residuals(lm_fit)
  
  # Hustoty pre kazdu sekciu
  density_data <- do.call(rbind, lapply(split(df, df$section), function(sub_df) {
    
    if (nrow(sub_df) < 3) return(NULL)
    
    output <- list()
    
    # Priemer odozvy v sekcii a posun pre vykreslenie hustoty
    y_mean <- mean(sub_df$response, na.rm = TRUE)
    x_max <- max(sub_df$predictor, na.rm = TRUE)
    
    
    
    # Empiricka hustota
    if (empirical_density) {
      if (is.null(bw_scale)){
        emp_density <- density(sub_df$residuals, n = 50, bw = "nrd0")
      }
      else{
        local_range <- max(sub_df$response, na.rm = TRUE) - min(sub_df$response, na.rm = TRUE)
        bw_effective <- bw_scale * local_range
        emp_density <- density(sub_df$residuals, n = 50, bw = bw_effective)
      }
      
      df_emp <- data.frame(
        x = x_max - emp_density$y * density_scaling,
        y = emp_density$x + y_mean,
        section = unique(sub_df$section),
        type = "empirical"
      )
      output[[length(output) + 1]] <- df_emp
    }
    
    # Hustota normalneho rozdelenia
    if (normal_density) {
      xs <- seq(min(sub_df$residuals), max(sub_df$residuals), length.out = 50)
      norm_density <- dnorm(xs, mean = 0, sd = sd(sub_df$residuals, na.rm = TRUE))
      
      df_theo <- data.frame(
        x = x_max - norm_density * density_scaling,
        y = xs + y_mean,
        section = unique(sub_df$section),
        type = "normal"
      )
      output[[length(output) + 1]] <- df_theo
    }
    
    if (length(output) == 0) {
      return(NULL)
    } else {
      return(do.call(rbind, output))
    }
  }))
  
  # Zakladny graf
  p <- ggplot(df, aes(x = predictor, y = response)) +
    geom_point(alpha = 0.6, color = "darkorange") +
    theme_bw() +
    labs(
      title = paste("Podmienené hustoty pre", response_name, "podľa", predictor_name),
      x = paste(predictor_name, "(Prediktor)"),
      y = paste(response_name, "(Spojitá odozva)")
    ) +
    geom_vline(xintercept = breaks, linetype = "dashed", color = "grey50")
  
  # Podmienene hustoty
  if (!is.null(density_data) && nrow(density_data) > 0) {
    p <- p + geom_path(
      data = density_data,
      aes(x = x, y = y, group = interaction(section, type), color = type),
      size = 1
    )
  } else {
    warning("Nebolo mozne vypocitat hustoty pre ziadnu sekciu!")
  }
  
  # Stredna hodnota regresie
  if (mean_curve) {
    mean_formula <- as.formula(paste("response ~ poly(predictor, ", mean_poly_degree, ", raw = TRUE)"))
    
    mean_model <- lm(mean_formula, data = df)
    mean_pred <- predict(mean_model, newdata = df)
    
    df$mean_pred <- mean_pred
    
    p <- p + geom_line(data = df, aes(x = predictor, y = mean_pred), color = "blue", size = 1.2)
  }
  
  # Kvantilove krivky
  if (!is.null(quantiles)) {
    for (q in quantiles) {
      quantile_formula <- as.formula(paste("response ~ poly(predictor, ", quantile_poly_degree, ", raw = TRUE)"))
      
      rq_fit <- quantreg::rq(quantile_formula, tau = q, data = df)
      
      quantile_pred <- predict(rq_fit, newdata = df)
      
      df$quantile_pred <- quantile_pred
      
      p <- p + geom_line(
        data = df,
        aes(x = predictor, y = quantile_pred),
        color = "purple",
        linetype = "dashed"
      )
    }
    
    p <- p + guides(color = guide_legend(title = "Hustota (typ)"))
  }
  
  return(p)
}

plot_conditional_discrete_densities <- function(df, n_breaks, density_scaling, ordinal) {
  
  response_name <- attr(df, "response_var")
  predictor_name <- attr(df, "predictor_var")
  
  # Rozdelenie prediktora na sekcie
  breaks <- seq(min(df$predictor, na.rm = TRUE),
                max(df$predictor, na.rm = TRUE),
                length.out = n_breaks + 1)
  
  df$section <- cut(df$predictor, breaks = breaks, include.lowest = TRUE)
  
  # Vypocet pravdepodobnostnych funkcii v jednotlivych sekciach
  density_data <- do.call(rbind, lapply(split(df, df$section), function(sub_df) {
    
    if (nrow(sub_df) < 3) return(NULL)
    
    # Pravdepodobnosti kategorii odozvy v ramci tejto sekcie
    prob_table <- prop.table(table(sub_df$response))
    
    # Y su hodnoty odozvy, X su dlzky ciar podla pravdepodobnosti
    y_vals <- as.numeric(names(prob_table))
    
    x_start <- max(sub_df$predictor, na.rm = TRUE)
    x_end <- x_start + (as.numeric(prob_table) * density_scaling)
    
    # Vysledny dataframe pre geom_segment + body
    df_segment <- data.frame(
      y = y_vals,
      x_start = x_start,
      x_end = x_end,
      prob = as.numeric(prob_table),
      section = unique(sub_df$section)
    )
    
    return(df_segment)
  }))
  
  if (is.null(density_data)) {
    warning("Nebolo mozne vypocitat ziadne pravdepodobnosti!")
  }
  
  print(density_data)
  
  # Vykreslenie grafu
  p <- ggplot(df, aes(x = predictor, y = as.numeric(response))) +
    geom_jitter(width = 0.1, height = 0.1, alpha = 0.6, color = "darkorange") +
    theme_bw() +
    labs(
      title = paste("Podmienené pravdepodobnostné funkcie pre", response_name, "podľa", predictor_name),
      x = paste(predictor_name, "(Prediktor)"),
      y = paste(response_name, "(Diskrétna odozva)")
    ) +
    geom_vline(xintercept = breaks, linetype = "dashed", color = "grey50")
  
  # Podmienene pravdepodobnostne funkcie
  if (!is.null(density_data) && nrow(density_data) > 0) {
    
    # Segmenty (vodorovne ciary)
    p <- p + geom_segment(
      data = density_data,
      aes(x = x_start, xend = x_end, y = y, yend = y),
      color = "blue",
      size = 1
    )
    
    # Body (konce pravdepodobnostnych funkcii)
    p <- p + geom_point(
      data = density_data,
      aes(x = x_end, y = y),
      shape = 15,
      color = "red",
      size = 1.5
    )
    
    # Ak je premenna ordinalna, spojime body ciarou
    if (ordinal) {
      sections <- unique(density_data$section)
      
      for (s in sections) {
        
        sub_density <- density_data %>%
          dplyr::filter(section == s) %>%
          dplyr::arrange(y)
        
        
        if (nrow(sub_density) >= 2) {
          p <- p + geom_path(
            data = sub_density,
            aes(x = x_end, y = y),
            color = "darkorchid",
            linewidth = 1
          )
        }
      }
    }
  }
  
  return(p)
}

plot_conditional_densities <- function(data, selected_variables, n_breaks = 5, density_scaling = 2000, ordinal = FALSE, mean_curve = TRUE, quantiles = NULL, mean_poly_degree = 1, quantile_poly_degree = 1, normal_density = TRUE, empirical_density = TRUE, bw_scale = NULL) {
  
  response_var <- selected_variables[1]
  predictor_var <- selected_variables[2]
  
  if (!(response_var %in% names(data)) || !(predictor_var %in% names(data))) {
    stop("Premenne nie su v datach!")
  }
  
  df <- data[, c(predictor_var, response_var)]
  var_types <- identify_variables(df)
  colnames(df) <- c("predictor", "response")
  
  # Atributy pre prediktor a odozvu
  attr(df, "response_var") <- response_var
  attr(df, "predictor_var") <- predictor_var
  
  if (response_var %in% var_types$Diskretne) {
    
    message(paste("Odozva", response_var, "bola identifikovana ako diskretna."))
    df$response <- as.factor(df$response)
    plot_conditional_discrete_densities(df, n_breaks, density_scaling, ordinal)
    
  } else if (response_var %in% var_types$Spojite) {
    
    message(paste("Odozva", response_var, "bola identifikovana ako spojita."))
    plot_conditional_continuous_densities(df, n_breaks, density_scaling, mean_curve, quantiles, mean_poly_degree, quantile_poly_degree, normal_density, empirical_density, bw_scale)
    
  } else {
    
    stop("Nebolo mozne identifikovat typ odozvy.")
  }
}

