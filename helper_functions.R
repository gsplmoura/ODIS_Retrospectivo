summarize_numerical <- function(data, group_col = NULL, use_labels = TRUE) {
    
    # Extract variable labels
    variable_labels <- sapply(names(data), function(var) {
        label <- attr(data[[var]], "label")
        if (is.null(label) || !use_labels) var else label
    }, simplify = TRUE)
    
    # Select numeric columns
    numeric_cols <- data %>% 
        dplyr::select(where(is.numeric)) %>% 
        names()
    
    if (!is.null(group_col)) {
        # Grouped summary (long format, consistent with summarize_categorical)
        summary <- data %>%
            tidyr::pivot_longer(cols = all_of(numeric_cols), names_to = "Variable", values_to = "Value") %>%
            group_by(across(all_of(group_col)), Variable) %>%
            summarise(
                Mean = round(mean(Value, na.rm = TRUE), 1),
                SD = round(sd(Value, na.rm = TRUE), 1),
                N = sum(!is.na(Value)),
                CI_lower = round(Mean - 1.96 * SD / sqrt(N), 1),
                CI_upper = round(Mean + 1.96 * SD / sqrt(N), 1),
                .groups = "drop"
            ) %>%
            mutate(
                Value = paste0(Mean, " (", CI_lower, "–", CI_upper, ")"),
                Variable = variable_labels[Variable]
            ) %>%
            dplyr::select(Variable, `Mean (95% CI)` = Value, N, all_of(group_col))
        
    } else {
        # Ungrouped summary
        summary <- data %>%
            tidyr::pivot_longer(cols = all_of(numeric_cols), names_to = "Variable", values_to = "Value") %>%
            group_by(Variable) %>%
            summarise(
                Mean = round(mean(Value, na.rm = TRUE), 1),
                SD = round(sd(Value, na.rm = TRUE), 1),
                N = sum(!is.na(Value)),
                CI_lower = round(Mean - 1.96 * SD / sqrt(N), 1),
                CI_upper = round(Mean + 1.96 * SD / sqrt(N), 1),
                .groups = "drop"
            ) %>%
            mutate(
                Value = paste0(Mean, " (", CI_lower, "–", CI_upper, ")"),
                Variable = variable_labels[Variable]
            ) %>%
            dplyr::select(Variable, `Mean (95% CI)` = Value, N)
    }
    
    return(summary)
}


summarize_categorical <- function(data, group_col = NULL, use_labels = TRUE) {
    # Extract variable labels, falling back to variable names if labels are missing
    variable_labels <- sapply(names(data), function(var) {
        label <- attr(data[[var]], "label")
        if (is.null(label) || !use_labels) var else label
    }, simplify = TRUE)
    
    # Identify categorical columns
    categorical_cols <- data %>% select(where(~ is.character(.x) || is.factor(.x))) %>% names()
    
    if (!is.null(group_col)) {
        # For grouped data
        summary <- lapply(categorical_cols, function(col) {
            data %>%
                group_by(across(all_of(group_col)), .drop = FALSE) %>%
                count(!!sym(col), .drop = FALSE, name = "Freq") %>%
                mutate(
                    Percent = round(100 * Freq / sum(Freq), 1),
                    Variable = variable_labels[col] # Use labels or names
                ) %>%
                rename(Level = !!sym(col)) %>%
                ungroup()
        }) %>%
            bind_rows()
    } else {
        # For ungrouped data
        summary <- lapply(categorical_cols, function(col) {
            data %>%
                count(!!sym(col), .drop = FALSE, name = "Freq") %>%
                mutate(
                    Percent = round(100 * Freq / sum(Freq), 1),
                    Variable = variable_labels[col] # Use labels or names
                ) %>%
                rename(Level = !!sym(col))
        }) %>%
            bind_rows()
    }
    
    # Arrange columns for consistency
    summary <- summary %>%
        select(Variable, Level, Freq, Percent, everything())
    
    return(summary)
}


compare_groups <- function(data, group_col = "allocation_group", use_labels = TRUE, return_df = FALSE) {
    
    # Load required packages
    required_packages <- c("dplyr", "gt")
    not_installed <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
    if (length(not_installed) > 0) {
        stop(paste("Missing required packages:", paste(not_installed, collapse = ", ")))
    }
    lapply(required_packages, require, character.only = TRUE)
    
    # Prepare labels
    variable_labels <- sapply(names(data), function(var) {
        label <- attr(data[[var]], "label")
        if (is.null(label) || !use_labels) var else label
    }, simplify = TRUE)
    
    # Initialize results dataframe
    results <- data.frame(
        Variable = character(),
        Test = character(),
        Statistic = numeric(),
        P_value = numeric(),
        stringsAsFactors = FALSE
    )
    
    # Loop through variables
    for (var in setdiff(names(data), group_col)) {
        
        # Prepare the data with non-missing group and variable
        group_data <- data %>%
            dplyr::select(all_of(c(var, group_col))) %>%
            dplyr::filter(!is.na(.data[[group_col]]))
        
        group_levels <- unique(group_data[[group_col]])
        
        if (length(group_levels) != 2) {
            warning(paste("Skipping", var, ": requires exactly 2 groups."))
            next
        }
        
        # Numeric variables → t-test
        if (is.numeric(group_data[[var]])) {
            
            var_data <- group_data %>% dplyr::filter(!is.na(.data[[var]]))
            
            var_in_groups <- split(var_data[[var]], var_data[[group_col]])
            
            # Check if both groups have variance
            if (any(sapply(var_in_groups, function(x) length(unique(x))) < 2)) {
                warning(paste("Skipping", var, ": one or both groups are constant or missing."))
                next
            }
            
            formula <- reformulate(group_col, response = var)
            
            test_result <- t.test(formula, data = var_data)
            
            results <- rbind(results, data.frame(
                Variable = variable_labels[var],
                Test = "t-test",
                Statistic = round(test_result$statistic, 2),
                P_value = round(test_result$p.value, 4)
            ))
            
        } 
        
        # Categorical variables → chi-squared or Fisher
        else if (is.factor(group_data[[var]]) || is.character(group_data[[var]])) {
            
            var_data <- group_data %>% dplyr::filter(!is.na(.data[[var]]))
            
            if (length(unique(var_data[[var]])) < 2) {
                warning(paste("Skipping", var, ": only one level present."))
                next
            }
            
            contingency_table <- table(var_data[[var]], var_data[[group_col]])
            
            if (any(suppressWarnings(chisq.test(contingency_table)$expected) < 5)) {
                test_result <- fisher.test(contingency_table)
                results <- rbind(results, data.frame(
                    Variable = variable_labels[var],
                    Test = "Fisher's exact test",
                    Statistic = NA,
                    P_value = round(test_result$p.value, 4)
                ))
            } else {
                test_result <- tryCatch(
                    chisq.test(contingency_table),
                    error = function(e) list(statistic = NA, p.value = NA)
                )
                results <- rbind(results, data.frame(
                    Variable = variable_labels[var],
                    Test = "Chi-squared test",
                    Statistic = round(test_result$statistic, 2),
                    P_value = round(test_result$p.value, 4)
                ))
            }
        }
    }
    
    # Return as dataframe if requested
    if (return_df) {
        return(results)
    }
    
    # Return as gt table
    gt_table <- results %>%
        gt() %>%
        tab_header(
            title = "Hypothesis Test Results",
            subtitle = paste("Comparison of", group_col)
        ) %>%
        cols_label(
            Variable = "Variable",
            Test = "Test Type",
            Statistic = "Test Statistic",
            P_value = "P-value"
        )
    
    return(gt_table)
}



## ✅ Full Function: plot_histograms_by_group()

plot_histograms_by_group <- function(data, group_col = NULL) {
    
    require(ggplot2)
    require(dplyr)
    require(tidyr)
    
    data_long <- data %>%
        {
            if (!is.null(group_col)) {
                mutate(., all_participants = "Total") %>%
                    rename(group = !!sym(group_col)) %>%
                    mutate(group = as.character(group)) %>%
                    bind_rows(mutate(., group = "Total"))
            } else {
                mutate(., group = "Total")
            }
        } %>%
        pivot_longer(
            cols = where(is.numeric),
            names_to = "Variable",
            values_to = "Value"
        )
    
    p <- ggplot(data_long, aes(x = Value)) +
        geom_histogram(fill = "steelblue", color = "black", bins = 15) +
        facet_grid(Variable ~ group, scales = "free") +  # <- FIXED HERE
        labs(
            title = "Histograms of Numerical Variables by Group",
            x = "Value",
            y = "Count"
        ) +
        theme_minimal(base_size = 10) +
        theme(
            strip.text = element_text(size = 7, face = "bold"),
            plot.title = element_text(hjust = 0.5, face = "bold")
        )
    
    return(p)
}

## ✅ Clean, Final Function Using facet_wrap(): 

plot_histograms_by_group <- function(data, group_col = NULL) {
    
    ## Load required libraries
    require(ggplot2)
    require(dplyr)
    require(tidyr)
    
    ## Prepare data: pivot to long format
    data_long <- data %>%
        {
            if (!is.null(group_col)) {
                mutate(., group = as.character(!!sym(group_col))) %>%
                    bind_rows(mutate(., group = "Total"))
            } else {
                mutate(., group = "Total")
            }
        } %>%
        pivot_longer(
            cols = where(is.numeric),
            names_to = "Variable",
            values_to = "Value"
        )
    
    ## Build the plot
    p <- ggplot(data_long, aes(x = Value, fill = group, color = group)) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
        facet_wrap(~ Variable, scales = "free", ncol = 3) +
        labs(
            title = "Histograms of Numerical Variables by Group",
            x = "Value",
            y = "Count",
            fill = "Group",
            color = "Group"
        ) +
        theme_minimal(base_size = 11) +
        theme(
            strip.text = element_text(size = 9, face = "bold"),
            plot.title = element_text(hjust = 0.5, face = "bold")
        ) +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    
    return(p)
}


sensitivity_check_lmer <- function(model, id_var = "record_id", top_n = 5) {
    require(influence.ME)
    require(dplyr)
    require(lme4)
    require(broom.mixed)
    
    # Compute influence measures
    infl <- influence(model, group = id_var)
    cooks <- cooks.distance(infl)
    
    # Extract IDs safely
    id_list <- rownames(as.data.frame(cooks))
    
    if (length(id_list) != length(cooks)) {
        stop("Mismatch between ID list and Cook's distances. Check grouping variable.")
    }
    
    # Build dataframe
    cooks_df <- tibble::tibble(
        record_id = id_list,
        cooks_distance = as.numeric(cooks)
    )
    
    # Rule-based threshold (4/n rule)
    influential_ids_rule <- cooks_df %>%
        filter(cooks_distance > (4 / nrow(cooks_df))) %>%
        pull(record_id)
    
    # Top N most influential
    top_ids <- cooks_df %>%
        arrange(desc(cooks_distance)) %>%
        slice_head(n = top_n) %>%
        pull(record_id)
    
    # Combine unique IDs
    influential_ids <- unique(c(influential_ids_rule, top_ids))
    
    # Refit model excluding influential IDs
    model_sens <- update(
        model,
        subset = !(get(id_var) %in% influential_ids)
    )
    
    # Compare fixed effects
    comparison <- bind_rows(
        broom.mixed::tidy(model) %>% mutate(Model = "Original"),
        broom.mixed::tidy(model_sens) %>% mutate(Model = "Sensitivity")
    ) %>%
        select(Model, term, estimate, std.error, statistic, p.value) %>%
        arrange(term, Model)
    
    # Output
    list(
        cooks_table = cooks_df,
        influential_ids = influential_ids,
        model_original = model,
        model_sensitivity = model_sens,
        comparison_table = comparison
    )
}

summarize_var <- function(data, var) {
    
    var <- rlang::ensym(var)
    
    data %>%
        group_by(allocation_group, visit) %>%
        summarise(
            N = sum(!is.na(!!var)),
            Mean = mean(!!var, na.rm = TRUE),
            SD = sd(!!var, na.rm = TRUE),
            Min = min(!!var, na.rm = TRUE),
            Q1 = quantile(!!var, 0.25, na.rm = TRUE),
            Median = median(!!var, na.rm = TRUE),
            Q3 = quantile(!!var, 0.75, na.rm = TRUE),
            Max = max(!!var, na.rm = TRUE),
            .groups = "drop"
        )
}


# Função auxiliar para IC95% da média (t de Student)

ic95 <- function(x) {
  x <- x[!is.na(x)]
  n  <- length(x)
  m  <- mean(x)
  se <- sd(x) / sqrt(n)
  t_crit <- qt(0.975, df = n - 1)
  c(inf = m - t_crit * se, sup = m + t_crit * se)
}


ic95_stats <- function(x) {
  x <- x[!is.na(x)]
  n  <- length(x)
  if (n == 0) {
    return(c(media = NA_real_, li = NA_real_, ls = NA_real_, min = NA_real_, max = NA_real_))
  }
  m  <- mean(x)
  s  <- sd(x)
  se <- s / sqrt(n)
  tcrit <- if (n > 1) qt(0.975, df = n - 1) else NA_real_
  li <- if (!is.na(tcrit)) m - tcrit * se else NA_real_
  ls <- if (!is.na(tcrit)) m + tcrit * se else NA_real_
  c(media = m, li = li, ls = ls, min = min(x), max = max(x))
}

# 2.2) Resumo de completude (N missings e %)
na_profile <- function(v) {
  n_total   <- length(v)
  n_na      <- sum(is.na(v))
  pct_na    <- if (n_total > 0) 100 * n_na / n_total else NA_real_
  c(n_total = n_total, n_na = n_na, pct_na = pct_na)
}

fmt <- function(x, dec) {
  ifelse(is.na(x), "",
         format(round(as.numeric(x), dec),
                big.mark = ".", decimal.mark = ",", trim = TRUE))
}

fmt_num <- \(x, dec) 
format(
  round(x, dec),
  big.mark = ".",
  decimal.mark = ",",
  trim = TRUE
)
