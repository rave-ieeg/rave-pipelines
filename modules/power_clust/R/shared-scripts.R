# DIPSAUS DEBUG START
#
# ravepipeline::pipeline_setup_rmd("power_clust")
#
# # 1. create RAVE subject
# subject <- ravecore::new_rave_subject(project_name = project_name, subject_code = subject_code)
#
# # 2. load repository
# ravepipeline::with_rave_parallel({
#   repository <- ravecore::prepare_subject_power_with_epochs(
#     subject = subject,
#     electrodes = electrodes,
#     reference_name = reference_name,
#     epoch_name = epoch_name,
#     time_windows = time_window
#   )
# })
#
# # 3. Calculate baseline percentage change
# ravecore::power_baseline(x = repository,
#                          baseline_windows = unlist(baseline_window),
#                          method = baseline_method)
#
# baseline_power <- ravepipeline::RAVEFileArray$new(repository$power$baselined)


#' Calculate similarity matrix across channels given frequencies, trials, and time-points
#' @param baseline_power baseline-corrected power
#' @param frequency_selector logical vector, length is consistent with the
#' frequency size
#' @param trial_numbers integer vector, corresponding to the epoch trial column
#' @param start_time_per_trial numeric vector, same length as `trial_numbers`,
#' the start-time in seconds for each selected trial
#' @param duration_per_trial similar to `start_time_per_trial`, the duration in
#' seconds for each selected trial
#' @returns A similarity matrix; row and column numbers are identical to
#' the number of channels from the loaded electrodes. Each element can be
#' treated as a "projection" of one channel sequence to the another normalized
#' channel within the given frequencies and time range. The projections are
#' normalized with a t-statistics when multiple trials exist. Higher statistics
#' means the channels are more similar
calc_similarity_matrix <- function(
    baseline_power,
    frequency_selector,
    trial_numbers,
    start_time_per_trial,
    duration_per_trial,
    collapse_frequency = TRUE
) {

  # DIPSAUS DEBUG START
  # frequency_selector <- c(rep(FALSE, 6), rep(TRUE, 10))
  # trial_numbers <- 1:10
  # start_time_per_trial <- rep(0, 10)
  # duration_per_trial <- rep(1, 10)
  # collapse_frequency <- TRUE

  if(inherits(baseline_power, "RAVEFileArray")) {
    # unwrap the baseline_power to filearray
    baseline_power <- baseline_power$`@impl`
  }

  # Calculate similarity_matrix for each trial
  similarities <- lapply(seq_along(trial_numbers), function(trial_ii) {
    # trial_ii <- 1
    trial_number <- trial_numbers[[trial_ii]]
    start_time <- start_time_per_trial[[trial_ii]]
    duration <- duration_per_trial[[trial_ii]]

    end_time <- start_time + duration

    trial_power <- subset(baseline_power,
                          Frequency ~ frequency_selector,
                          Trial ~ Trial == trial_number,
                          Time ~ Time >= start_time & Time <= end_time,
                          drop = FALSE)
    dimnames(trial_power) <- NULL
    trial_power[is.na(trial_power)] <- 0

    if( collapse_frequency ) {
      # if collapse freq
      trial_power <- ravetools::collapse(trial_power, keep = c(2, 4))
    } else {
      # if no collapse
      dm <- dim(trial_power)
      dim(trial_power) <- c(length(trial_power) / dm[[4]], dm[[4]])
    }

    # Standardize
    norm <- sqrt(colSums(trial_power^2, na.rm = TRUE))
    norm[norm == 0] <- 1
    trial_power_standardized_t <- t(trial_power) / norm

    # Similarity (V^tilde %*% V)
    trial_power_standardized_t %*% trial_power
  })

  similarities <- simplify2array(similarities, higher = TRUE)

  dm <- dim(similarities)
  dm_12 <- dm[c(1, 2)]

  # When only one trial is available... This is bad... but we still
  # return a matrix
  if(length(dm) < 3 || dm[[3]] <= 1) {
    dim(similarities) <- dm_12

    # normalize the similarities matrix
    sdiag <- sqrt(diag(similarities))
    similarities <- t(t(similarities / sdiag) / sdiag)
    return(similarities)
  }

  # Average similarity matrix across frequencies
  n <- dm[[3]]
  df <- n - 1

  dim(similarities) <- c(length(similarities) / n, n)

  # mean
  mx <- rowMeans(similarities)

  # variance
  vx <- (rowSums(similarities^2) - n * mx^2) / df

  # standard error of mean
  stderr <- sqrt(vx / n)

  dim(mx) <- dm_12
  dim(stderr) <- dm_12

  is_constant <- stderr < 10 * .Machine$double.eps * abs(mx)
  diag(is_constant) <- TRUE

  # t-statistics
  tstat <- mx / stderr
  scaling <- 1 / max(tstat[!is_constant], na.rm = TRUE)

  tstat <- tstat * scaling
  tstat[is_constant] <- 1
  tstat[tstat < 0] <- 0
  tstat
}


#' Calculate average responses from given frequency and trial configurations
#' @returns a matrix of Time by Channel
calc_average_responses <- function(
    baseline_power,
    frequency_selector,
    trial_numbers,
    start_time_per_trial,
    duration_per_trial
) {

  # DIPSAUS DEBUG START
  # frequency_selector <- c(rep(FALSE, 6), rep(TRUE, 10))
  # trial_numbers <- 1:10
  # start_time_per_trial <- rep(0, 10)
  # duration_per_trial <- rep(1, 10)
  # collapse_frequency <- TRUE

  if(inherits(baseline_power, "RAVEFileArray")) {
    # unwrap the baseline_power to filearray
    baseline_power <- baseline_power$`@impl`
  }

  # Calculate average_responses
  max_duration <- max(duration_per_trial)

  time_points <- dimnames(baseline_power)$Time
  n_time <- sum(time_points <= min(time_points) + max_duration) - 1

  env <- new.env(parent = emptyenv())
  env$trial_power <- 0

  lapply(seq_along(trial_numbers), function(trial_ii) {
    # trial_ii <- 1
    trial_number <- trial_numbers[[trial_ii]]
    start_time <- start_time_per_trial[[trial_ii]]
    # duration <- duration_per_trial[[trial_ii]]

    end_time <- start_time + max_duration

    trial_power <- subset(baseline_power,
                          Frequency ~ frequency_selector,
                          Trial ~ Trial == trial_number,
                          Time ~ Time >= start_time & Time <= end_time,
                          drop = FALSE)
    dimnames(trial_power) <- NULL
    trial_power[is.na(trial_power)] <- 0

    # Time x Channel
    re <- ravetools::collapse(trial_power, keep = c(2, 4))
    ntp <- nrow(re)
    if(ntp > n_time) {
      re <- re[seq_len(n_time), , drop = FALSE]
    } else if (ntp < n_time) {
      re <- cbind(re, array(NA_real_, c(n_time - ntp, ncol(re))))
    }
    env$trial_power <- env$trial_power + re / length(trial_numbers)
    return()
  })

  # trial_power <- simplify2array(trial_power)
  #
  # ravetools::collapse(trial_power, keep = c(1, 2))
  env$trial_power
}

#' Prepare data for given conditions and epoch events or duration
#' @param repository,baseline_power pipeline targets
#' @param frequency_range frequency ranges to subset on; power within the
#' frequency range is considered similar (gamma, alpha, ...)
#' @param group_conditions character vector; condition names (in the epoch
#' `Condition` column) that should be considered in the same group
#' @param event_start start of the event, typically trial onset
#' @param event_end,duration end of the event or the trial duration; at least
#' one must be specified. If `event_end` is given, then the trial duration is
#' computed as from `event_start` to `event_end`; otherwise use `duration`,
#' a numeric vector of duration for each trial in the `group_conditions`;
#' if both are provided, then `duration` is the max cutoff
#' @param start_offset start-time offset; this is relative to the event start,
#' if users want to include or exclude time points near the event start.
#' `duration` and `start_offset` are always relative to the event start.
#' @returns A list of similarity matrix and average_responses (time over channel
#' ) for the condition group
prepare_fpca_data <- function(
    repository,
    baseline_power,
    frequency_range,
    group_conditions,
    event_start = "Trial Onset",
    event_end = NULL,
    start_offset = 0,
    duration = NULL,
    ...
) {

  # DIPSAUS DEBUG START
  # frequency_range <- c(70, 150)
  # group_conditions <- c("known_a")
  # event_start <- "Trial Onset"
  # duration = 0.5

  if(inherits(baseline_power, "RAVEFileArray")) {
    # unwrap the baseline_power to filearray
    baseline_power <- baseline_power$`@impl`
  }

  # Need to calculate time index for each trial
  epoch_table <- repository$epoch_table
  event_start_cname <- repository$epoch$get_event_colname(
    event = event_start, missing = "error")

  if(is.null(event_end) && is.null(duration)) {
    stop("Please specify either `event_end` and/or `duration`")
  }
  if(length(event_end) > 0) {
    if(length(duration) == 0 || is.na(duration)) {
      max_duration <- Inf
    } else {
      max_duration <- rep(duration, ceiling(nrow(epoch_table) / length(duration)))
    }
    event_end_cname <- repository$epoch$get_event_colname(event = event_end)
    duration <- epoch_table[[event_end_cname]] - epoch_table[[event_start_cname]]
    max_duration <- max_duration[seq_along(duration)]

    sel <- duration > max_duration
    duration[sel] <- max_duration[sel]
  } else {
    duration <- rep(duration, ceiling(nrow(epoch_table) / length(duration)))
  }

  # get trial numbers for subset
  epoch_rows <- epoch_table$Condition %in% group_conditions

  # If there is no trial, return nothing
  if(!any(epoch_rows)) { return(NULL) }

  trial_numbers <- epoch_table$Trial[epoch_rows]
  trial_start <- (epoch_table[[event_start_cname]] - epoch_table$Time + start_offset)[epoch_rows]
  trial_duration <- duration[epoch_rows]

  frequency_range <- range(unlist(frequency_range), na.rm = TRUE)
  frequency_selector <- repository$frequency >= frequency_range[[1]] & repository$frequency <= frequency_range[[2]]
  if(!any(frequency_selector)) {
    nearest_idx <- which.min(abs(repository$frequency - mean(frequency_range)))
    frequency_selector[nearest_idx] <- TRUE
  }

  # For each trial, calculate similarity score

  similarity_matrix <- calc_similarity_matrix(
    baseline_power = baseline_power,
    frequency_selector = frequency_selector,
    trial_numbers = trial_numbers,
    start_time_per_trial = trial_start,
    duration_per_trial = trial_duration
  )

  average_responses <- calc_average_responses(
    baseline_power = baseline_power,
    frequency_selector = frequency_selector,
    trial_numbers = trial_numbers,
    start_time_per_trial = trial_start,
    duration_per_trial = trial_duration
  )

  list(
    similarity_matrix = similarity_matrix,
    average_responses = average_responses,
    start_offset = start_offset
  )
}


# For each condition group
# 5. Matrix factorization to the similarity matrix
decompose_similarity_matrix <- function(similarity_matrix, initial_q, min_q = 2, zeta_threshold = 0.5, ...) {
  # similarity_matrix <- structure(c(1, 1, 0.640372587078979, 0.51742008795571, 0.661718353578408,
  #             0.92222156994797, 1, 0.645856190903045, 0.707495475554599, 0.920271471379005,
  #             0.614814524934785, 0.579735065513815, 1, 0.408079476633489, 0.507332982766963,
  #             0.486024668543285, 0.681333231405471, 0.42987707898677, 1, 0.648078911510519,
  #             0.613547895821444, 0.871692145968245, 0.506664752858333, 0.584535012489347,
  #             1), dim = c(5L, 5L))
  # initial_q <- 5
  #
  # rank <- initial_q
  # nmf <- ravetools::naive_nmf(x = similarity_matrix, k = rank)

  # Validate input matrix
  if (!is.matrix(similarity_matrix)) {
    stop("Input `similarity_matrix` must be a matrix.")
  }
  if(anyNA(similarity_matrix)) {
    stop("Similarity matrix may not contain NA")
  }
  N <- nrow(similarity_matrix)
  min_q <- max(min_q, 1)
  initial_q <- min(max(initial_q, min_q), N)

  # Starting NNMF clustering process
  for (rank in seq(from = initial_q, to = min_q, by = -1)) {

    nmf <- ravetools::naive_nmf(x = similarity_matrix, k = rank, verbose = FALSE)

    # NMF package: similarity_matrix (N x N) = basis(nmf_res) (N x Q) %*% coef(nmf_res) (Q x N)
    W_image <- nmf$W # This is N x Q (N x q_current)
    H_image <- nmf$H  # This is Q x N (q_current x N)

    H_image <- H_image * (t(W_image) %*% similarity_matrix) / ((crossprod(W_image) %*% H_image))

    scaling <- sqrt(rowSums(H_image^2))
    H_image <- H_image / scaling
    W_image <- t(t(W_image) * scaling)

    W_image <- W_image * (similarity_matrix %*% t(H_image)) / (W_image %*% tcrossprod(H_image))

    # Calculate degeneracy from HHt
    zeta <- -Inf # Default for cases where calculation is not meaningful (e.g., N=0, or N=1)

    if (N > 1) {
      HHt <- H_image %*% t(H_image) # N x N matrix

      svd_decomp <- svd(HHt)

      # svd_decomp$u %*% diag(svd_decomp$d) %*% t(svd_decomp$v) - HHt
      # svd_decomp$v %*% diag(1 / svd_decomp$d) %*% t(svd_decomp$u) - solve(HHt)

      d <- svd_decomp$d
      min_d <- 1e-4 * abs(max(d))
      d[d < min_d] <- min_d

      HHt_inverse <- svd_decomp$v %*% diag(1 / d) %*% t(svd_decomp$u)
      HHt_inverse_sqrt <- sqrt(diag(HHt_inverse))
      HHt_inverse <- t(HHt_inverse / HHt_inverse_sqrt) / HHt_inverse_sqrt
      zeta <- max(abs(HHt_inverse[upper.tri(HHt_inverse)]))

      # upper_tri_elements <- HHt[upper.tri(HHt)]
      # zeta <- max(upper_tri_elements, na.rm = TRUE)

      if(!is.finite(zeta)) {
        zeta <- -Inf
      }
    } else if (N == 1) { # N=1, H_image is 1xq_current. HHt is 1x1. upper.tri is empty.
      zeta <- -Inf # Max of empty set is -Inf. No off-diagonal elements.
    }

    if (rank == 1 && N > 1) {
      ravepipeline::logger(sprintf("  For Q = %d, calculated zeta = %.4f (Note: Q=1, interpretation of zeta for cluster degeneracy differs)", rank, zeta), level = "trace")
    } else {
      ravepipeline::logger(sprintf("  For Q = %d, calculated zeta = %.4f", rank, zeta), level = "trace")
    }

    current_iter_result <- list(
      Q = rank,
      H_image = H_image,
      W_image = W_image,
      zeta = zeta,
      # nmf_residuals = tryCatch(residuals(nmf_res_obj), error=function(e) NA),
      nmf_object = nmf
    )

    # Check if zeta falls below the threshold
    if (zeta < zeta_threshold) {
      current_iter_result$threshold_reached <- TRUE
      return(current_iter_result)
    }
  }

  current_iter_result$threshold_reached <- FALSE
  return(current_iter_result)

}

# decompose_result <- decompose_similarity_matrix(similarity_matrix, 10, zeta_threshold = 0.1)

# 6.1 calculate initial cluster from the NMF (first run)
pca_initial_cluster <- function(decompose_result) {
  # Initial cluster from H
  cls <- apply(decompose_result$H_image, 2L, which.max)
  cls
}

# 6.2 Calculate PC curves
calc_basis <- function(initial_cluster, average_responses) {
  cls <- initial_cluster

  # Calculate mean
  basis_curves <- sapply(sort(unique(cls)), function(cl) {
    cluster_responses <- average_responses[, cls == cl, drop = FALSE]
    basis <- rowMeans(cluster_responses)

    # Normalize
    basis_norm <- sqrt(sum(basis^2))
    if( basis_norm == 0 ) {
      basis_norm <- 1
    }
    basis / basis_norm
  })

  # Time x cluster
  basis_curves
}

# 6.3 Calculate PC scores (beta)
calc_basis_scores <- function(basis_curves, average_responses, lambda = 0.5) {
  solve(crossprod(basis_curves) + diag(lambda, ncol(basis_curves))) %*% crossprod(basis_curves, average_responses)
}

# 6.4 group-condition-level (high-level) caller
prepare_fpca_per_condition <- function(group, ...) {

  initial_q <- group$initial_rank
  zeta_threshold <- group$zeta_threshold

  ravepipeline::logger("Decomposing similarity matrix for group ", sQuote(group$label), level = "debug")
  fpca_decompose_result <- decompose_similarity_matrix(
    similarity_matrix = group$similarity_matrix,
    initial_q = initial_q,
    zeta_threshold = zeta_threshold,
    ...
  )
  initial_cluster <- pca_initial_cluster(fpca_decompose_result)
  basis_curves <- calc_basis(initial_cluster, group$average_responses)
  basis_scores <- calc_basis_scores(basis_curves, group$average_responses)

  list(
    group_index = group$group_index,
    label = group$label,
    conditions = group$conditions,
    average_responses = group$average_responses,
    initial_cluster = initial_cluster,
    initial_n_clusters = length(unique(initial_cluster)),
    basis_scores = basis_scores,
    basis_curves = basis_curves
  )
}

# 7. combine results (transpose) from condition group
#.   Why do we need this? (clustering and plotting)
combine_condition_groups <- function(..., .list = list()) {

  # results <- list(fpca_data_ac_result, fpca_data_v_result)
  results <- c(list(...), as.list(.list))

  group_indexes <- vapply(results, function(group) { as.integer(group$group_index) }, 1L)
  group_labels <- vapply(results, function(group) { as.character(group$label) }, "")

  group_n_conditions <- vapply(results, function(group) { length(group$conditions) }, 1L)

  # list of time x channel
  average_responses_list <- lapply(results, "[[", "average_responses")

  # concaternate all channels by time
  # rbind(average_responses[[1]], average_responses[[2]], ...)
  average_responses <- do.call("rbind", average_responses_list)

  # For visualizating vertical bars
  group_time_points <- sapply(average_responses_list, nrow)

  initial_n_clusters <- sapply(results, "[[", "initial_n_clusters")

  # basis scores: list of nclust x channel
  basis_scores_list <- lapply(results, "[[", "basis_scores")
  basis_scores <- do.call("rbind", basis_scores_list)

  #
  # res$electrode_channels <- group$electrode_channels
  # res$sample_rate <- group$sample_rate
  # res$event_start <- group$event_start
  # res
  electrode_channels <- results[[1]]$electrode_channels
  sample_rate <- results[[1]]$sample_rate
  group_event_starts <- sapply(results, "[[", "event_start")

  group_start_offset <- sapply(results, "[[", "start_offset")

  return(list(
    group_indexes = group_indexes,
    group_labels = group_labels,
    group_n_conditions = group_n_conditions,
    combined_average_responses = average_responses,
    combined_basis_scores = basis_scores,
    group_n_clusters = initial_n_clusters,
    group_n_time_points = group_time_points,
    group_event_starts = group_event_starts,
    group_start_offset = group_start_offset,
    electrode_channels = electrode_channels,
    sample_rate = sample_rate
  ))
}


# 8. Final clustering
calc_final_cluster_tree <- function(
    combined_group_results,
    distance_method = c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski"),
    cluster_method = c("ward.D2", "ward.D", "single", "complete", "average", "mcquitty", "median", "centroid")
) {

  distance_method <- match.arg(distance_method)
  cluster_method <- match.arg(cluster_method)
  # distance_method <- "euclidean"
  # cluster_method <- "ward.D2"

  combined_basis_scores <- combined_group_results$combined_basis_scores
  n_channels <- ncol(combined_basis_scores)
  max_clusters <- min(prod(combined_group_results$group_n_clusters), n_channels - 1)
  min_clusters <- min(max(combined_group_results$group_n_clusters), n_channels - 1)

  distance_matrix <- stats::dist(t(combined_basis_scores), method = distance_method)
  hc <- stats::hclust(d = distance_matrix, method = cluster_method)

  list(
    cluster_object = hc,
    cluster_range = c(min_clusters, max_clusters),
    distance_matrix = distance_matrix
  )
}

# cut the hclust
calc_final_cut_tree <- function(cluster_result, k, cvi = FALSE) {
  # print(k)
  cls <- stats::cutree(cluster_result$cluster_object, k)
  if( cvi ) {
    sil <-  cluster::silhouette(cls, cluster_result$distance_matrix)
    cvi_index <- mean(sil[,'sil_width'])

    list(
      k = k,
      cluster = cls,
      cvi_index = cvi_index
    )
  } else {
    list(
      k = k,
      cluster = cls
    )
  }
}

# Silhouette score
choose_n_clusters <- function(cluster_result, cluster_range = NULL, plot = TRUE) {

  if(length(cluster_range) > 0) {
    cluster_range <- range(cluster_range, na.rm = TRUE)
  } else {
    cluster_range <- cluster_result$cluster_range
  }

  cluster_k <- seq(cluster_range[[1]], cluster_range[[2]])
  cluster_results <- lapply(cluster_k, function(k) {
    calc_final_cut_tree(cluster_result, k, cvi = TRUE)
  })


  k_list <- sapply(cluster_results, "[[", "k")
  cvi_indexes <- sapply(cluster_results, "[[", "cvi_index")

  # TODO: find the best cluster
  best_k <- k_list[which(cvi_indexes == max(cvi_indexes))]

  if(plot) {
    plot(
      x = k_list,
      y = cvi_indexes,
      main = 'Clustering index',
      xlab = 'number of clusters',
      ylab = "Silhouette score",
      pch = 16,
      axes = FALSE,
      xlim = c(1, max(k_list))
    )
    axis(1, at = c(1, k_list))
    axis(2, pretty(cvi_indexes))
  }


  final_cluster <- cluster_results[[which(k_list == best_k)]]

  list(
    suggested = final_cluster,
    scores = data.frame(k = k_list, silhouette = cvi_indexes)
  )
}
