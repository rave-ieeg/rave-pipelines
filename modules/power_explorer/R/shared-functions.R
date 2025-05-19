# require(dipsaus)
# require(rutabaga)
# require(ravebuiltins)
# require(magrittr)
# require(stringr)
# require(stringi)
# require(data.table)
# require(ravedash)
# require(lmtest)
`%$%` <- magrittr::`%$%`
`%<>%` <- magrittr::`%<>%`
`%>%` <- magrittr::`%>%`
`%within%` <- rutabaga::`%within%`
`%&%` <- rutabaga::`%&%`
`%OF%` <- dipsaus::`%OF%`
`%?<-%` <- dipsaus::`%?<-%`
str_collapse <- rutabaga::str_collapse
which.equal <- rutabaga::which.equal
fast_median <- ravetools::fast_median
rbind_list <- rutabaga::rbind_list
rand_string <- raveio:::rand_string
stopifnot2 <- raveio:::stopifnot2

# require(data.table)
.datatable.aware = TRUE

get_recursive_summary <- function(ll, nm, FUN=range) {
  if(is.null(ll[[nm]])) {
    return(FUN(unlist(lapply(ll, get_recursive_summary, nm=nm))))
  }

  FUN(ll[[nm]])
}

pretty.character <- function(x, ...,  upper=c('first', 'all', 'none')) {

  cap_first_letter <- function(s) {
    paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)), collapse='')
  }

  upper = match.arg(upper)

  str <- stringr::str_split(x, '_')[[1]]

  if(upper == 'first') {
    str[1] %<>% cap_first_letter
  } else if (upper == 'all') {
    str %<>% sapply(cap_first_letter)
  }

  return(paste(str, collapse=" "))
}

unpretty <- function(str, ...) {
  UseMethod('unpretty')
}

unpretty.default <- function(str, ...) {
  return(str)
}

unpretty.character <- function(str, ...) {
  tolower(stringr::str_replace_all(str, " ", '_'))
}


get_from_arr <- function(x, v, FUN=`%in%`) {
  FUN = match.fun(FUN)
  x[FUN(x,v)]
}

# note that negate=TRUE is almost always the same as just calling
# get_from_arr
remove_from_arr <- function(x, v, FUN=`==`, negate=FALSE) {
  FUN <- if(negate) {
    base::Negate(FUN)
  } else {
    match.fun(FUN)
  }
  x[!FUN(x,v)]
}

signed_floor <- function(x) {
  sign(x)*floor(abs(x))
}

tensor_reshape <- function(mat, orig_dim, pivot) {
  # ensure we're pivoting within the original matrix
  stopifnot(pivot <= length(orig_dim))

  if(is.list(mat)) {
    mat = do.call(c, mat)
  }

  # redimension the matrix
  pivot_dim_size <- length(mat) / prod(orig_dim[-pivot])
  new_dims <- c(orig_dim[-pivot], pivot_dim_size)
  dim(mat) = new_dims

  # put in order based on pivot location
  k = length(orig_dim)
  if(pivot == 1) {
    perm = c(k, seq_len(k-1))
  } else if (pivot==k) {
    perm = seq_len(k)
  } else {
    # here 1 < pivot < k
    perm = c(seq_len(pivot-1), k, seq(from=pivot, to=k-1))
  }

  return(
    aperm(mat, perm)
  )

}


which_have_trials <- function(analysis_groups) {
  which(sapply(analysis_groups, function(ag) isTRUE(ag$has_trials)))
}

determine_available_shift <- function(event_offsets, available_time, sample_rate) {
  time_range = round(range(event_offsets), 7)

  # we want to be conservative, so we want to shrink the range as needed. so we use signed_floor
  signed_floor(sample_rate * (available_time-time_range))/sample_rate
}

determine_relative_shift_amount <- function(available_shift, event_time, sample_rate, available_time_points) {
  new_range_ind = abs(round(available_shift*sample_rate))

  new_0_ind = round(sample_rate*(event_time - min(available_time_points)))

  return(new_0_ind - new_range_ind[1])
}


get_pluriform_power <- function(
    baselined_data, trial_indices, events, epoch_event_types,
    event_of_interest, trial_outliers_list, sample_rate,
    final_data_only = FALSE) {

  res <- list()

  # res <- list(
  #   data = subset(baselined_data, Trial ~ Trial %in% trial_indices)
  # )

  stopifnot(which(names(dimnames(baselined_data)) == 'Trial')==3)
  ti <- as.numeric(dimnames(baselined_data)$Trial) %in% trial_indices

  res <- list(
    data = baselined_data[,,ti,,drop=FALSE]
  )

  # now check if we need shifted data
  # R is copy on write, so no worries here about memory
  res$shifted_data = res$data
  shift_amount = NULL
  # event_of_interest = 'Lag_1'
  if(event_of_interest != epoch_event_types[1]) {
    # stop("shifting data not supported yet!")
    ravedash::logger('Shifting data to: ' %&% event_of_interest, level='debug')

    event_of_interest = paste0('Event_', event_of_interest)
    event_offsets = events[[event_of_interest]][ti] - events$Time[ti]

    times = as.numeric(dimnames(res$data)$Time)

    new_range = determine_available_shift(event_offsets,
      available_time = range(times),
      sample_rate = sample_rate
    )

    # ravedash::logger('available shift: ' %&% paste0(new_range, collapse=':'))

    shift_amount = determine_relative_shift_amount(
      event_time = event_offsets,
      available_shift=new_range,
      available_time_points = times,
      sample_rate = sample_rate
    )

    # ravedash::logger('dispaus::shift')

    # this is checked above
    # stopifnot('Trial' == names(dimnames(res$data))[3])
    stopifnot('Time' == names(dimnames(res$data))[2])

    if(length(shift_amount) != dim(res$data)[3L]) {
      # assign('shift_amt', shift_amount, envir = globalenv())
      # assign('event_mat', events, envir = globalenv())
      stop('shift amount != # trials... stopping')
    }

    res$shifted_data = get_shifted_data(data = res$data, shift_amount = shift_amount)

    ## now we need to update the time dimension to reflect the new data range
    # fill out times, then subset if we need to
    new_time = round(seq(from=new_range[1], by = 1/sample_rate, length.out = dim(res$data)[2L]), 7)
    to_keep = new_time %within% new_range
    if(any(!to_keep)) {
      res$shifted_data = res$shifted_data[,to_keep,,,drop=FALSE]
    }
    dimnames(res$shifted_data)$Time = new_time[new_time %within% new_range]

    # alright, now that we've shifted the data we also need to shift the events dataset, so that future sorts on the event_of_interest don't do anything
    # ravedash::logger('updating events file')
    nms <- paste0('Event_', epoch_event_types[-1])
    events[nms] <- events[nms] - events[[event_of_interest]]
    # ravedash::logger('done with shifting', level='debug')
  }

  # handle outliers
  if(length(trial_outliers_list) == 0) {
    res$clean_data <- res$data
    res$shifted_clean_data <- res$shifted_data
  } else {
    ravedash::logger('Handling outliers...')

    # UPDATED res$data is not a filearray now so res$data$subset is not a function!
    #res$clean_data <- res$data$subset(Trial = !(Trial %in% trial_outliers_list))
    #res$shifted_clean_data <- res$shifted_data$subset(Trial = !(Trial %in% trial_outliers_list))

    ti = ! (dimnames(res$data)$Trial %in% trial_outliers_list)

    res$clean_data = res$data[,,ti,,drop=FALSE]
    res$shifted_clean_data = res$shifted_data[,,ti,,drop=FALSE]
  }

  # make sure to save out the updated time stamps to be used later
  res$events = events

  if(final_data_only) {
    return(res$shifted_clean_data)
  }

  return(res)
}

shift_baselined_power <- function(
    baselined_data, events, epoch_event_types, event_of_interest, sample_rate,
    final_data_only = FALSE) {

  dn.orig <- dimnames(baselined_data)
  stopifnot(names(dn.orig)[3] == 'Trial', 'Time' == names(dn.orig)[2])

  shift_amount = NULL

  re <- baselined_data

  # event_of_interest = 'Lag_1'
  if(event_of_interest != epoch_event_types[1]) {
    # stop("shifting data not supported yet!")
    ravedash::logger('Shifting data to: ' %&% event_of_interest, level='debug')

    # get the times for the trials needed
    ti <- events$Trial %in% as.numeric(dimnames(baselined_data)$Trial)
    event_column_name = paste0('Event_', event_of_interest)
    event_offsets = events[[event_column_name]][ti] - events$Time[ti]

    times = as.numeric(dn.orig$Time)

    new_range = determine_available_shift(event_offsets,
                                          available_time = range(times),
                                          sample_rate = sample_rate
    )

    # ravedash::logger('available shift: ' %&% paste0(new_range, collapse=':'))

    shift_amount = determine_relative_shift_amount(
      event_time = event_offsets,
      available_shift=new_range,
      available_time_points = times,
      sample_rate = sample_rate
    )

    if(length(shift_amount) != length(dn.orig$Trial)) {
      # assign('shift_amt', shift_amount, envir = globalenv())
      # assign('event_mat', events, envir = globalenv())
      stop('shift amount != # trials... stopping')
    }

    re = get_shifted_data(data = baselined_data, shift_amount = shift_amount)

    ## now we need to update the time dimension to reflect the new data range
    # fill out times, then subset if we need to
    new_time = round(seq(from=new_range[1], by = 1/sample_rate, length.out = length(dn.orig$Time)), 7)
    to_keep = new_time %within% new_range
    if(any(!to_keep)) {
      re = re[,to_keep,,,drop=FALSE]
    }
    dimnames(re)$Time = new_time[new_time %within% new_range]

    # alright, now that we've shifted the data we also need to shift the events dataset, so that future sorts on the event_of_interest don't do anything
    # ravedash::logger('updating events file')
    nms <- paste0('Event_', epoch_event_types[-1])
    events[nms] <- events[nms] - events[[event_column_name]]


    ravedash::logger('done!', level='debug')
  }

  # make sure to save out the updated time stamps to be used later
  return(list(events = events, data=re))

}


get_shifted_data <- function(data, shift_amount, new_range) {
  dn <- names(dimnames(data))

  # make sure the indices below line up with what we're expecting
  stopifnot('Electrode' == dn[4], 'Trial'== dn[3], 'Time' == dn[2])

  for(ii in seq_len(dim(data)[4L])) {
    subarr <- data[,,, ii, drop = FALSE]

    data[,,,ii] <- ravetools::shift_array(
      x = subarr,
      along_margin = 2L, shift_amount = shift_amount, unit_margin = 3L
    )
  }

  data
}


build_heatmap_data <- function(data, data_wrapper, do_censor=FALSE, censor_window=NULL, analysis_settings, ...) {

  hd <- data_wrapper(
    # hmd is using the clean data
    data$collapse(keep = c(3,2), method = analysis_settings$collapse_method),
    xlab='Time (s)', ylab='Frequency', zlab='auto',
    x = data$dimnames$Time,
    y = data$dimnames$Frequency,
    N = dim(data)[1L], ...
  )

  if(!missing(analysis_settings)) {
    hd[names(analysis_settings)] = analysis_settings
  }

  if(do_censor && !is.null(censor_window)) {
    hd$range <- .fast_range(hd$data[!(hd$x %within% censor_window), ] )
  }

  return(hd)
}

build_heatmap_correlation_data <- function(data, data_wrapper,
  analysis_settings, analysis_settings2, ...) {

  time_index1 <- data$dimnames$Time %within% analysis_settings$analysis_window
  time_index2 <- data$dimnames$Time %within% analysis_settings2$analysis_window

  if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
    t_clean <- !(data$dimnames$Time %within% analysis_settings$censor_window)

    time_index1 = time_index1 & t_clean
    time_index2 = time_index2 & t_clean
  }

  t1 <- data$subset(Time=time_index1)$collapse(keep = c(3,2), method = analysis_settings$collapse_method)
  t2 <- data$subset(Time=time_index2)$collapse(keep = c(3,2), method = analysis_settings$collapse_method)

  # image(cor(cbind(t1,t2)), col=colorRampPalette(c('navy', 'white', 'red'))(101), y=data$dimnames$Frequency, x=data$dimnames$Frequency)

  # fastest to just get the whole cmat, but then subset it so that it only includes the t1-t2 correlations
  ind <- 1:dim(t1)[2L]
  cmat <- cor(cbind(t1,t2))[-ind,-ind]
  # image(t(cmat)[,rev(ind)], axes=F,
  #     y=data$dimnames$Frequency, x=data$dimnames$Frequency,
  #     col=colorRampPalette(c('navy', 'white', 'red'))(101), zlim=c(-1,1), xlab='Frequency at Time 1', ylab='Frequency at Time 2')
  # axis(2, at=data$dimnames$Frequency, las=1, lwd=0)
  #
  data_wrapper(
    data=cmat,
    xlab=sprintf('%s from %ss', analysis_settings$unit_of_analysis, str_collapse(analysis_settings$analysis_window, '-')),
    ylab=sprintf('%s from %ss', analysis_settings$unit_of_analysis, str_collapse(analysis_settings2$analysis_window, '-')),
    zlab='Frequency-wise Pearson correlation',
    x = data$dimnames$Frequency,
    y = data$dimnames$Frequency,
    N = 2*dim(data)[2L],
    frequency_window1 = analysis_settings$frequency_window,
    frequency_window2 = analysis_settings2$frequency_window,
    ...
  )
}
build_by_trial_heatmap_data <- function(data, data_wrapper, analysis_settings, ...) {
  bthd <- data_wrapper(
    data$collapse(keep = c(3,1), method = analysis_settings$collapse_method),
    x = data$dimnames$Time,
    y = seq_along(data$dimnames$Trial),
    xlab='Time (s)', ylab='Trial', zlab='auto'
  )

  bthd[names(analysis_settings)] = analysis_settings

  if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
    bthd$range <- .fast_range(bthd$data[ !(bthd$x %within% analysis_settings$censor_window), ])
  }

  return(bthd)
}

build_over_time_correlation_data <- function(f1, f2,
  lag_length=50, ...) {

  # time_index1 <- f1$dimnames$Time %within% f1$analysis_window
  # time_index2 <- f1$dimnames$Time %within% f2$analysis_window

  # warning('over_time_correlation is ignoring censoring information')

  # if(do_censor && !is.null(censor_window)) {
  #     t_clean <- !(f1$dimnames$Time %within% censor_window)
  #
  #     time_index1 = time_index1 & t_clean
  #     time_index2 = time_index2 & t_clean
  # }

  t1a <- f1$data[,1]
  t2a <- f2$data[,1]

  # nb: negative lag means t2 is "ahead" of t1
  lag_corr <- c(
    rev(lagged_cor(t1a,t2a,len=lag_length)),
    (lagged_cor(t2a,t1a,len=lag_length))
  )

  #remove the duplicate at 0
  lag_corr <- lag_corr[-(lag_length+1)]
  abs_ind <- which.max(abs(lag_corr))
  signed_ind <- which.max(lag_corr)
  lags <- -lag_length:lag_length

  return(list(
    correlations = lag_corr,
    ind_max_cor = signed_ind,
    ind_max_abs_cor = abs_ind,
    lags = lags,
    zf1 = scale(t1a),
    zf2 = scale(t2a),
    rf1 = rank(t1a),
    rf2 = rank(t2a)
  ))
}
build_electrode_heatmap_data <- function(data, data_wrapper, analysis_settings, ...) {
  ehd <- data_wrapper(
    data$collapse(keep = c(3,4), method = analysis_settings$collapse_method),
    x=data$dimnames$Time,
    y=data$dimnames$Electrode,
    xlab='Time (s)', ylab='Electrode', zlab='auto', ...
  )

  ehd[names(analysis_settings)] = analysis_settings

  if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
    ehd$range <- .fast_range(ehd$data[ !(ehd$x %within% analysis_settings$censor_window), ])
  }

  return(ehd)
}

build_over_time_data <- function(data, data_wrapper, analysis_settings, ...) {
  otd <- data_wrapper(t(
    apply(data$collapse(keep = 3:4, method = analysis_settings$collapse_method),
      1, .fast_mse)
  ),
    xlab='Time (s)', ylab='auto', N=dim(data)[4L], x=data$dimnames$Time, ...
  )


  otd[names(analysis_settings)] <- analysis_settings

  # set NA (divide by zero) error bars to 0
  otd$data[is.na(otd$data[,2]),2] <- 0

  # we want to make a special range for the line plot data that takes into account mean +/- SE
  if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
    otd$range <- .fast_range(rutabaga::plus_minus(otd$data[!(otd$x %within% analysis_settings$censor_window),]))
  } else {
    otd$range <- .fast_range(rutabaga::plus_minus(otd$data))
  }

  if(!all(is.finite(otd$range))) {
    if(any(is.nan(otd$data[,1]))) {
      stop('Unable to plot data, data contain NaN')
    } else {
      stop(paste0('non-finite range... ', paste(otd$range, collapse='|')))
    }
  }

  return(otd)
}

build_scatter_bar_data <- function(data, data_wrapper,
  analysis_settings, group_info, jitter_seed, ...) {

  time_ind <- data$dimnames$Time %within% unlist(analysis_settings$analysis_window)
  if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
    time_ind = time_ind & (! data$dimnames$Time %within% unlist(analysis_settings$censor_window))
  }

  sbd <- data_wrapper(
    rowMeans(data$subset(Time = time_ind, data_only = TRUE)),
    xlab='Group', ylab='auto'
  )

  # add in the analysis settings so that we have them
  sbd[names(analysis_settings)] = analysis_settings

  # Although this seems to be the wrong place to do this, not sure where else we can do it
  # to enable point identification later, we need to know the x-location of each point. So the jittering
  # needs to be done here.
  .xp <- barplot(which(group_info$group_statuses),plot=FALSE)
  .r <- if(sum(group_info$group_statuses)>1) {
    mean(unique(diff(.xp)))*0.25
  } else {
    0.75*(1/3)
  }

  xpi <- which(group_info$current_group == which(group_info$group_statuses))
  sbd$xp <- .xp[xpi]

  sbd$x <- R.utils::withSeed({
    .xp[xpi] + runif(length(sbd$data), -.r, .r)
  }, seed = jitter_seed)

  # for the scatter_bar_data we also need to get m_se within condition, this is ALWAYS with the clean data
  sbd$mse <- .fast_mse(sbd$data[sbd$is_clean])

  return(sbd)
}

build_scatter_bar_correlation_data <- function(sb1, sb2, data_wrapper, ...) {
  dv <- attr(sb1$data, 'ylab')
  do_str <- function(d) {
    paste0(dv,' from ', str_collapse(d$my_analysis_window, '-'), 's, at ',
      str_collapse(d$my_frequency_window,':'), 'Hz')
  }

  data_wrapper(
    data = cor(sb1$data, sb2$data),
    x = sb1$data,
    y = sb2$data,
    xlab=do_str(sb1),
    ylab=do_str(sb2), ...
  )
}

get_p.adjust_method <- function(pval_filter=c('p', 'FDR(p)', 'Bonf(p)')) {
  pval_filter = match.arg(pval_filter)
  c('p'='none', 'FDR(p)'='fdr', 'Bonf(p)'='bonferroni')[pval_filter]
}

combine_emmeans_results <- function(r) {
  # as.data.frame(r$emmeans)
  # as.data.frame(r$contrasts)

  if(all(c('Electrode', 'Group') %in% names(as.data.frame(r$emmeans)))) {
    d = as.data.frame(r$emmeans)
    d$Electrode <- paste0('E', d$Electrode)
    gind <- which(names(d) == 'Group')
    tbl = d[,-gind]
    tbl$Electrode %<>% paste(as.character(d$Group), sep=':')
  } else {
    tbl = as.data.frame(r$emmeans)
  }
  names(tbl)[1:2] = c('label', 'estimate')

  contr = as.data.frame(r$contrasts)
  if('Electrode' %in% names(contr)) {
    contr$label = contr$Electrode %&% ':' %&% contr$contrast
    contr = subset(contr, select = -c(contrast, Electrode))
    rbind(tbl, contr)
  } else {
    rbind(tbl, contr %>% set_colnames(colnames(tbl)))
  }
}

summary_stat.random_intercept <- function(overall_stats) {
  if(nlevels(overall_stats$Group) > 1) {
    return (
      combine_emmeans_results(
        emmeans::emmeans(
          lmerTest::lmer(y ~ Group + (1|Electrode), data = overall_stats),
          options = list(infer=c(F,T)),
          specs = pairwise ~ Group, infer=c(F,T))
      )
    )
  }

  res <- as.data.frame(emmeans::emmeans(lmerTest::lmer(
    y ~ 1 + (1|Electrode), data=overall_stats
  ), specs=~1, options=list(infer=c(F,T))))
  names(res)[1:2] = c('label', 'estimate')

  return(res)
}

summary_stat.contrasts_per_electrode <- function(overall_stats) {
  if(nlevels(overall_stats$Group) > 1) {
    return(
      combine_emmeans_results(
        emmeans::emmeans(
          lmerTest::lmer(y ~ Group*Electrode + (1|Electrode), data = overall_stats),
          options = list(infer=c(F,T)),
          specs = pairwise ~ Group|Electrode, infer=c(F,T))
      )
    )
  }

  res <- as.data.frame(emmeans::emmeans(
    lmerTest::lmer(y ~ Electrode + (1|TrialNumber), data=overall_stats)
    , specs = pairwise ~ Electrode, options=list(infer=c(F,T)))$emmeans
  )
  names(res)[1:2] = c('label', 'estimate')
  res$label = 'E' %&% res$label
  return(res)
}

summary_stat.collapse_electrode <- function(overall_stats) {
  .d <- aggregate(y ~ TrialNumber+Group, mean, data=overall_stats)

  if(nlevels(.d$Group) > 1) {
    return (combine_emmeans_results(emmeans::emmeans(lm(y ~ Group, data=.d),
      options = list(infer=c(F,T)),
      pairwise ~ Group, infer=c(F,T)))
    )
  }
  res = as.data.frame(
    emmeans::emmeans(lm(y ~ 1, data=.d), ~ 1, options=list(infer=c(F,T)))
  )
  names(res)[1:2] = c('label', 'estimate')
  return(res)
}

summary_stat.fixed_effect <- function(overall_stats) {
  if(nlevels(overall_stats$Group) > 1) {
    return(
      combine_emmeans_results(emmeans::emmeans(
        lmerTest::lmer(y ~ Group*Electrode + (1|TrialNumber), data=overall_stats),
        options = list(infer=c(F,T)),
        specs=pairwise~Electrode*Group, infer=c(F,T)
      ))
    )
  }

  combine_emmeans_results(emmeans::emmeans(lmerTest::lmer(y~factor(Electrode)+(1|TrialNumber), data=overall_stats),
    options = list(infer=c(T,T)),
    specs=pairwise~Electrode, infer=c(F,T))
  )
}

get_summary_statistics <- function(overall_stats, analysis_type) {
  switch(analysis_type,
    'Random intercept' = summary_stat.random_intercept(overall_stats),
    'Contrasts per electrode' = summary_stat.contrasts_per_electrode(overall_stats),
    'Collapse electrode' = summary_stat.collapse_electrode(overall_stats),
    'Fixed effect' = summary_stat.fixed_effect(overall_stats)
  )
}

# available units of analysis
get_unit_of_analysis <- function(requested_unit, names=FALSE) {
  ll = list(
    '% Change Power' = 'percentage',
    '% Change Amplitude' = 'sqrt_percentage',
    'z-score Power' = 'zscore',
    'z-score Amplitude' = 'sqrt_zscore',
    'decibel' = 'decibel'
  )

  if(missing(requested_unit)) {
    if(names) return (names(ll))

    return (ll)
  }

  if(!any(requested_unit == names(ll))) {
    warning("requested unit of analysis not available: ", requested_unit, '. Returning % Change Power')
    return(ll[['% Change Power']])
  }

  return(ll[[requested_unit]])
}

get_unit_of_analysis_varname <- function(uoa){
  ll = list('percentage' = 'Pct_PowerChange',
    'sqrt_percentage' = 'Pct_AmpChange',
    'zscore' = 'Z_PowerChange',
    'sqrt_zscore' = 'Z_AmpChange',
    'decibel' = 'Decibel_PowerChange'
  )

  if(missing(uoa)) return (ll)

  gu = get_unit_of_analysis(names=TRUE)
  if(uoa %in% gu) {
    uoa %<>% get_unit_of_analysis
  }

  ll[[uoa]]
}

get_baseline_scope <- function(requested_unit, names=FALSE) {

  ll = list(
    "Per frequency, trial, and electrode" = c("Trial", "Frequency", "Electrode"),
    "Across trials (aka global baseline)" = c("Frequency", "Electrode"),
    "Across trials and electrodes" = c("Frequency"),
    "Across electrodes only" = c("Trial", "Frequency")
  )


  if(missing(requested_unit)) {
    if(names) return (names(ll))

    return (ll)
  }

  if(!any(requested_unit == names(ll))) {
    warning("requested unit of analysis not available: ", requested_unit, '. Returning "Per frequency, trial, and electrode"')
    return(ll[["Per frequency, trial, and electrode"]])
  }

  return(ll[[requested_unit]])
}

wrap_data = function(data, ...){
  ll <- list(
    data = data,
    range = .fast_range(data),
    N = N,
    trials = epoch_data_subset$Condition,
    Trial_num = group_data[[ii]]$Trial_num,
    is_clean = !(epoch_data_subset$Trial %in% trial_outliers_list),
    name = group_data[[ii]]$name,
    has_trials = group_data[[ii]]$has_trials,
    conditions = group_data[[ii]]$conditions,
    stimulation_window = stimulation_window,
    censor_stimulation_window = censor_stimulation_window,
    electrodes = requested_electrodes,
    events = events,
    trial_alignment = event_of_interest,
    subject_code=subject_code,
    group_info = group_info
  )

  ## by default we use the f1 setttings
  ll[names(f1_analysis_settings)] = f1_analysis_settings

  vals = list(...)

  for (k in c('ylab', 'zlab')) {
    if (isTRUE(vals[[k]] == 'auto')) {
      vals[[k]] = 'Mean ' %&% unit_of_analysis
    }
  }

  for(k in names(vals)) {
    # check for attribute labels
    if (k %in% c('xlab', 'ylab', 'zlab')) {
      attr(ll$data, k) = vals[[k]]
    }

    # all other values just add into the data list
    else {
      ll[[k]] = vals[[k]]
    }
  }

  return (ll)
}

add_analysis_settings <- function(ll, analysis_settings, baseline_settings) {
  append(ll, list(
    subject_code = analysis_settings$subject_code,
    analysis_group = analysis_settings$label,
    analysis_event = analysis_settings$event,
    analysis_window = analysis_settings$time,
    analysis_frequency = analysis_settings$frequency,
    baseline_window = baseline_settings$window[[1]],
    baseline_scope = baseline_settings$scope,
    unit_of_analysis = baseline_settings$unit_of_analysis
  ))
}

data_builder <- function(pluriform_power, condition_groups, baseline_settings,
                         BUILDER_FUN, data_type='shifted_clean_data_Fsub') {

  tmp <- mapply(function(pp, cg) {
    # pp = pluriform_power[[1]]
    # cg = condition_groups[[1]]
    # power are nested within analysis groups
    sapply(pp, function(ppa) {
      # ppa = pp[[1]]
      res <- BUILDER_FUN(data=ppa$data[[data_type]],
                  analysis_settings=ppa$settings,
                  condition_group = cg,
                  baseline_settings = baseline_settings) %>%
        add_analysis_settings(ppa$setting, baseline_settings)

      # add in other details the user may have forgotten
      res$condition_group %?<-% cg$label
      res$electrodes %?<-% as.integer(dimnames(ppa$data[[data_type]])$Electrode)

      return(res)

    }, simplify = FALSE, USE.NAMES = TRUE)
  }, pluriform_power, condition_groups, SIMPLIFY = FALSE)

  # flatten the result
  do.call(c, tmp)
}



export_something_great <- function(pipeline, ...) {
  repo <- pipeline$read('repository')

  dest <- tempfile()
  arr <- filearray::filearray_load_or_create(
    filebase = dest, dimension = unname(repo$power$dim), type = 'double',
    repo_signature = repo$signature,
    electrode_list = repo$electrode_list,
    on_missing = function(arr) {
      for(ii in seq_along(repo$electrode_list)) {
        print(ii)
        e <- repo$electrode_list[[ii]]
        arr[,,,ii] <- repo$power$data_list[[sprintf("e_%d", e)]][]
      }
      dimnames(arr) <- repo$power$dimnames
      arr
    }
  )
  arr$get_header('electrode_list')
  # fa <- filearray::repo$power$data_list
}

export_electrode_level_data <- function(pipeline, ...) {

}

# build_data_for_export <- function(pipeline, ...) {
#
# }

get_available_events <- function(columns) {
  eet <- stringr::str_subset(columns, stringr::fixed('Event_'))

  if(length(eet) > 0) {
    eet <- stringr::str_remove_all(eet, stringr::fixed('Event_'))
  }

  eet <- c("Trial Onset", eet)

  return(eet)
}

round_pval <- function(pval) {
  lpval = pmax(round(log10(.Machine$double.eps)), log10(pval))
  ifelse(lpval > -3.5,
    formatC(round(pval,4),width = 4, digits=4),
    paste0('1e', formatC(round(lpval), width=3,flag=0)))
}

trial_export_types <- function() {
  return(
    list(
      'CLP_CND' = 'Collapsed by condition column',
      'CLP_GRP' = 'Collapsed by grouping factors',
      'RAW_GRP' = 'Raw, Only trials used in grouping factors',
      'RAW_ALL' = 'Raw, All available trials')
  )
}

time_export_types <- function() {
  return(
    list(
      'CLP_AWO' = 'Collapsed, Analysis window(s) only',
      'RAW_AWO' = 'Raw, Analysis window(s) only',
      'RAW_ALL' = 'Raw, All available times'
    )
  )
}

frequency_export_types <- function() {
  return(
    list(
      'CLP_AWO' = 'Collapsed, Analysis window(s) only',
      'RAW_AWO' = 'Raw, Analysis window(s) only',
      'RAW_ALL' = 'Raw, All available frequencies'
    )
  )
}

new_shift_array <- function() {
  repository <- raveio::prepare_subject_power('demo/DemoSubject', time_windows = c(-1,2))

  # baseline
  raveio::power_baseline(repository, baseline_windows = c(-1,0))

  shift_amount <- sample(1:10, size = repository$epoch$n_trials, replace = TRUE)

  # start shift array

  # create a temporary file relative to the app session. it persists through out
  # the RAVE session, and can be restored even browser is closed (shiny session reset)
  pathdir_app_persist <- ravedash::temp_dir(persist = "app-session")

  # get repository signature so the shifted array is linked to this signature
  # If the repository does not change, the filearray path need not change
  # this will prevent spawning too many garbage files
  shiftarray_basename <- sprintf(
    "shiftarray_%s",
    dipsaus::digest(list(
      repository = repository$signature
    ))
  )

  # create file array with cache information
  shifted_array <- filearray::filearray_load_or_create(
    filebase = file.path(pathdir_app_persist, shiftarray_basename),
    dimension = dim(repository$power$baselined),
    type = "float", # or "double"
    mode = "readwrite", partition_size = 1L,

    # make sure if baseline change, this array will change
    baselined_signature = repository$power$baselined$.header$rave_signature,

    # make sure if shift_amount changes, this array will change
    # this checking is type-sensitive
    shift_amount = as.integer(shift_amount),

    on_missing = function(arr) {
      # create one!
      baselined <- repository$power$baselined
      n_electrodes <- dim(baselined)[[4]]

      raveio::lapply_async(seq_len(n_electrodes), function(ii) {
        subarr <- baselined[,,,ii, drop = FALSE]

        # trial is now at 3rd margin, time is at 2nd margin
        shifted_array <- ravetools::shift_array(subarr, along_margin = 2L, shift_amount = shift_amount, unit_margin = 3L)
        arr[,,,ii] <- shifted_array
      })
    }
  )

  # change dimnames
  dnames <- dimnames(repository$power$baselined)
  # dnames$Time <- ...

  dimnames(shifted_array) <- dnames
}


count = function(x) {
  if(is.null(x)) return (1)
  length(unique(x))
}

#
count_elements <- count

### UI impl to share the exporting code where possible
customDownloadButton <- function(outputId, label='Export', class=NULL, icon_lbl="download", ...) {
  tags$a(id = outputId,
    class = paste("btn btn-default shiny-download-link", class),
    href = "", target = "_blank", download = NA,
    ravedash::shiny_icons[[icon_lbl]], label, ...)
}


## for downloading images
build_modal_plot_download <- function(download_plot_info, outputId='do_download_plot') {
  shiny::showModal(shiny::modalDialog(
    title = "Download plot",
    size = "m",
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::downloadButton(
        outputId = ns(outputId),
        label = "Download plot",
        class = "btn-primary"
      )
    ),
      shiny::fluidRow(
        shiny::column(4, shiny::p(shiny::strong("Plot name:"), pretty(download_plot_info$id))),
        shiny::column(3, shiny::numericInput(ns('download_plot_width'), 'Width (in)',
                                                 min = 0.1, max=100, value = 7, step=.25)
        ),
        shiny::column(3, shiny::numericInput(ns('download_plot_height'), 'Height (in)',
                                                 min = 0.1, max=100, value = 4, step=.25)
        ),
        shiny::column(2, shiny::selectInput(ns('download_plot_type'), "Type", selected='pdf',
                                            choices = c('pdf', 'png', 'jpeg', 'bmp', 'tiff (lzw)')))
      )
  ))

}

get_order_of_magnitude <- function(x) {
  floor(log10(abs(x)))
}

# helper to allow passing in the index of a column/row alongside the data
apply_ii <- function (X, MARGIN, FUN, ..., simplify = TRUE) {
  apply(X, MARGIN, FUN, ii=1, ..., simplify = simplify)
}


# from an emmGrid, get all possible pairwise comparisons
# for all possible 1- and 2-way slices
get_stratified_contrasts <- function(emmGrid) {
  fe = names(emmGrid@levels)

  holdouts = fe;
  if(length(fe) > 2) {
    holdouts = append(fe, combn(fe,2,simplify=FALSE))
  }

  lapply(holdouts, function(out) {
    suppressMessages(emmeans::emmeans(
      emmGrid,
      as.formula(
        sprintf("pairwise ~ %s", paste0(collapse='*',fe[! fe %in% out]))
      ),
      by = out
    ))}) %>% setNames(sapply(holdouts, paste0, collapse='.'))
}
