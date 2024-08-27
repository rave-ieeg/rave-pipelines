TrialGroupVariable <- R6::R6Class(
  classname = "TrialGroupVariable",
  inherit = raveio::RAVEVariable,
  private = list(
    # group order (used by group collection)
    .order = NA_integer_,

    # group id, used to calculate variable name
    .id = character(0L),

    .epoch = NULL

  ),
  active = list(
    order = function(v) {
      if(!missing(v)) {
        v <- checkmate::assert_int(v, na.ok = TRUE, lower = 1L, coerce = TRUE)
        private$.order <- v
      }
      private$.order
    },
    id = function(v) {
      if(!missing(v)) {
        checkmate::assert_string(
          v,
          na.ok = FALSE,
          min.chars = 1,
          ignore.case = TRUE,
          null.ok = FALSE,
          .var.name = "RAVETrialGroup -> id",
          pattern = "^[a-zA-Z0-9][a-zA-Z0-9_\\.]{0,}$"
        )
        private$.id <- v
        if(!nzchar(self$name)) {
          self$name <- v
        }
      }
      private$.id
    },
    varname = function() {
      sprintf("cond_%s", self$id)
    },

    epoch = function() {
      if(inherits(private$.epoch, "RAVEEpoch")) {
        return(private$.epoch$table)
      }
      return(private$.epoch)
    },

    selected_rows = function(v) {
      if(!missing(v)) {
        self$set_value(v)
      }
      self$get_value()
    },

    selected_epoch = function() {
      self$epoch[self$selected_rows, , drop = FALSE]
    },

    available_events = function() {
      cnames <- names(self$epoch)
      cnames <- cnames[startsWith(cnames, "Event_")]
      gsub("^Event_", "", cnames)
    },

    trial_num = function(v) {
      epoch_table <- self$epoch
      if(!missing(v)) {
        v <- dipsaus::parse_svec(v)
        v <- which(epoch_table$Trial %in% v)
        self$set_value(v)
      } else {
        v <- self$get_value()
      }
      epoch_table$Trial[ v ]
    },

    has_trials = function() {
      return(length(self$get_value()) > 0)
    },

    conditions = function(v) {
      epoch_table <- self$epoch
      if(!missing(v)) {
        v <- which( epoch_table$Condition %in% v )
        self$set_value(v)
      } else {
        v <- self$get_value()
      }
      conditions <- unique(epoch_table$Condition[ v ])
      sort(as.character(conditions))
    },

    isTrialGroupVariable = function() { TRUE },

    generator = function() { TrialGroupVariable }

  ),

  public = list(

    format = function(...) {
      trial_num <- self$trial_num
      if(inherits(private$.epoch, "RAVEEpoch")) {
        e <- sprintf("  - Epoch      : %s (%s, subset=%d)",
                     private$.epoch$name,
                     private$.epoch$subject$subject_id,
                     length(trial_num))
      } else {
        e <- sprintf("  - Epoch table: nrows=%d, subset=%d",
                     nrow(private$.epoch), length(trial_num))
      }
      c(
        sprintf("<TrialGroupVariable, order=%d,n=%d> %s",
                private$.order, length(self$selected_rows), self$varname),
        sprintf("  - Name       : %s", self$name),
        e,
        sprintf("  - Conditions : unique=%d", length(self$conditions)),
        sprintf("  - Trials     : %s", dipsaus::deparse_svec(trial_num))
      )
    },

    initialize = function(epoch = NULL, name = "", id = name, order = NA, ...) {
      super$initialize(name = name, ...)

      self$order <- order

      if(identical(name, id)) {
        id <- tolower(gsub("[^a-zA-Z0-9_\\.]+", "_", id))
      }
      if(!nzchar(id)) {
        if(is.na(order)) {
          id <- asNamespace("raveio")$rand_string(4)
        } else {
          id <- as.character(private$.order)
        }
      }

      self$id <- id

      if(missing(epoch) || is.null(epoch)) {
        epoch <- data.frame(
          Block = character(0L),
          Time = numeric(0L),
          Trial = integer(0L),
          Condition = character(0L)
        )
      }

      if(inherits(epoch, "RAVEEpoch")) {
        private$.epoch <- epoch
      } else {
        epoch <- as.data.frame(epoch)
        checkmate::assert_subset(
          x = c("Block", "Time", "Trial", "Condition"),
          choices = names(epoch),
          .var.name = "names(epoch)"
        )
        private$.epoch <- epoch
      }

      self$use_constraints(
        raveio::new_constraints(
          type = "integer",
          assertions = list(
            "integer" = quote({
              checkmate::test_integer(
                x = .x,
                lower = 1,
                upper = nrow(.self$epoch),
                any.missing = FALSE,
                unique = TRUE,
                null.ok = TRUE
              )
            })
          )
        )
      )

      # initialize as empty trial set
      self$set_value(integer(0L), validate = FALSE)

    },

    store = function(...) {
      res <- super$store(...)
      res$id <- private$.id
      res$order <- private$.order
      # epoch <- private$.epoch
      # if(inherits(epoch, "RAVEEpoch")) {
      #   res$epoch <- list(
      #     type = "RAVEEpoch",
      #     subject = epoch$subject$subject_id,
      #     name = epoch$name
      #   )
      # } else {
      #   res$epoch <- list(
      #     type = "data.frame",
      #     data = epoch
      #   )
      # }
      res$epoch <- list(
        type = "data.frame",
        data = self$epoch
      )
      class(res) <- c("raveio_store.TrialGroupVariable", class(res))
      res
    },

    restore = function(x, ...) {
      stopifnot(inherits(x, "raveio_store.TrialGroupVariable"))

      if(x$epoch$type == "RAVEEpoch") {
        subject <- as_rave_subject(subject_id = x$epoch$subject, strict = FALSE)
        epoch <- subject$get_epoch(epoch_name = x$epoch$name)
      } else {
        epoch <- x$epoch$data
        checkmate::assert_subset(
          x = c("Block", "Time", "Trial", "Condition"),
          choices = names(epoch),
          .var.name = "names(epoch)"
        )
      }
      self$id <- x$id
      self$order <- x$order
      private$.epoch <- epoch

      super$restore(x, ...)
      invisible(self)
    }
  )
)

new_trial_group <- function(epoch = NULL, name = "", order = NA_integer_) {
  TrialGroupVariable$new(epoch = epoch, name = name, order = order)
}

new_analysis_range <- function(name, ...) {

  coll <- raveio::new_variable_collection(name = name)
  coll$add_variable(
    id = "event",
    var = raveio::new_constrained_variable(
      "Analysis event",
      initial_value = "",
      constraints = "string",
      na.ok = FALSE,
      null.ok = FALSE
    )
  )

  coll$add_variable(
    id = "sample_rate",
    var = raveio::new_constrained_variable(
      "Sampling frequency",
      initial_value = "",
      constraints = "number",
      na.ok = FALSE,
      null.ok = FALSE,
      lower = 0,
      finite = TRUE
    )
  )

  coll$add_variable(
    id = "filter_frequency",
    var = raveio::new_constrained_variable(
      "Filter frequency (Hz)",
      initial_value = c(NA_real_, NA_real_),
      constraints = raveio::new_constraints(
        type = "numeric",
        assertions = list(
          "numeric" = quote({
            checkmate::check_numeric(
              x = .x,
              lower = 0,
              upper = .collection[["sample_rate"]] / 2,
              finite = FALSE,
              len = 2,
              all.missing = TRUE,
              any.missing = TRUE,
              sorted = TRUE, null.ok = FALSE
            )
          })
        )
      )
    )
  )

  coll$add_variable(
    id = "max_time_range",
    var = raveio::new_constrained_variable(
      "Analysis time range", initial_value = c(0, 1),
      constraints = "numeric",
      finite = TRUE, any.missing = FALSE,
      all.missing = FALSE, len = 2,
      unique = FALSE, sorted = TRUE,
      null.ok = FALSE
    )
  )

  coll$add_variable(
    id = "time_range",
    var = raveio::new_constrained_variable(
      "Analysis time range", initial_value = c(0, 1),
      constraints = "numeric",
      finite = TRUE, any.missing = FALSE,
      all.missing = FALSE, len = 2,
      unique = FALSE, sorted = TRUE,
      null.ok = FALSE
    )
  )

  coll$add_variable(
    id = "time_shift_range",
    var = raveio::new_constrained_variable(
      "Event shift relative to onset", initial_value = c(0, 0),
      constraints = "numeric",
      finite = TRUE, any.missing = FALSE,
      all.missing = FALSE, len = 2,
      unique = FALSE, sorted = TRUE,
      null.ok = FALSE
    )
  )

  coll$add_variable(
    id = "max_analysis_time_range",
    var = raveio::new_constrained_binding(
      "Max available range for analysis",
      expr = quote({
        time_shift_range <- .collection[["time_shift_range"]]
        max_analysis_time_range <- .collection[["max_time_range"]]
        max_analysis_time_range[[2]] <- max_analysis_time_range[[2]] - time_shift_range[[2]]
        max_analysis_time_range[[1]] <- max_analysis_time_range[[1]] - time_shift_range[[1]]
        max_analysis_time_range
      }),
      quoted = TRUE
    )
  )

  coll$use_constraints(raveio::new_constraints(
    type = "time_range %within% max_analysis_time_range",
    assertions = list(
      "ValidTimeRange" = quote({
        time_range <- .x$time_range
        max_analysis_time_range <- .x$max_analysis_time_range
        if(
          time_range[[1]] < max_analysis_time_range[[1]] ||
          time_range[[2]] > max_analysis_time_range[[2]]
        ) {
          return(sprintf(
            "Invalid analysis time range. Max available with current event: [%.2f, %.2f]",
            max_analysis_time_range[[1]], max_analysis_time_range[[2]]
          ))
        }
        TRUE
      })
    )
  ))

  coll$add_variable("isAnalysisRange", TRUE)

  coll

}

new_param_grid <- function(name = "Parameter grid (cond x time)") {

  container <- raveio::new_variable_collection(name = name)

  container$add_variable("isVoltExplParamGrid", TRUE)

  container$add_variable(
    id = "condition_groups",
    var = raveio::new_constrained_variable(
      name = "List of condition group",
      initial_value = list(),
      constraints = "list",
      quote({
        if(!is.list(.x)) { return("`condition_groups` must be a list") }
        has_trials <- FALSE
        for(item in .x) {
          if(!isTRUE(item$isTrialGroupVariable)) {
            return("`condition_groups` elements must be instances of `TrialGroupVariable`")
          }
          item$validate()
          if( !has_trials && item$has_trials ) {
            has_trials <- TRUE
          }
        }
        if(!has_trials) {
          return("No valid condition group specified. Please check the inputs for trial conditions.")
        }
      })
    )
  )

  container$add_variable(
    id = "analysis_settings",
    var = raveio::new_constrained_variable(
      name = "List of analysis setting",
      initial_value = list(),
      constraints = "list",
      quote({
        if(!is.list(.x)) { return("`analysis_settings` must be a list") }
        for(item in .x) {
          if(!isTRUE(item$isAnalysisRange)) {
            return("`analysis_settings` elements must be instances of created from `new_analysis_range`")
          }
          item$validate()
        }
        return(TRUE)
      })
    )
  )

  container$use_constraints(raveio::new_constraints(
    type = "Valid event",
    assertions = list(
      "EventChecker" = quote({
        available_events <- c("", .x$condition_groups[[1]]$available_events)
        missing_events <- lapply(.x$analysis_settings, function(range) {
          if(range$event %in% available_events) { return(NULL) }
          return(range$event)
        })
        missing_events <- unlist(missing_events)
        if(!length(missing_events)) { return(TRUE) }
        sprintf("The following events are unavailable: [%s]", paste(missing_events, collapse = ", "))
      })
    )
  ))
  container
}
