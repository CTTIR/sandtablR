# Simulation engine ----------------------------------------------------------
# Maps a scenario onto a simmer discrete-event model. Each casualty is an
# entity carrying its card as simmer attributes, routed through board-area
# resources by sighting category:
#
#   green   ward                                              (terminal)
#   red     ed_red    -> diagnostics -> or -> icu_imc         (terminal)
#                                          -> recovery -> ward
#                                    -> held in ed_red        (terminal)
#   yellow  ed_yellow -> diagnostics -> ... as above, different probabilities
#
# Three dispositions are terminal by design, and that is what makes the
# end-of-game distribution match the published one: critical care is where a
# casualty stays, a ward bed is the end of the line, and a casualty who is
# worked up but not taken to theatre is still being held in the emergency
# department when the game is called. Routing them onward to a ward instead
# leaves roughly seven casualties in ten there, against a published third.
#
# Probabilities and service times come from sdt_reference("defaults"); the
# horizon and board capacities were calibrated against the published
# end_state and are recorded there under $calibration.
#
# The engine's real output is not the summary but the per-area event log,
# built from the monitored "area" attribute rather than from release
# timestamps: simmer only writes a per-resource record when a casualty
# releases a resource, so anyone still being treated at the horizon would
# otherwise be attributed to the area they had already left.

new_game_run <- function(arrivals = tibble::tibble(),
                         events = tibble::tibble(),
                         resources = tibble::tibble(),
                         scenario = NULL,
                         meta = list()) {
  structure(
    list(arrivals = arrivals, events = events, resources = resources,
         scenario = scenario, meta = meta),
    class = "game_run"
  )
}

# Attributes have to be numeric, so the vocabularies are encoded by position
# and decoded on the way out.
.code_of <- function(x, levels) match(x, levels)
.decode <- function(x, levels) levels[x]

#' Run a mass-casualty patient-flow simulation
#'
#' Executes a discrete-event simulation of a scenario with \pkg{simmer},
#' routing each casualty through board-area resources according to its
#' sighting category and therapy concept, and returns per-area event records,
#' per-casualty flow records and resource utilisation.
#'
#' Theatre time scales with the therapy concept: abbreviated surgical
#' care is quickest, damage-control surgery the reference, early total care the
#' longest. The multipliers live in `sdt_reference("defaults")$or_multiplier`.
#'
#' @param scenario A `scenario` object from [sdt_scenario()].
#' @param replications Number of independent replications (positive whole
#'   number). The replication index is carried in every returned table.
#' @param service_times Named list of mean service times in minutes per board
#'   area, used as exponential rates. Missing areas fall back to the reference
#'   defaults.
#' @param horizon Length of the game in minutes. Casualties still in the system
#'   when the clock stops are recorded where they stood, which is what makes
#'   the end-of-game distribution meaningful. `Inf` runs every casualty to
#'   completion.
#' @param parallel If `TRUE`, run replications concurrently. Defaults to
#'   `FALSE`. The backend is set up for the duration of the call and the
#'   caller's plan is restored on exit.
#' @param workers Number of parallel workers, or `NULL` to choose one: a core
#'   short of the machine, capped at 8.
#' @param seed Optional integer seed, applied with [withr::local_seed()].
#' @param verbose If `TRUE`, report progress via \pkg{cli}.
#'
#' @return An S3 object of class `game_run` with elements:
#'   \describe{
#'     \item{`events`}{One row per casualty per board area entered:
#'       `replication`, `id`, `step`, `area`, `start`, `end`, `duration`,
#'       `service_time`. A state runs from entering an area to entering the
#'       next, and the last runs to the horizon, so `duration` is time spent
#'       standing in that area. `service_time` is the treatment time simmer
#'       actually recorded, and is `NA` where the casualty was still being
#'       treated at the horizon or was only being held. The two differ
#'       whenever a casualty is finished but waiting for a bed downstream.}
#'     \item{`arrivals`}{One row per casualty per replication, carrying the
#'       card fields and total time in system.}
#'     \item{`resources`}{Resource utilisation over time.}
#'   }
#' @seealso [summary.game_run()] for the end-of-game distribution, and
#'   [sdt_decision_paths()] to turn the event log into state sequences.
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 20, seed = 1)
#' sc <- sdt_scenario(co, resources = list(or = 2, icu_imc = 6))
#' run <- sdt_simulate(sc, replications = 2, seed = 1, verbose = FALSE)
#' run
#' head(run$events)
sdt_simulate <- function(scenario,
                         replications = 1L,
                         service_times = list(),
                         horizon = 270,
                         parallel = FALSE,
                         workers = NULL,
                         seed = NULL,
                         verbose = TRUE) {
  .check_class(scenario, "scenario")
  replications <- .check_count(replications)
  .check_flag(parallel)
  .check_flag(verbose)
  if (!is.numeric(horizon) || length(horizon) != 1L || is.na(horizon) ||
        horizon <= 0) {
    cli::cli_abort("{.arg horizon} must be a single positive number.")
  }
  if (!is.list(service_times)) {
    cli::cli_abort("{.arg service_times} must be a named list.")
  }
  unknown <- setdiff(names(service_times), sdt_board_areas())
  if (length(unknown)) {
    cli::cli_abort(c(
      "{.arg service_times} names must be board areas.",
      "x" = "Unknown: {.val {unknown}}."
    ))
  }
  if (!is.null(seed)) {
    withr::local_seed(.check_count(seed))
  }

  st <- utils::modifyList(as.list(.ref_default("service_times")), service_times)
  run_one <- function(r) .simulate_once(scenario, st, horizon, rep_id = r)

  if (parallel && !requireNamespace("future.apply", quietly = TRUE)) {
    cli::cli_warn(c(
      "!" = "Package {.pkg future.apply} is not installed; running serially.",
      "i" = "Install it with {.code install.packages(\"future.apply\")}."
    ))
    parallel <- FALSE
  }
  # Without this the branch below dispatched through future.apply under the
  # default sequential plan and ran no faster than the serial path.
  parallel <- .sdt_parallel_plan(parallel, workers)

  reps <- if (parallel) {
    future.apply::future_lapply(
      seq_len(replications), run_one, future.seed = TRUE
    )
  } else {
    lapply(seq_len(replications), function(r) {
      if (verbose) cli::cli_inform("Replication {r}/{replications}")
      run_one(r)
    })
  }

  new_game_run(
    arrivals = dplyr::bind_rows(lapply(reps, `[[`, "arrivals")),
    events = dplyr::bind_rows(lapply(reps, `[[`, "events")),
    resources = dplyr::bind_rows(lapply(reps, `[[`, "resources")),
    scenario = scenario,
    meta = list(
      replications = replications, service_times = st,
      horizon = horizon, seed = seed, parallel = parallel
    )
  )
}

# One replication.
.simulate_once <- function(scenario, st, horizon, rep_id) {
  co <- scenario$cohort
  n <- nrow(co)
  if (!n) {
    return(list(
      arrivals = .empty_arrivals(co), events = .empty_events(),
      resources = .empty_resources()
    ))
  }

  env <- simmer::simmer("sandtablR", verbose = FALSE)
  for (a in sdt_board_areas()) {
    env <- simmer::add_resource(
      env, a,
      capacity = scenario$resources[[a]] %||% 1L
    )
  }

  traj <- .casualty_trajectory(env, co, st)
  env <- simmer::add_generator(
    env, "cas", traj, simmer::at(.arrival_times(n, horizon)), mon = 2
  )
  env <- simmer::run(env, until = horizon)

  .harvest(env, co, rep_id, horizon)
}

# When each casualty reaches the board.
#
# The exercise does not feed casualties in one at a time: a game leader plays
# the patient cards in tranches at fixed points in the game. Modelling that,
# rather than a smooth stream, is what produces a screening area that is still
# occupied when the clock stops -- the last tranche has not been worked through
# yet. A smooth stream empties the screening area long before the horizon and
# cannot reproduce the published share.
#
# Within a tranche the order is the cohort's arrival order, spread over a short
# window so the casualties queue rather than arriving at one instant.
#
# The tranche schedule is a fraction of the game length, so it needs a game
# length to be a fraction of. Callers who ask for an unbounded horizon -- which
# is how you ask "let every casualty finish, however long it takes" -- get the
# smooth exponential stream instead; scaling a fraction by Inf would otherwise
# make every arrival time NaN and the run silently empty.
.arrival_times <- function(n, horizon) {
  arr <- .ref_default("arrivals")
  if (!is.finite(horizon)) {
    return(cumsum(stats::rexp(n, 1 / .ref_default("interarrival"))))
  }
  starts <- arr$tranche_starts * horizon
  # Split the cohort evenly across the tranches by arrival rank. Computed
  # directly rather than with cut(), which needs unique breaks and so fails on
  # any cohort smaller than the number of tranches -- a three-casualty test
  # cohort, for instance.
  tranche <- 1L + floor((seq_len(n) - 1L) * length(starts) / n)
  within <- stats::runif(n, 0, arr$tranche_spread * horizon)
  sort(starts[tranche] + within)
}

# Build the routing trajectory. `env` is captured by the closures so they can
# read the current arrival's attributes at run time.
.casualty_trajectory <- function(env, co, st) {
  triage_code <- .code_of(co$triage, sdt_triage_levels())
  therapy_code <- .code_of(co$therapy, sdt_therapy_levels())
  life <- as.numeric(co$life_points)
  vent <- as.numeric(co$ventilated)

  or_mult <- .ref_default("or_multiplier")
  p_or <- .ref_default("p_or")
  p_icu <- .ref_default("p_icu")
  diag_p <- .ref_default("diagnostics")

  # simmer names arrivals "<prefix><n>" from zero, so this recovers the
  # casualty's row exactly rather than relying on generation order.
  idx <- function() {
    as.integer(sub("^cas", "", simmer::get_name(env))) + 1L
  }
  triage_now <- function() {
    .decode(simmer::get_attribute(env, "triage_code"), sdt_triage_levels())
  }
  # Every area records its own entry as a monitored attribute the moment
  # service starts. That log, not the release log, is what says where a
  # casualty stood when the clock stopped: simmer only writes a per-resource
  # record on release, so anyone still being treated at the horizon would
  # otherwise be attributed to the previous area.
  enter <- function(trj, a) {
    simmer::set_attribute(trj, "area", .code_of(a, sdt_board_areas()))
  }
  area <- function(a, mult = 1) {
    simmer::trajectory(a) |>
      simmer::seize(a, 1L) |>
      enter(a) |>
      simmer::timeout(function() stats::rexp(1L, 1 / (st[[a]] * mult))) |>
      simmer::release(a, 1L)
  }

  # Diagnostics: the time scales with how many of the card's three modalities
  # this casualty gets, which is what makes a red workup longer than a green
  # one. Drawing each independently, rather than sampling one, is what lets the
  # realised modality mix be compared against the published one.
  modalities <- sdt_diagnostics()
  diagnostics <- simmer::trajectory("diagnostics") |>
    simmer::seize("diagnostics", 1L) |>
    enter("diagnostics") |>
    simmer::timeout(function() {
      tri <- triage_now()
      p <- vapply(modalities, function(m) diag_p[[m]][[tri]], numeric(1))
      k <- sum(stats::runif(length(p)) < p)
      stats::rexp(1L, 1 / (st$diagnostics * max(0.5, k)))
    }) |>
    simmer::release("diagnostics", 1L)

  # Theatre, with service time set by the therapy concept, then a disposition.
  # Both arms are terminal: critical care is where a casualty stays, and the
  # step-down arm ends in a ward bed. Nothing follows theatre.
  theatre <- simmer::trajectory("or") |>
    simmer::seize("or", 1L) |>
    enter("or") |>
    simmer::timeout(function() {
      th <- .decode(simmer::get_attribute(env, "therapy_code"),
                    sdt_therapy_levels())
      stats::rexp(1L, 1 / (st$or * or_mult[[th]]))
    }) |>
    simmer::release("or", 1L) |>
    simmer::branch(
      option = function() {
        if (stats::runif(1L) < p_icu[[triage_now()]]) 1L else 2L
      },
      continue = c(FALSE, FALSE),
      area("icu_imc"),
      simmer::join(area("recovery"), area("ward"))
    )

  # A casualty who is worked up but not taken to theatre is observed in the
  # emergency department and then admitted: to critical care if the workup
  # found enough, otherwise to a ward bed. The board's disposition field offers
  # exactly this choice, so parking them in the department for the rest of the
  # game would be a modelling shortcut rather than a reading of the exercise.
  #
  # The observation is a bare timeout, not a seize. They are waiting on a
  # decision, not occupying a treatment bay, and holding a server here would
  # deadlock the casualties queueing behind them. Whoever is still inside that
  # timeout when the clock stops is what the published emergency-department
  # share counts.
  observe <- .ref_default("observation")
  p_imc <- .ref_default("p_icu_conservative")
  hold <- function(a, level) {
    simmer::trajectory(paste0("hold_", a)) |>
      simmer::set_attribute("area", .code_of(a, sdt_board_areas())) |>
      simmer::timeout(function() stats::rexp(1L, 1 / observe[[level]])) |>
      simmer::branch(
        option = function() {
          if (stats::runif(1L) < p_imc[[level]]) 1L else 2L
        },
        continue = c(FALSE, FALSE),
        area("icu_imc"),
        area("ward")
      )
  }

  # A casualty whose card says "no operation" never reaches theatre, however
  # the routing probability falls: the concept is a decision that has already
  # been taken, not a coin waiting to be flipped.
  operable <- function() {
    .decode(simmer::get_attribute(env, "therapy_code"),
            sdt_therapy_levels()) != "none"
  }

  pathway <- function(ed_area, level) {
    simmer::join(
      area(ed_area),
      diagnostics,
      simmer::branch(
        simmer::trajectory(),
        option = function() {
          if (operable() && stats::runif(1L) < p_or[[level]]) 1L else 2L
        },
        continue = c(FALSE, FALSE),
        theatre,
        hold(ed_area, level)
      )
    )
  }

  red <- pathway("ed_red", "red")
  yellow <- pathway("ed_yellow", "yellow")
  # Green casualties are cared for in the board's green treatment area, which
  # is where they stand at the end of the game.
  green <- area("green_area")

  simmer::trajectory("casualty") |>
    simmer::set_attribute(
      keys = c("triage_code", "therapy_code", "life_points", "ventilated"),
      values = function() {
        i <- idx()
        c(triage_code[[i]], therapy_code[[i]], life[[i]], vent[[i]])
      }
    ) |>
    # The screening area is entered on arrival, before the seize, not after
    # it. A card that has been played but not yet worked through is standing
    # at screening -- that is what the area is -- and recording it only once a
    # screening slot frees up would file every queueing casualty under "not yet
    # seen", which the source uses for cards the game ended before playing.
    # It also makes the recorded time at screening include the wait, which is
    # the quantity a congested board is supposed to show.
    enter("triage") |>
    simmer::seize("triage", 1L) |>
    simmer::timeout(function() stats::rexp(1L, 1 / st$triage)) |>
    simmer::release("triage", 1L) |>
    simmer::branch(
      option = function() simmer::get_attribute(env, "triage_code"),
      continue = c(TRUE, TRUE, TRUE),
      red, yellow, green
    )
}

# Pull the monitors out and put casualty identifiers back on them.
.harvest <- function(env, co, rep_id, horizon) {
  attrs <- tibble::as_tibble(simmer::get_mon_attributes(env))
  whole <- tibble::as_tibble(simmer::get_mon_arrivals(env))
  res <- tibble::as_tibble(simmer::get_mon_resources(env))

  # A casualty who completed its pathway leaves at its arrival end time; one
  # still in the system when the clock stops is held to the horizon. Using the
  # horizon for both would report an infinite last state whenever the caller
  # asks for horizon = Inf.
  departure <- if (nrow(whole)) {
    stats::setNames(as.numeric(whole$end_time), .id_for(whole$name, co))
  } else {
    stats::setNames(numeric(), character())
  }

  # True service time per area, from simmer's per-resource monitor. This is
  # distinct from how long the casualty stood in the area: a casualty that has
  # finished in theatre but is queuing for a critical-care bed is still in the
  # "or" state, and conflating the two makes service times look like whatever
  # the downstream bottleneck happens to be.
  per_area <- tibble::as_tibble(
    simmer::get_mon_arrivals(env, per_resource = TRUE)
  )
  service <- if (nrow(per_area)) {
    tibble::tibble(
      id = .id_for(per_area$name, co),
      area = as.character(per_area$resource),
      start = as.numeric(per_area$start_time),
      service_time = as.numeric(per_area$activity_time)
    )
  } else {
    NULL
  }

  events <- .events_from_attributes(
    attrs, co, rep_id, horizon, departure, service
  )

  arrivals <- if (nrow(whole)) {
    tibble::tibble(
      replication = rep_id,
      id = .id_for(whole$name, co),
      start = as.numeric(whole$start_time),
      end = as.numeric(whole$end_time),
      activity_time = as.numeric(whole$activity_time),
      finished = as.logical(whole$finished)
    )
  } else {
    .empty_arrivals(co)
  }
  # Carry the card through, so downstream code never has to join it back.
  arrivals <- dplyr::left_join(arrivals, tibble::as_tibble(co), by = "id")

  resources <- if (nrow(res)) {
    tibble::tibble(
      replication = rep_id,
      resource = as.character(res$resource),
      time = as.numeric(res$time),
      server = as.numeric(res$server),
      queue = as.numeric(res$queue),
      capacity = as.numeric(res$capacity)
    )
  } else {
    .empty_resources()
  }

  list(arrivals = arrivals, events = events, resources = resources)
}

# Turn the monitored "area" attribute into one row per casualty per state.
#
# A state runs from the moment the casualty enters an area until it enters the
# next one, and the final state runs to the horizon. Defining it this way, and
# not from release timestamps, is what keeps a casualty still being treated at
# the horizon attributed to the area it is actually in.
.events_from_attributes <- function(attrs, co, rep_id, horizon, departure,
                                    service = NULL) {
  if (!nrow(attrs)) {
    return(.empty_events())
  }
  attrs <- attrs[attrs$key == "area", , drop = FALSE]
  if (!nrow(attrs)) {
    return(.empty_events())
  }

  out <- tibble::tibble(
    replication = rep_id,
    id = .id_for(attrs$name, co),
    area = .decode(as.integer(attrs$value), sdt_board_areas()),
    start = as.numeric(attrs$time)
  )
  out <- out[!is.na(out$id), , drop = FALSE]
  out <- dplyr::arrange(out, .data$id, .data$start)

  # Where the pathway ends for each casualty: its departure time if it got
  # there, otherwise the horizon.
  last_end <- unname(departure[out$id])
  last_end[is.na(last_end)] <- horizon

  grouped <- dplyr::group_by(out, .data$id)
  filled <- dplyr::mutate(
    grouped,
    end = dplyr::lead(.data$start),
    step = seq_len(dplyr::n())
  )
  out <- dplyr::ungroup(filled)
  out$end[is.na(out$end)] <- last_end[is.na(out$end)]
  out$end <- pmax(out$start, pmin(out$end, horizon))
  out$duration <- out$end - out$start
  out$service_time <- .service_for(out, service)
  out[, c("replication", "id", "step", "area", "start", "end",
          "duration", "service_time")]
}

# Match each state to its resource record. A casualty can enter the same area
# twice -- treatment in the emergency department, then being held there after
# the decision not to operate -- and the hold has no resource record at all,
# so the k-th entry to an area is matched to the k-th record for it and any
# surplus entries get NA.
.service_for <- function(events, service) {
  if (is.null(service) || !nrow(service)) {
    return(rep(NA_real_, nrow(events)))
  }
  occurrence <- function(id, area) {
    stats::ave(seq_along(id), paste(id, area), FUN = seq_along)
  }
  service <- dplyr::arrange(service, .data$id, .data$area, .data$start)
  service$occ <- occurrence(service$id, service$area)
  key <- paste(service$id, service$area, service$occ)
  lookup <- stats::setNames(service$service_time, key)

  want <- paste(events$id, events$area, occurrence(events$id, events$area))
  unname(lookup[want])
}

# simmer names arrivals "<prefix><n>" from zero; map that back to the cohort.
.id_for <- function(name, co) {
  i <- suppressWarnings(as.integer(sub("^cas", "", name))) + 1L
  out <- rep(NA_character_, length(i))
  ok <- !is.na(i) & i >= 1L & i <= nrow(co)
  out[ok] <- co$id[i[ok]]
  out
}

.empty_events <- function() {
  tibble::tibble(
    replication = integer(), id = character(), step = integer(),
    area = character(), start = numeric(), end = numeric(),
    duration = numeric(), service_time = numeric()
  )
}

.empty_arrivals <- function(co = NULL) {
  out <- tibble::tibble(
    replication = integer(), id = character(), start = numeric(),
    end = numeric(), activity_time = numeric(), finished = logical()
  )
  if (is.null(co)) {
    return(out)
  }
  dplyr::left_join(out, tibble::as_tibble(co), by = "id")
}

.empty_resources <- function() {
  tibble::tibble(
    replication = integer(), resource = character(), time = numeric(),
    server = numeric(), queue = numeric(), capacity = numeric()
  )
}

#' @export
format.game_run <- function(x, ...) {
  rlang::check_dots_empty()
  n_rep <- x$meta$replications
  n_cas <- nrow(x$arrivals)
  n_ev <- nrow(x$events)
  c(
    sprintf(
      "<game_run> %d replication%s over %s minutes",
      n_rep, if (identical(n_rep, 1L)) "" else "s", format(x$meta$horizon)
    ),
    sprintf(
      "%d casualty record%s, %d area event%s",
      n_cas, if (n_cas == 1L) "" else "s",
      n_ev, if (n_ev == 1L) "" else "s"
    )
  )
}

#' @export
print.game_run <- function(x, ...) {
  rlang::check_dots_empty()
  cat(format(x), sep = "\n")
  invisible(x)
}

#' End-of-game location distribution
#'
#' Reports where the casualties stood when the clock stopped, which is the
#' quantity the published reference distribution describes. A casualty's end
#' state is the last board area it had entered by the horizon; one that never
#' cleared triage counts as `"not_seen"`.
#'
#' Two granularities are available. By default the tibble has one row per board
#' area. With `reference = TRUE` it is folded onto the eight locations the
#' source reports, because those are the only ones a published proportion
#' exists for: the two emergency-department areas become one, and the green
#' treatment area joins the ward. Both pairings are recorded in
#' `sdt_reference("defaults")$end_state_map` rather than being applied here, so
#' they can be inspected and argued with.
#'
#' @param object A `game_run` from [sdt_simulate()].
#' @param reference If `TRUE`, fold the areas onto the published reporting
#'   locations and add the reference proportion beside the observed one, with
#'   their difference.
#' @param version Which evaluated version of the exercise to compare against,
#'   `"2.0"` or `"1.0"`. Only used when `reference = TRUE`.
#' @param ... These dots are for future extensions and must be empty.
#'
#' @return A [tibble::tibble()] with columns `area`, `label`, `n` and
#'   `proportion`; when `reference = TRUE`, also `reference` and `difference`.
#'   Every area is present even at a zero count, so the shape does not depend
#'   on the run.
#' @seealso [sdt_reference()] for the published values and their provenance.
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 40, seed = 1)
#' run <- sdt_simulate(sdt_scenario(co), replications = 2, seed = 1,
#'                     verbose = FALSE)
#' summary(run)
#' summary(run, reference = TRUE)
#' summary(run, reference = TRUE, version = "1.0")
summary.game_run <- function(object, reference = FALSE,
                             version = c("2.0", "1.0"), ...) {
  rlang::check_dots_empty()
  .check_flag(reference)
  version <- rlang::arg_match(version)
  end <- .end_states(object)

  levels <- if (reference) .sdt_end_state_areas() else sdt_board_areas()
  areas <- if (reference) .sdt_report_area(end$area) else end$area
  counts <- table(factor(areas, levels = levels))
  total <- sum(counts)
  share <- if (total) {
    as.numeric(counts) / total
  } else {
    rep(0, length(counts))
  }
  out <- tibble::tibble(
    area = names(counts),
    label = .sdt_label_area(names(counts)),
    n = as.integer(counts),
    proportion = share
  )
  if (!reference) {
    return(out)
  }

  ref <- .ref_published("end_state", version = version)
  out$reference <- unname(ref[out$area])
  out$difference <- out$proportion - out$reference
  out
}

# The last area each casualty had entered by the end of the run.
#
# Every casualty in every replication appears, including the ones that generated
# no events at all because the game was called before their card was played.
# Those are exactly what the source's "not yet seen" counts, and dropping them
# would not only lose that category but shrink the denominator, quietly
# inflating every other share.
#
# A casualty whose card *was* played but who is still queueing for a screening
# slot is not one of these: the trajectory records the screening area on
# arrival, so they already carry an event.
.end_states <- function(run) {
  co <- tibble::as_tibble(run$scenario$cohort)
  reps <- seq_len(run$meta$replications %||% 1L)
  if (!nrow(co)) {
    return(tibble::tibble(
      replication = integer(), id = character(), area = character()
    ))
  }
  full <- tibble::tibble(
    replication = rep(reps, each = nrow(co)),
    id = rep(co$id, times = length(reps))
  )

  ev <- run$events
  if (!nrow(ev)) {
    full$area <- "not_seen"
    return(full)
  }
  .check_areas(ev$area, arg = "run$events$area")
  # Break the tie on `step`, not on row order. A simulated run draws continuous
  # start times and never ties, so this is invisible there -- but an observed
  # log records many moves inside one round, and `with_ties = FALSE` was then
  # returning whichever row happened to come first. On 19 real course exports
  # that mis-assigned 351 of 6,048 end states, and reversing the row order of
  # the same data changed the answer.
  grouped <- dplyr::group_by(ev, .data$replication, .data$id)
  latest <- dplyr::slice_max(
    grouped, dplyr::pick("start", "step"), n = 1L, with_ties = FALSE
  )
  latest <- dplyr::select(dplyr::ungroup(latest), "replication", "id", "area")

  out <- dplyr::left_join(full, latest, by = c("replication", "id"))
  out$area[is.na(out$area)] <- "not_seen"
  out
}
