# Decision-path analysis ------------------------------------------------------
# A casualty's decision path is the ordered sequence of board areas it passed
# through. The simulation engine records that directly, so everything here is
# a view over run$events rather than a reconstruction.
#
# Two things are worth separating. A *state sequence* is the ordered list of
# areas, which is what sequence analysis operates on. A *consistency* score
# asks how far apart the same casualty's paths are across replications, which
# is a distance question, not an agreement-on-one-label question. The earlier
# modal-area measure answered neither: with one recorded area per casualty it
# was always exactly 1.

#' Build decision paths from a game run
#'
#' Extracts, for each casualty in each replication, the ordered sequence of
#' board areas visited.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param format `"long"` for one row per casualty per step, or `"wide"` for
#'   one row per casualty per replication with a column per position. The wide
#'   form is the state-sequence (STS) layout [sdt_seqdef()] expects.
#'
#' Every casualty appears in every replication. One the game was called before
#' reaching gets a single-step path of `"not_seen"` rather than being left out:
#' never being screened is a pathway the exercise produces and the source
#' reports, and a sequence analysis that silently dropped those casualties
#' would be computed on a different denominator than the one it claims.
#'
#' @return A [tibble::tibble()]. Long: `replication`, `id`, `step`, `area`,
#'   `start`, `end`, `duration`. Wide: `replication`, `id`, then `step_1` to
#'   `step_k`, padded with `NA` for casualties whose path was shorter.
#' @seealso [sdt_seqdef()], [sdt_decision_consistency()],
#'   [sdt_plot_sequences()].
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 10, seed = 1)
#' run <- sdt_simulate(sdt_scenario(co), replications = 2, seed = 1,
#'                     verbose = FALSE)
#' sdt_decision_paths(run)
#' sdt_decision_paths(run, format = "wide")
sdt_decision_paths <- function(run, format = c("long", "wide")) {
  .check_class(run, "game_run")
  format <- rlang::arg_match(format)

  long <- run$events[, c("replication", "id", "step", "area", "start", "end",
                         "duration")]
  .check_areas(long$area, arg = "run$events$area")
  long <- dplyr::bind_rows(long, .unseen_steps(run, long))
  long <- dplyr::arrange(long, .data$replication, .data$id, .data$step)
  if (identical(format, "long")) {
    return(long)
  }

  if (!nrow(long)) {
    return(tibble::tibble(replication = integer(), id = character()))
  }
  width <- max(long$step)
  keys <- unique(long[, c("replication", "id")])
  cells <- t(vapply(seq_len(nrow(keys)), function(i) {
    hit <- long$replication == keys$replication[[i]] & long$id == keys$id[[i]]
    path <- long$area[hit][order(long$step[hit])]
    length(path) <- width
    path
  }, character(width)))
  colnames(cells) <- paste0("step_", seq_len(width))
  dplyr::bind_cols(keys, tibble::as_tibble(cells))
}

# One "not_seen" step for every casualty-replication pair the event log has no
# record of. Times are the horizon: that is when the observation was made, and
# leaving them NA would put holes in a column the plots treat as numeric.
.unseen_steps <- function(run, long) {
  co <- tibble::as_tibble(run$scenario$cohort)
  reps <- seq_len(run$meta$replications %||% 1L)
  if (!nrow(co)) {
    return(long[0L, , drop = FALSE])
  }
  want <- tibble::tibble(
    replication = rep(reps, each = nrow(co)),
    id = rep(co$id, times = length(reps))
  )
  seen <- paste(long$replication, long$id)
  missing <- want[!paste(want$replication, want$id) %in% seen, , drop = FALSE]
  if (!nrow(missing)) {
    return(long[0L, , drop = FALSE])
  }
  horizon <- run$meta$horizon %||% NA_real_
  missing$step <- 1L
  missing$area <- "not_seen"
  missing$start <- horizon
  missing$end <- horizon
  missing$duration <- 0
  missing[, names(long), drop = FALSE]
}

# One printable character per board area, so a pathway becomes a string an
# edit distance can be taken over.
#
# LETTERS was the obvious encoding and it is a trap: it silently yields NA
# beyond 26 states, and NA is exactly what `paste()` renders as a two-character
# literal. Printable ASCII from "!" (33) upward gives 94 codes and asserts its
# own ceiling instead of overflowing. Verified to produce distances identical
# to the letter coding over the shared range.
.area_codes <- function(area, areas = sdt_board_areas(),
                        call = rlang::caller_env()) {
  n_areas <- length(areas)
  if (n_areas > 94L) {
    cli::cli_abort(
      c("The board vocabulary is too large to encode as single characters.",
        "x" = "{n_areas} areas; the limit is 94."),
      call = call
    )
  }
  .check_areas(area, areas = areas, call = call)
  intToUtf8(32L + match(area, areas), multiple = TRUE)
}

# One string per casualty-replication, one character per state. Used by the
# distance measures and by the base-R fallback for sequence comparison.
.path_strings <- function(run) {
  long <- sdt_decision_paths(run, format = "long")
  if (!nrow(long)) {
    return(tibble::tibble(replication = integer(), id = character(),
                          path = character()))
  }
  long$code <- .area_codes(long$area)
  grouped <- dplyr::group_by(long, .data$replication, .data$id)
  dplyr::summarise(
    grouped,
    path = paste(.data$code[order(.data$step)], collapse = ""),
    .groups = "drop"
  )
}

#' Score decision consistency
#'
#' Quantifies how consistently the same casualty is routed across
#' replications. Two measures are offered, both returning a score in `[0, 1]`
#' where 1 is perfectly consistent.
#'
#' `"om"` compares each casualty's paths with one another by optimal matching:
#' the mean pairwise edit distance between its replication paths, normalised
#' by the longer of each pair. This is the measure that answers "does this
#' casualty get handled the same way twice?".
#'
#' `"entropy"` scores each path on its own, as the normalised Shannon entropy
#' of the time it spent in each area. A casualty that sat in one area all game
#' scores 1; one moved through many areas scores lower. That describes the
#' shape of a single trajectory rather than agreement between trajectories, so
#' it is meaningful even with a single replication.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param method `"om"` (default) or `"entropy"`.
#'
#' @return A [tibble::tibble()] with one row per casualty: `id`, `n_paths`
#'   (how many replication paths contributed), `n_distinct` (how many of them
#'   differ), and `consistency`. With `method = "om"` and a single
#'   replication there is nothing to compare, so `consistency` is `NA`.
#' @seealso [sdt_decision_paths()], [sdt_seq_cluster()].
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 12, seed = 1)
#' run <- sdt_simulate(sdt_scenario(co), replications = 4, seed = 1,
#'                     verbose = FALSE)
#' sdt_decision_consistency(run)
#' sdt_decision_consistency(run, method = "entropy")
sdt_decision_consistency <- function(run, method = c("om", "entropy")) {
  .check_class(run, "game_run")
  method <- rlang::arg_match(method)
  empty <- tibble::tibble(
    id = character(), n_paths = integer(),
    n_distinct = integer(), consistency = numeric()
  )

  if (identical(method, "entropy")) {
    return(.consistency_entropy(run, empty))
  }
  paths <- .path_strings(run)
  if (!nrow(paths)) {
    return(empty)
  }
  split_paths <- split(paths$path, paths$id)
  out <- lapply(names(split_paths), function(id) {
    p <- split_paths[[id]]
    tibble::tibble(
      id = id,
      n_paths = length(p),
      n_distinct = length(unique(p)),
      consistency = 1 - .mean_pairwise_distance(p)
    )
  })
  dplyr::arrange(dplyr::bind_rows(out), .data$id)
}

# Mean pairwise normalised edit distance. utils::adist() gives Levenshtein
# distance with unit costs, which is optimal matching with an indel cost of 1
# and a constant substitution cost -- the conventional default when no
# substitution matrix is justified. Returns NA for a single path, because a
# distance to nothing is not a distance of zero.
.mean_pairwise_distance <- function(paths) {
  n <- length(paths)
  if (n < 2L) {
    return(NA_real_)
  }
  d <- utils::adist(paths)
  len <- nchar(paths)
  denom <- outer(len, len, pmax)
  denom[denom == 0] <- 1
  mean((d / denom)[upper.tri(d)])
}

# Normalised Shannon entropy of the time spent per area, averaged over a
# casualty's replications. Normalising by the log of the alphabet size, rather
# than of the number of areas actually visited, keeps paths of different
# lengths on one scale.
.consistency_entropy <- function(run, empty) {
  long <- sdt_decision_paths(run, format = "long")
  if (!nrow(long)) {
    return(empty)
  }
  n_areas <- length(sdt_board_areas())
  per_path <- long |>
    dplyr::group_by(.data$replication, .data$id, .data$area) |>
    dplyr::summarise(time = sum(.data$duration), .groups = "drop_last") |>
    dplyr::summarise(
      entropy = .shannon(.data$time) / log(n_areas),
      .groups = "drop"
    )
  per_path <- dplyr::left_join(
    per_path, .path_strings(run),
    by = c("replication", "id")
  )

  per_path |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(
      n_paths = dplyr::n(),
      n_distinct = dplyr::n_distinct(.data$path),
      consistency = 1 - mean(.data$entropy),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$id)
}

.shannon <- function(x) {
  x <- x[x > 0]
  if (!length(x)) {
    return(0)
  }
  p <- x / sum(x)
  -sum(p * log(p))
}


# TraMineR bridge -------------------------------------------------------------

#' Build a TraMineR state-sequence object
#'
#' Converts the decision paths of a game run into a [TraMineR::seqdef()]
#' object over the board-area alphabet, ready for sequence analysis. The
#' alphabet is fixed to [sdt_board_areas()], so sequences from different runs
#' stay comparable even when a run never visits some area.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param replication Optional replication to restrict to. `NULL` keeps all,
#'   which is what you want when clustering across replications.
#'
#' @return A `stslist` object from \pkg{TraMineR}, carrying `replication` and
#'   `casualty` attributes recording where each row came from.
#' @seealso [sdt_seq_cluster()], [sdt_plot_sequences()].
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 12, seed = 1)
#' run <- sdt_simulate(sdt_scenario(co), replications = 2, seed = 1,
#'                     verbose = FALSE)
#' if (requireNamespace("TraMineR", quietly = TRUE)) {
#'   seqs <- sdt_seqdef(run)
#'   dim(seqs)
#' }
sdt_seqdef <- function(run, replication = NULL) {
  .check_class(run, "game_run")
  .require_pkgs("TraMineR")

  wide <- sdt_decision_paths(run, format = "wide")
  if (!is.null(replication)) {
    replication <- .check_count(replication)
    wide <- wide[wide$replication == replication, , drop = FALSE]
  }
  if (!nrow(wide)) {
    cli::cli_abort("No decision paths to convert.")
  }

  step_cols <- grep("^step_", names(wide), value = TRUE)
  labels <- sdt_board_areas()
  seqs <- suppressMessages(TraMineR::seqdef(
    as.data.frame(wide[, step_cols, drop = FALSE]),
    alphabet = labels,
    states = labels,
    labels = labels,
    xtstep = 1L
  ))
  attr(seqs, "replication") <- wide$replication
  attr(seqs, "casualty") <- wide$id
  seqs
}

#' Cluster decision paths
#'
#' Groups casualties by how similar their board pathways are: an optimal
#' matching dissimilarity between state sequences, then partitioning around
#' medoids. What comes out is a small number of characteristic ways through
#' the board, which is usually more useful than a per-casualty path listing.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param k Number of clusters.
#' @param method Dissimilarity passed to [TraMineR::seqdist()]. `"OM"` is
#'   optimal matching.
#' @param indel Insertion/deletion cost for optimal matching.
#' @param sm Substitution-cost matrix or method. `"CONSTANT"` applies a flat
#'   cost, the right default when no substitution is more plausible than
#'   another.
#'
#' @return A [tibble::tibble()] with one row per sequence: `replication`,
#'   `id`, `cluster`, and `medoid`, which flags the sequence representing its
#'   cluster.
#' @seealso [sdt_seqdef()], [sdt_plot_sequences()].
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 30, seed = 1)
#' run <- sdt_simulate(sdt_scenario(co), replications = 2, seed = 1,
#'                     verbose = FALSE)
#' if (requireNamespace("TraMineR", quietly = TRUE) &&
#'     requireNamespace("WeightedCluster", quietly = TRUE)) {
#'   sdt_seq_cluster(run, k = 3)
#' }
sdt_seq_cluster <- function(run, k = 3L, method = "OM",
                            indel = 1, sm = "CONSTANT") {
  .require_pkgs(c("TraMineR", "WeightedCluster"))
  k <- .check_count(k)
  .check_string(method)

  seqs <- sdt_seqdef(run)
  n <- nrow(seqs)
  if (k > n) {
    cli::cli_abort(c(
      "{.arg k} must not exceed the number of sequences.",
      "x" = "Asked for {k} cluster{?s} from {n} sequence{?s}."
    ))
  }
  dist <- suppressMessages(
    TraMineR::seqdist(seqs, method = method, indel = indel, sm = sm)
  )
  pam <- WeightedCluster::wcKMedoids(dist, k = k)

  tibble::tibble(
    replication = attr(seqs, "replication"),
    id = attr(seqs, "casualty"),
    cluster = as.integer(factor(pam$clustering)),
    medoid = seq_len(n) %in% unique(pam$clustering)
  )
}


# Plots -----------------------------------------------------------------------

#' Plot decision-path sequences
#'
#' Two standard sequence-analysis views, both drawn with \pkg{ggplot2} rather
#' than \pkg{TraMineR}'s base-graphics plotting, so they compose and theme like
#' every other plot in the package.
#'
#' `"index"` draws one row per casualty, coloured by the area occupied at each
#' position: the shape of individual pathways. `"distribution"` draws the share
#' of casualties in each area at each position: the shape of the cohort.
#'
#' @param run A `game_run` from [sdt_simulate()].
#' @param type `"index"` or `"distribution"`.
#' @param replication Optional replication to restrict to.
#' @param clusters Optional cluster assignment from [sdt_seq_cluster()]. When
#'   supplied, the plot is facetted by cluster.
#'
#' @return A \pkg{ggplot2} object.
#' @seealso [sdt_seq_cluster()], [sdt_decision_paths()].
#' @export
#' @examples
#' co <- sdt_synthetic_cohort(n = 20, seed = 1)
#' run <- sdt_simulate(sdt_scenario(co), replications = 2, seed = 1,
#'                     verbose = FALSE)
#' sdt_plot_sequences(run)
#' sdt_plot_sequences(run, type = "distribution")
sdt_plot_sequences <- function(run,
                               type = c("index", "distribution"),
                               replication = NULL,
                               clusters = NULL) {
  .check_class(run, "game_run")
  type <- rlang::arg_match(type)

  long <- sdt_decision_paths(run, format = "long")
  if (!is.null(replication)) {
    replication <- .check_count(replication)
    long <- long[long$replication == replication, , drop = FALSE]
  }
  if (!nrow(long)) {
    return(.sdt_blank_plot("No decision paths"))
  }
  # Labelled, not named. A legend reading "ed_red" and "icu_imc" is the data
  # model leaking onto a figure that will end up in a slide deck.
  long$area <- factor(
    .sdt_label_area(long$area),
    levels = .sdt_label_area(sdt_board_areas())
  )

  if (!is.null(clusters)) {
    long <- dplyr::left_join(
      long, clusters[, c("replication", "id", "cluster")],
      by = c("replication", "id")
    )
  }

  plot <- if (identical(type, "index")) {
    .plot_seq_index(long)
  } else {
    .plot_seq_distribution(long)
  }
  if (!is.null(clusters)) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data$cluster))
  }
  plot
}

.plot_seq_index <- function(long) {
  row_id <- paste(long$replication, long$id, sep = ":")
  long$row <- factor(row_id, levels = rev(sort(unique(row_id))))

  ggplot2::ggplot(long) +
    ggplot2::geom_tile(
      ggplot2::aes(x = .data$step, y = .data$row, fill = .data$area),
      colour = NA
    ) +
    ggplot2::scale_fill_viridis_d(drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq_len(max(long$step))) +
    ggplot2::labs(
      title = "Decision-path index plot",
      x = "Position in pathway", y = NULL, fill = "Board area"
    ) +
    .sdt_theme() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())
}

.plot_seq_distribution <- function(long) {
  by_step <- long |>
    dplyr::count(.data$step, .data$area, name = "n") |>
    dplyr::group_by(.data$step) |>
    dplyr::mutate(share = .data$n / sum(.data$n)) |>
    dplyr::ungroup()

  ggplot2::ggplot(by_step) +
    ggplot2::geom_col(
      ggplot2::aes(x = .data$step, y = .data$share, fill = .data$area),
      width = 0.9
    ) +
    ggplot2::scale_fill_viridis_d(drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq_len(max(by_step$step))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
    ggplot2::labs(
      title = "Decision-path state distribution",
      x = "Position in pathway", y = "Share of casualties",
      fill = "Board area"
    ) +
    .sdt_theme()
}
