# Reference distributions for calibrating sandtablR's generators and for
# overlaying "observed vs reference" in the workbench.
#
# Run this file to (re)generate inst/extdata/tdsc_reference.rds.
#
# PROVENANCE. The list is split deliberately into three parts.
#
#   $published  Marginals attributed to the TDSC tabletop evaluation
#               (Achatz et al. 2023, Eur J Trauma Emerg Surg 49(2):607-617,
#               doi:10.1007/s00068-022-02217-1; open access, CC BY 4.0). Both
#               evaluated versions of the exercise are carried. Every value
#               below is traceable to a numbered figure, a table or a sentence
#               in that paper, and the figure is named beside it.
#
#   $design     The exercise as specified in the paper's Methods: how long it
#               runs, how many cards, how the board is laid out. Also published,
#               but structural rather than distributional.
#
#   $defaults   Modelling choices made by sandtablR where the source does not
#               pin a value. They are NOT attributed to the paper. Anything
#               promoted from $defaults to $published must first be checked
#               against the published text.
#
# Keeping the parts apart is the point: a calibration test that mixes
# fitted-to-us defaults with published marginals proves nothing.

# The eight locations Fig. 6 reports, in board order. The green treatment area
# (board position 9) is not among them, so it carries no published share.
end_state_areas <- c(
  "triage", "ed", "diagnostics", "or", "recovery", "icu_imc", "ward",
  "not_seen"
)

# --- Version 2.0, the current exercise ---------------------------------------

v2_0 <- list(
  # Sighting-category mix of the casualty stream. Results, "Sighting frequency
  # and hit accuracy of the triage categories".
  triage_mix = c(red = 0.444, yellow = 0.236, green = 0.319),
  # Proportion of casualties correctly categorised by participants, per
  # category, and overall. Fig. 5 and the surrounding text.
  triage_hit = c(red = 0.8822, yellow = 0.590, green = 0.7075),
  triage_hit_overall = 0.7694,
  # The lowest and highest per-patient hit accuracy observed in any group,
  # which is the spread the mean above conceals.
  triage_hit_min = c(red = 0.28, yellow = 0.39, green = 0.00),
  triage_hit_max = c(red = 1.00, yellow = 1.00, green = 1.00),
  # How many of the 20 cards carried each category. Results text.
  casualties_per_category = c(red = 9L, yellow = 3L, green = 8L),
  # Therapy-concept mix. ETC is never chosen under mass-casualty conditions.
  # Fig. 8; n = 105 procedures, DCS n = 31, TASC n = 74.
  therapy_mix = c(ETC = 0.000, DCS = 0.295, TASC = 0.705),
  therapy_n = c(ETC = 0L, DCS = 31L, TASC = 74L),
  therapy_total = 105L,
  # Diagnostic modality mix. Fig. 7; n = 183 diagnostic measures.
  diagnostics_mix = c(efast = 0.831, xray = 0.022, ct = 0.148),
  diagnostics_total = 183L,
  # Where casualties stand when the game is called. Fig. 6 and the itemised
  # list beside it. These eight sum to 0.98; the paper reports them rounded to
  # whole percentages and does not attribute the remainder.
  end_state = c(
    triage = 0.08, ed = 0.16, diagnostics = 0.04, or = 0.11,
    recovery = 0.01, icu_imc = 0.23, ward = 0.35, not_seen = 0.00
  ),
  # Study scale. Methods, "Study group description".
  n_charts = 360L,
  n_charts_played = 369L,
  n_groups = 18L,
  n_participants = 49L,
  n_courses = 3L
)

# --- Version 1.0, the predecessor --------------------------------------------

v1_0 <- list(
  triage_mix = c(red = 0.434, yellow = 0.217, green = 0.350),
  triage_hit = c(red = 0.8141, yellow = 0.4567, green = 0.7341),
  triage_hit_overall = 0.7425,
  triage_hit_min = c(red = 0.077, yellow = 0.16, green = 0.13),
  triage_hit_max = c(red = 1.00, yellow = 1.00, green = 1.00),
  casualties_per_category = c(red = 9L, yellow = 3L, green = 8L),
  therapy_mix = c(ETC = 0.000, DCS = 0.225, TASC = 0.775),
  therapy_n = c(ETC = 0L, DCS = 18L, TASC = 62L),
  therapy_total = 80L,
  diagnostics_mix = c(efast = 0.769, xray = 0.105, ct = 0.126),
  diagnostics_total = 247L,
  end_state = c(
    triage = 0.15, ed = 0.17, diagnostics = 0.06, or = 0.09,
    recovery = 0.01, icu_imc = 0.20, ward = 0.31, not_seen = 0.03
  ),
  n_charts = 360L,
  n_charts_played = 369L,
  n_groups = 19L,
  n_participants = 55L,
  n_courses = 3L
)

stopifnot(
  identical(names(v2_0), names(v1_0)),
  identical(names(v2_0$end_state), end_state_areas),
  identical(names(v1_0$end_state), end_state_areas)
)

# --- The exercise as specified ------------------------------------------------

design <- list(
  # Methods, "Relevant elements of the table-top exercise".
  duration = 90,
  n_cards = 20L,
  card_rounds = 4L,
  team_size = 3L,
  roles = c(
    LArS = "Senior Triage Coordinator",
    ZONK = "Emergency Operational and Medical Coordinator",
    helper = "ZONK helper, leads the whiteboard"
  ),
  scenario = paste0(
    "Explosion at a rock concert with more than 2000 spectators; ",
    "20 casualties reach one hospital."
  ),
  # The board, numbered and laid out as in Fig. 1. `area` names the package's
  # board areas, `label` is what the figure calls them, and xmin/xmax/ymin/ymax
  # place each area on a 20-by-2 grid reproducing the figure's two rows: four
  # wide areas along the top, five along the bottom.
  board = data.frame(
    position = 1:9,
    area = c("triage", "ed_red", "diagnostics", "or", "recovery",
             "ed_yellow", "icu_imc", "ward", "green_area"),
    label = c("Screening area", "Emergency treatment area red",
              "Radiology", "Operating room", "Recovery room",
              "Emergency treatment area yellow",
              "Intensive / intermediate care",
              "Ward block", "Treatment area green"),
    xmin = c(0, 4, 8, 5, 10, 12, 14, 16, 0),
    xmax = c(4, 8, 12, 10, 14, 16, 20, 20, 5),
    ymin = c(0, 0, 0, 1, 1, 0, 1, 0, 1),
    ymax = c(1, 1, 1, 2, 2, 1, 2, 1, 2),
    stringsAsFactors = FALSE
  ),
  # The four tasks the exercise drills, in order.
  tasks = c("Categorising", "Prioritising", "Coordinating", "Implementing"),
  # Life points as printed on the version 2.0 patient card (Fig. 1): a death
  # marker followed by six numbered points.
  life_points = 1:6,
  # The sighting algorithm the exercise uses, and its published benchmark.
  triage_algorithm = "Berlin hospital screening algorithm (SK I / II / III)",
  triage_algorithm_benchmark = 0.80
)

# --- sandtablR's own modelling choices ---------------------------------------

# Values marked "fitted" below were found by data-raw/calibrate.R, which
# searches them for the setting that best reproduces $published$end_state. Do
# not edit one by hand: change it and the calibration test will fail, and the
# honest fix is to re-run the search. The fit as recorded reproduces all eight
# published locations to within 2.2 percentage points, at a root-mean-square
# error of 1.3 percentage points.

defaults <- list(
  # Mean service time in minutes per board area, used as exponential rates.
  # triage, diagnostics, or and green_area are fitted.
  # morgue and external carry a zero time and are never drawn: no trajectory
  # seizes them, because the engine has no branch to either. They are present
  # so the vector stays total over sdt_board_areas().
  service_times = c(
    triage = 12.679, ed_red = 20, ed_yellow = 15, diagnostics = 13.001,
    or = 123.844, recovery = 30, icu_imc = 120, ward = 15,
    green_area = 63.077,
    morgue = 0, external = 0,
    not_seen = 0
  ),
  # Theatre time scales with the therapy concept: damage control is quicker
  # than early total care, abbreviated surgical care quicker still.
  # "none" never reaches theatre and is carried only so the lookup is total.
  or_multiplier = c(ETC = 1.35, DCS = 1.00, TASC = 0.70, none = 0.00),
  # Probability a casualty receives each diagnostic, by sighting category.
  # The published mix (above) is over procedures performed, not over
  # casualties, so it cannot be used directly here; these are chosen to
  # reproduce it and the calibration test checks that they do.
  diagnostics = list(
    efast = c(red = 0.90, yellow = 0.75, green = 0.20),
    xray  = c(red = 0.03, yellow = 0.02, green = 0.01),
    ct    = c(red = 0.24, yellow = 0.10, green = 0.02)
  ),
  # Probability of going to theatre after diagnostics, by category. A casualty
  # whose therapy concept is "none" is held regardless. Fitted.
  p_or = c(red = 0.8144, yellow = 0.1148, green = 0.0000),
  # Probability of critical care rather than a ward bed after theatre. Fitted.
  p_icu = c(red = 0.9331, yellow = 0.4306, green = 0.0000),
  # Mean observation time in minutes for a casualty who is worked up but not
  # taken to theatre, and the probability that observation ends in critical
  # care rather than a ward bed. Both fitted.
  observation = c(red = 101.262, yellow = 140.532, green = 40),
  p_icu_conservative = c(red = 0.5599, yellow = 0.2321, green = 0.0000),
  # How the casualty stream reaches the board. The game leader plays the cards
  # in four tranches at fixed points ($design$card_rounds), so arrivals are
  # modelled as four bursts rather than a smooth stream: `tranche_starts` are
  # the fractions of the horizon at which each burst begins, and
  # `tranche_spread` is the fraction of the horizon a burst is spread over.
  # Both fitted.
  arrivals = list(
    tranche_starts = c(0.0000, 0.3093, 0.3569, 0.6286),
    tranche_spread = 0.1115
  ),
  # Mean inter-arrival time (minutes). Retained for callers that want a smooth
  # stream; the engine uses the tranche schedule above.
  interarrival = 3,
  # Life-point band per sighting category on the published 1-6 scale; lower is
  # more critical.
  life_bands = list(red = 1:2, yellow = 3:4, green = 5:6),
  # Age distribution of the casualty stream.
  age = c(mean = 42, sd = 15),
  # Recorded sex mix.
  sex_mix = c(f = 0.45, m = 0.53, d = 0.02),
  # Therapy concept conditioned on sighting category. "none" is the card's
  # "keine OP": worked up, but no operation indicated.
  #
  # ETC is zero everywhere. The source is explicit that in neither version was
  # any therapy carried out on the early-total-care principle, and a generator
  # that produced a few anyway would put a level in the training data that the
  # exercise never generates.
  #
  # Among the casualties that do go to theatre, the DCS share is the published
  # 29.5% in every category. The source reports the therapy mix over procedures
  # performed and does not break it down by sighting category, so splitting it
  # differently between categories would be inventing a structure it does not
  # describe -- and would move the marginal off the one number it does.
  # Only the "none" shares below are free, and they are ours.
  therapy_given_triage = list(
    red    = c(ETC = 0.00, DCS = 0.2950, TASC = 0.7050, none = 0.00),
    yellow = c(ETC = 0.00, DCS = 0.2596, TASC = 0.6204, none = 0.12),
    green  = c(ETC = 0.00, DCS = 0.0885, TASC = 0.2115, none = 0.70)
  ),
  # Probability a red casualty arrives ventilated.
  p_ventilated_red = 0.6,

  # Board areas map onto the eight locations Fig. 6 reports. Two of these
  # pairings are modelling choices, not published facts:
  #
  #   ed_red + ed_yellow -> ed    The paper reports one "emergency department
  #                               treatment area red and yellow" bar.
  #   green_area -> ward          Fig. 6 has no green-area bar, so the green
  #                               treatment area must sit inside one of the
  #                               eight. Ward is the only plausible home: it is
  #                               a holding disposition, and its published
  #                               share (35%) is the only one large enough to
  #                               contain the green stream (32%).
  #   morgue -> ward             Fig. 6 has no mortuary bar and the exercise
  #   external -> ward           records no transfer out, so both terminal
  #                              states have to land somewhere among the eight.
  #                              Ward is the holding disposition the paper
  #                              already absorbs the green area into. This is a
  #                              modelling choice, not a published fact, and it
  #                              is the one summary.game_run(map=) exists to let
  #                              a caller vary: on real course data, folding
  #                              them to "ward" versus dropping them from the
  #                              denominator moves max|difference| against the
  #                              published v1.0 marginal from 0.043 to 0.061.
  end_state_map = c(
    triage = "triage", ed_red = "ed", ed_yellow = "ed",
    diagnostics = "diagnostics", or = "or", recovery = "recovery",
    icu_imc = "icu_imc", ward = "ward", green_area = "ward",
    morgue = "ward", external = "ward",
    not_seen = "not_seen"
  ),

  # What each board area *is*, as distinct from where it is reported. The
  # engine does not read this; it records a structural fact that the
  # end_state_map alone hides, because the map folds the green area onto the
  # ward and thereby makes a holding area look like a disposition.
  #
  # Measured on 19 real course exports (149 team play-throughs): the green
  # area is terminal for only 10.4% of visits (n = 838) against 98.3% for the
  # ward, and 58.1% of the casualties who leave it are escalated to the yellow
  # emergency department. It is a holding and re-triage area, not a bed.
  #
  # The published calibration depends on the green area folding to the ward,
  # so this is documentation of a known divergence, not a lever: changing the
  # fold moves max|difference| from 0.040 to 0.272.
  area_roles = c(
    triage = "transit", ed_red = "transit", ed_yellow = "transit",
    diagnostics = "transit", or = "transit", recovery = "transit",
    icu_imc = "disposition", ward = "disposition",
    green_area = "holding",
    morgue = "terminal", external = "terminal",
    not_seen = "unreached"
  ),

  # The board the engine is calibrated on. sdt_simulate()'s defaults were
  # chosen so that this configuration reproduces the published $end_state; the
  # calibration test in tests/testthat/test-simulate.R checks that it still
  # does. Change these and the calibration has to be redone.
  #
  # The horizon is not the published 90 minutes. The exercise pauses four times
  # for the game leader to play the next tranche of cards, so 90 minutes of
  # wall clock covers a longer stretch of simulated care; the engine runs in
  # continuous simulated time and needs the longer horizon to land on the
  # published end state. The published figure is in $design$duration.
  calibration = list(
    n = 40L,
    horizon = 270,
    seed = 1L,
    # triage, diagnostics and or are fitted; the rest are set generously so
    # they do not become accidental bottlenecks.
    resources = list(
      triage = 2L, ed_red = 5L, ed_yellow = 4L, diagnostics = 7L,
      or = 7L, recovery = 4L, icu_imc = 12L, ward = 25L, green_area = 12L
    )
  )
)

tdsc_reference <- list(
  # $published is version 2.0, the current exercise, so that code written
  # against it gets the current numbers by default.
  published = v2_0,
  versions = list(`2.0` = v2_0, `1.0` = v1_0),
  design = design,
  defaults = defaults,

  meta = list(
    source = paste0(
      "Achatz G, Reckziegel A, Friemert B, Blaetzinger M, Hinz-Bauer S, ",
      "Franke A, Bieler D, Paffrath T, Hoth P. Evaluation of the ",
      "decision-making process within the table-top exercise of the Terror ",
      "and Disaster Surgical Care (TDSC) course. ",
      "Eur J Trauma Emerg Surg 2023;49(2):607-617."
    ),
    doi = "10.1007/s00068-022-02217-1",
    pmid = "36792724",
    licence = "CC BY 4.0",
    versions = c("2.0", "1.0"),
    note = paste0(
      "sandtablR is a generic re-implementation built from the published ",
      "specification. It is not TDSC course software and reproduces none ",
      "of the course's materials."
    )
  )
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
saveRDS(tdsc_reference, "inst/extdata/tdsc_reference.rds", version = 2L)
