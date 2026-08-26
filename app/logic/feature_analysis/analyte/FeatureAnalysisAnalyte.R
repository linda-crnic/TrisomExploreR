box::use(
    R6[R6Class],
    dplyr[case_when]
)

box::use(
    app/logic/shared/factory_routing[resolve_class],
    app/logic/shared/global_utils[`%||%`],
    app/logic/feature_analysis/analyte/AnalyteDataManagers[RuntimeAnalyteDataSource,
        PreCalcualtedAnalyteDataSource, CorrelatesAnalyteDataSource, PreCalcualtedTOFAAnalyteDataSource],
    app/logic/feature_analysis/analyte/AnalyteDataPreparers[CategoricalSinglePreparer, PrecalculatedCategoricalSinglePreparer,
        PrecalculatedLog2CategoricalSinglePreparer, TOFAEndpointsCategoricalPreparer, ContinuousSinglePreparer,
        HeatmapPreparer, CorrelatesPreparer, CorrelatesHeatmapPreparer],
    app/logic/feature_analysis/analyte/AnalytePlotStrategies[BoxPlotStrategy, ScatterPlotStrategy,
        HeatmapPlotStrategy, ScatterPlotWithSmoothingStrategy]
)

getPlotKind <- function(analysis_type, analyte) {
    vars_n <- length(analyte %||% character())
    if (vars_n > 1) return("Heatmap")
    type <- trimws(analysis_type)
    if (type == "Continuous") return("Scatter")
    if (type == "Categorical") return("Box")
    if (type == "Correlates") return("Correlates")
    stop(sprintf("Unknown AnalysisType: '%s'", type), call. = FALSE)
}

DATA_SOURCE_MAP <- list(
    Precalc = PreCalcualtedAnalyteDataSource,
    TOFA = PreCalcualtedTOFAAnalyteDataSource,
    Runtime = RuntimeAnalyteDataSource,
    Correlates = CorrelatesAnalyteDataSource
)

PREPARER_MAP <- list(
    Scatter = ContinuousSinglePreparer,
    Box = CategoricalSinglePreparer,
    PrecalcBox = PrecalculatedCategoricalSinglePreparer,
    PrecalcLog2Box = PrecalculatedLog2CategoricalSinglePreparer,
    TOFAEndpointsBox = TOFAEndpointsCategoricalPreparer,
    Heatmap = HeatmapPreparer,
    Correlates = CorrelatesPreparer,
    CorrelatesHeatmap = CorrelatesHeatmapPreparer
)

isPrecalculatedLog2Package <- function(study_plan = NULL) {
    if (is.null(study_plan)) {
        return(FALSE)
    }

    package_manifest <- study_plan$package_manifest %||% list()
    fact_name <- study_plan$fact_name

    if (is.null(fact_name) || !nzchar(fact_name)) {
        fact_names <- names(package_manifest$facts %||% list())
        fact_names <- fact_names[!is.na(fact_names) & nzchar(trimws(fact_names))]

        if (length(fact_names) > 0) {
            fact_name <- fact_names[[1]]
        }
    }

    fact_cfg <- NULL

    if (!is.null(fact_name) && nzchar(fact_name)) {
        fact_cfg <- (package_manifest$facts %||% list())[[fact_name]]
    }

    scale_tag <- tolower(trimws(as.character(
        fact_cfg$measurement_scale %||% fact_cfg$measurement_transform %||% ""
    )))
    identical(scale_tag, "log2")
}

PLOT_STRATEGY_MAP <- list(
    Scatter = ScatterPlotStrategy,
    Box = BoxPlotStrategy,
    Heatmap = HeatmapPlotStrategy,
    Correlates = ScatterPlotWithSmoothingStrategy
)

getRouteProfile <- function(precalculated, analysis_type, analyte, analysis_config, study = NULL, study_plan = NULL) {
    normalized_analysis_type <- trimws(analysis_type)
    plot_kind <- getPlotKind(normalized_analysis_type, analyte)
    is_precalculated_log2 <- isPrecalculatedLog2Package(study_plan)

    data_source_key <- case_when(
        normalized_analysis_type == "Correlates" ~ "Correlates",
        analysis_config$Namespace == "Timepoint" && isTRUE(precalculated) ~ "TOFA",
        isTRUE(precalculated) ~ "Precalc",
        TRUE ~ "Runtime"
    )

    preparer_key <- case_when(
        normalized_analysis_type == "Correlates" && plot_kind == "Scatter" ~ "Correlates",
        normalized_analysis_type == "Correlates" && plot_kind == "Heatmap" ~ "CorrelatesHeatmap",
        isTRUE(precalculated) && plot_kind == "Box" && identical(study, "tofa_proteomics_endpoints") ~ "TOFAEndpointsBox",
        isTRUE(precalculated) && plot_kind == "Box" && isTRUE(is_precalculated_log2) ~ "PrecalcLog2Box",
        isTRUE(precalculated) && plot_kind == "Box" ~ "PrecalcBox",
        TRUE ~ plot_kind
    )

    plotter_key <- case_when(
        normalized_analysis_type == "Correlates" && plot_kind == "Correlates" ~ "Correlates",
        TRUE ~ plot_kind
    )

    list(
        plot_kind = plot_kind,
        data_source_key = data_source_key,
        preparer_key = preparer_key,
        plotter_key = plotter_key,
        normalized_analysis_type = normalized_analysis_type
    )
}

resolvePrecalculatedMode <- function(precalculated, study_plan = NULL) {
    if (is.null(study_plan) || is.null(study_plan$execution_mode)) {
        return(precalculated)
    }

    if (identical(study_plan$execution_mode, "generated")) {
        return(FALSE)
    }

    if (identical(study_plan$execution_mode, "precalculated")) {
        return(TRUE)
    }

    precalculated
}

instantiateMappedClass <- function(map, key, kind, analysis_config, analyte, ...) {
    cls <- resolve_class(map, key, kind)
    cls$new(analysis_config = analysis_config, analyte = analyte, ...)
}

getDataSource <- function(route_profile, analysis_config, analyte, study_plan = NULL, ...) {
    instantiateMappedClass(
        DATA_SOURCE_MAP,
        route_profile$data_source_key,
        "DataSource",
        analysis_config,
        analyte,
        study_plan = study_plan,
        ...
    )
}

getPreparer <- function(route_profile, analysis_config, analyte, ...) {
    instantiateMappedClass(PREPARER_MAP, route_profile$preparer_key, "Preparer", analysis_config, analyte, ...)
}

getPlotStrategy <- function(route_profile, analysis_config, analyte, ...) {
    instantiateMappedClass(PLOT_STRATEGY_MAP, route_profile$plotter_key, "PlotStrategy", analysis_config, analyte, ...)
}

FeatureAnalysisAnalyteRunner <- R6Class(
    "FeatureAnalysisAnalyteRunner",
    public = list(
        precalculated = NULL,
        analysis_type = NULL,
        data_source = NULL,
        preparer = NULL,
        plotter = NULL,
        analyte = NULL,
        initialize = function(
            precalculated,
            analysis_type,
            data_source,
            preparer,
            plotter,
            analyte
        ) {
            self$precalculated <- precalculated
            self$analysis_type <- analysis_type
            self$data_source <- data_source
            self$preparer <- preparer
            self$plotter <- plotter
            self$analyte <- analyte
        },
        get_analyte_data = function(analyte) {
            self$data_source$get_data(analyte) |>
                self$preparer$prepare()
        },
        get_analyte_plot = function(.data) {
            self$plotter$render(.data)
        },
        get_table_data = function() {
            if (is.null(self$preparer$prepared_data)) {
                self$get_analyte_data(self$analyte)
            }
            self$preparer$formatted_analyte_data
        }
    )
)

#' @export
getFeatureAnalysisForAnalyte <- function(
        analysis_config,
        analyte,
    study_plan = NULL,
        ...
    ) {
    args_list <- list(...)
    study <- args_list$study
    precalculated <- resolvePrecalculatedMode(
        NULL,
        study_plan = study_plan
    )
    route_profile <- getRouteProfile(
        precalculated,
        analysis_config$AnalysisType,
        analyte,
        analysis_config,
        study = study,
        study_plan = study_plan
    )
    data_src <- getDataSource(route_profile, analysis_config, analyte, study_plan = study_plan, ...)
    preparer <- getPreparer(route_profile, analysis_config, analyte, ...)
    plotter <- getPlotStrategy(route_profile, analysis_config, analyte, ...)

    FeatureAnalysisAnalyteRunner$new(
        precalculated,
        route_profile$normalized_analysis_type,
        data_src,
        preparer,
        plotter,
        analyte
    )
}
