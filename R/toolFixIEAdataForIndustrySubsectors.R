#' Apply adjustments to industry-related IEA data
#'
#' This function prepares the industry-related IEA before mapping it to REMIND sectors. There are three different types of adjustments done:
#' 1. replace coke oven and blast furnace outputs (`BLFURGS`, `OGASES`, `OVENCOKE`,
#' `COKEOVGS`, `COALTAR`, `NONCRUDE`) by inputs
#' (required for dealing with energy flows from the steel sector to other sectors)
#' 2. prepare industry-related time series
#' 3. apply corrections to IEA data to cope with fragmentary time series
#'
#' The corrections done by this function are rather rudimentary and crude. This
#' gets smoothed away in regional aggregation. But do not use the resulting
#' country-level data without additional scrutiny.
#'
#' Use regional or global averages if IEA industry data lists energy use only as
#' "non-specified".
#'
#' @md
#' @param data MAgPIE object containing the IEA Energy Balances data
#'
#' @param ieamatch mapping of IEA product/flow combinations to REMIND sectors and energy carriers
#'
#'
#' @param threshold minimum share each industry subsector uses of each product.
#'   Defaults to 1 %.
#' @param fixing temporary flag
#'
#' @return a MAgPIE object
#'
#' @author Michaja Pehl, Felix Schreyer
#'
#' @importFrom assertr not_na assert
#' @importFrom dplyr anti_join group_by inner_join left_join mutate pull rename
#'     select summarise
#' @importFrom readr read_delim cols col_skip col_character
#' @importFrom quitte cartesian interpolate_missing_periods overwrite
#'             character.data.frame interpolate_missing_periods_
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom stats na.omit
#' @importFrom tidyr complete gather nesting spread crossing
#' @export
toolFixIEAdataForIndustrySubsectors <- function(data, fixing, threshold = 1e-2) {

  ####

  # This function contains the following steps:

  # (CO = coke oven,
  #  BF = blast furnace)

  # 1. Replace steel sector outputs by inputs
  #   1.1 Define functions
  #   1.2 Prepare data and define flows
  #   1.3 Replace flows of BF outputs and by inputs into BF
  #   1.4 Replace flows of CO outputs by inputs to CO
  #   1.5 Calculate CO Losses
  #   1.6 Recalculate BF inputs w/ CO replacements
  #   1.7 Calculate BF Losses
  #   1.8 Replace IEA data with steel sector adjustments
  # 2. Prepare Industry Subsectors Timeseries
  #   2.1 Define flows and mappings
  #   2.2 Extend industry subsector timeseries
  #   2.3 Apply five-year moving average
  # 3. Fix suspicious industry products
  #   3.1 Prepare data to fix
  #   3.2 Redistribute products to industry-related flows
  #   3.3 Replace and append data

  ####

  # 1. Replace steel sector outputs by inputs ----

  ## 1.1 Define functions ----

  . <- NULL

  .clean_data <- function(m, keep_zeros = FALSE) {
    m %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2',
             value = 'Value') %>%
      filter(0 != .data$value | keep_zeros) %>%
      character.data.frame() %>%
      mutate(year = as.integer(.data$year))
  }

  ## 1.2 Prepare data and define flows ----

  ## flow definitions
  IEA_flows <- tribble(
    ~summary.flow,   ~flow,
    # Total Primary Energy Production
    'TES',          'INDPROD',    # primary energy production
    'TES',          'IMPORTS',
    'TES',          'EXPORTS',
    'TES',          'MARBUNK',    # international marine bunkers
    'TES',          'AVBUNK',     # international aviation bunkers
    NA_character_,  'TRANSFER',   # inter-product transfers, product transfers,
    # and recycling
    'TES',          'STOCKCHA',   # stock changes

    # Transformation Processes
    'TOTTRANF',      'MAINELEC',    # main activity producer electricity plants
    'TOTTRANF',      'AUTOELEC',    # autoproducer electricity plants
    'TOTTRANF',      'MAINCHP',     # main activity producer CHP plants
    'TOTTRANF',      'AUTOCHP',     # autoproducer electricity plants
    'TOTTRANF',      'MAINHEAT',    # main activity producer heat plants
    'TOTTRANF',      'AUTOHEAT',    # autoproducer heat plants
    'TOTTRANF',      'THEAT',       # heat pumps
    'TOTTRANF',      'TBOILER',     # electric boilers
    'TOTTRANF',      'TELE',        # chemical heat for electricity production
    'TOTTRANF',      'TBLASTFUR',   # blast furnaces
    'TOTTRANF',      'TGASWKS',     # gas works
    'TOTTRANF',      'TCOKEOVS',    # coke ovens
    'TOTTRANF',      'TPATFUEL',    # patent fuel plants
    'TOTTRANF',      'TBKB',        # peat briquette plants
    'TOTTRANF',      'TREFINER',    # oil refineries
    'TOTTRANF',      'TPETCHEM',    # petrochemical plants
    'TOTTRANF',      'TCOALLIQ',    # coal liquefaction plants
    'TOTTRANF',      'TGTL',        # gas-to-liquid plants
    'TOTTRANF',      'TBLENDGAS',   # blended natural gas
    'TOTTRANF',      'TCHARCOAL',   # charcoal production plants
    'TOTTRANF',      'TNONSPEC',    # non-specified transformation

    # Energy Industry Own Use and Losses
    'TOTENGY',       'EMINES',      # coal mines
    'TOTENGY',       'EOILGASEX',   # oil and gas extraction
    'TOTENGY',       'EBLASTFUR',   # blast furnaces
    'TOTENGY',       'EGASWKS',     # gas works
    'TOTENGY',       'EBIOGAS',     # gasifications plants for biogases
    'TOTENGY',       'ECOKEOVS',    # coke ovens
    'TOTENGY',       'EPATFUEL',    # patent fuel plants
    'TOTENGY',       'EBKB',        # peat briquette plants
    'TOTENGY',       'EREFINER',    # oil refineries
    'TOTENGY',       'ECOALLIQ',    # coal liquefaction plants
    'TOTENGY',       'ELNG',        # liquefaction/regasification plants
    'TOTENGY',       'EGTL',        # gas-to-liquied plants
    'TOTENGY',       'EPOWERPLT',   # own use in electricity, CHP, and heat

    # plants
    'TOTENGY',       'EPUMPST',     # pumped storage plants
    'TOTENGY',       'ENUC',        # nuclear industry
    'TOTENGY',       'ECHARCOAL',   # charcoal production plants
    'TOTENGY',       'ENONSPEC',    # non-specified energy industry

    # Final Consumption
    'TFC',           'IRONSTL',    # iron and steel
    'TFC',           'CHEMICAL',   # chemical and petrochemical
    'TFC',           'NONFERR',    # non-ferrous metals
    'TFC',           'NONMET',     # non-metallic minerals
    'TFC',           'TRANSEQ',    # transport equipment
    'TFC',           'MACHINE',    # machinery
    'TFC',           'MINING',     # mining and quarrying
    'TFC',           'FOODPRO',    # food production
    'TFC',           'PAPERPRO',   # paper, pulp, and print
    'TFC',           'WOODPRO',    # wood and wood products
    'TFC',           'CONSTRUC',   # construction
    'TFC',           'TEXTILES',   # textiles
    'TFC',           'INONSPEC',   # non-specified industry
    'TFC',           'WORLDAV',    # world aviation bunkers
    'TFC',           'DOMESAIR',   # domestic aviation
    'TFC',           'ROAD',       # road
    'TFC',           'RAIL',       # rail
    'TFC',           'PIPELINE',   # pipeline transport
    'TFC',           'WORLDMAR',   # world marine bunkers
    'TFC',           'DOMESNAV',   # domestic navigation
    'TFC',           'TRNONSPE',   # non-specified transport
    'TFC',           'RESIDENT',   # residential
    'TFC',           'COMMPUB',    # commercial and public services
    'TFC',           'AGRICULT',   # agriculture/forestry
    'TFC',           'FISHING',    # fishing
    'TFC',           'ONONSPEC',   # non-specified other consumption
    'TFC',           'NONENUSE',   # non-energy use

    'TOTIND',        'IRONSTL',    # iron and steel
    'TOTIND',        'CHEMICAL',   # chemical and petrochemical
    'TOTIND',        'NONFERR',    # non-ferrous metals
    'TOTIND',        'NONMET',     # non-metallic minerals
    'TOTIND',        'TRANSEQ',    # transport equipment
    'TOTIND',        'MACHINE',    # machinery
    'TOTIND',        'MINING',     # mining and quarrying
    'TOTIND',        'FOODPRO',    # food production
    'TOTIND',        'PAPERPRO',   # paper, pulp, and print
    'TOTIND',        'WOODPRO',    # wood and wood products
    'TOTIND',        'CONSTRUC',   # construction
    'TOTIND',        'TEXTILES',   # textiles
    'TOTIND',        'INONSPEC',   # non-specified industry

    # Transport
    'TOTTRANS',      'WORLDAV',    # world aviation bunkers
    'TOTTRANS',      'DOMESAIR',   # domestic aviation
    'TOTTRANS',      'ROAD',       # road
    'TOTTRANS',      'RAIL',       # rail
    'TOTTRANS',      'PIPELINE',   # pipeline transport
    'TOTTRANS',      'WORLDMAR',   # world marine bunkers
    'TOTTRANS',      'DOMESNAV',   # domestic navigation
    'TOTTRANS',      'TRNONSPE',   # non-specified transport

    # Other Consumption
    'TOTOTHER',      'RESIDENT',   # residential
    'TOTOTHER',      'COMMPUB',    # commercial and public services
    'TOTOTHER',      'AGRICULT',   # agriculture/forestry
    'TOTOTHER',      'FISHING',    # fishing
    'TOTOTHER',      'ONONSPEC',   # non-specified other consumption

    # Non-Energy Use
    'NONENUSE',      'NEINTREN',   # non-energy use in industry/transformation/
    # energy
    NA_character_,   'NECHEM',     # non-energy use chemical/petrochemical
    'NONENUSE',      'NETRANS',    # non-energy use in transport
    'NONENUSE',      'NEOTHER',    # non-energy use in other

    # Electricity Output
    'ELOUTPUT',      'ELMAINE',   # main activity producer electricity plants
    'ELOUTPUT',      'ELAUTOE',   # autoproducer electricity plants
    'ELOUTPUT',      'ELMAINC',   # main activity producer CHP plants
    'ELOUTPUT',      'ELAUTOC',   # autoproducer CHP plants

    # Heat Output
    'HEATOUT',       'HEMAINC',   # main activity producer CHP plants
    'HEATOUT',       'HEAUTOC',   # autoproducer CHP plants
    'HEATOUT',       'HEMAINH',   # main activity producer heat plants
    'HEATOUT',       'HEAUTOH'    # autoproducer heat plants
  )

  base_flows <- unique(IEA_flows$flow)
  summary_flows <- unique(na.omit(IEA_flows$summary.flow))
  all_flows <- intersect(
    c(base_flows, summary_flows), getNames(data, dim = "FLOW"))

  ### blast furnace flows to be replaced
  # all transformation, energy system and final consumption flows, except for
  # those related to blast furnaces
  flow_BLASTFUR_to_replace <- setdiff(all_flows, c('EBLASTFUR', 'TBLASTFUR'))

  ### coke oven flows to be replaced
  # all transformation, energy system and final consumption flows, except for
  # those related to coke ovens
  flow_COKEOVS_to_replace <- setdiff(all_flows, c('ECOKEOVS', 'TCOKEOVS'))

  ## 1.3 Replace flows of BF outputs and by inputs into BF ----

  # Example of how replacement routine works:
  # Flows of BF outputs into other sectors: BLFURGAS.MAINELEC = -20
  # BF inputs: OVENCOKE.TBLASTFUR = -90, COKCOAL.TBLASTFUR = -10
  # Flows of BF outputs are attributed to inputs via input shares:
  # New BF output flows
  # OVENCOKE.MAINELEC = -20 * (90 / 100) = -18
  # COKCOAL.MAINELEC = -20 * (10 / 100) = -2

  # all products in/out of blast furnace transformation and energy demand, except
  # summary flows 'TOTAL' and 'MRENEW'
  data_BLASTFUR <- data[, , c("EBLASTFUR", "TBLASTFUR")][, , c("TOTAL", "MRENEW"), invert = TRUE] %>%
    .clean_data() %>%
    group_by(!!!syms(c("iso3c", "year", "product"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  ### blast furnace inputs
  # inputs into transformation/energy system are negative
  data_BLASTFUR_inputs <- data_BLASTFUR %>%
    filter(0 > .data$value)

  ### blast furnace outputs
  # outputs from transformation are positive
  data_BLASTFUR_outputs <- data_BLASTFUR %>%
    filter(0 < .data$value)

  ### blast furnace output products
  # products blast furnaces supply to other flows
  outputs_BLASTFUR <- data_BLASTFUR_outputs %>%
    select(-'value')

  ### blast furnace product use
  data_BLASTFUR_use <- data[, , flow_BLASTFUR_to_replace] %>%
    .clean_data(keep_zeros = TRUE) %>%
    right_join(outputs_BLASTFUR, by = c('iso3c', 'year', 'product'))

  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products
  data_BLASTFUR_replacement <- right_join(
    data_BLASTFUR_inputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-'value'),

    data_BLASTFUR_use %>%
      select(-'product'),

    c('iso3c', 'year')
  ) %>%
    # assume that countries/years that have no inputs into blast furnaces,
    # also have no outputs and use of blast furnace products (e.g. ISR 1973)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid blast furnace replacement data') %>%
    mutate(value = .data$value * .data$factor) %>%
    group_by(!!!syms(c('iso3c', 'year', 'product', 'flow'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ## 1.4 Replace flwos of CO outputs by inputs to CO ----

  # Example of how replacement routine works:
  # Flow of CO outputs: COKEOVGS.TBLASTFUR = -10 (coke oven gas used in blast furnace)
  # CO inputs: COKCOAL.TCOKEOVS = -180, NATGAS.TCOKEOVS = -20
  # Flows of CO outputs are attributed to inputs via input shares:
  # New Flows CO outputs:
  # COKCOAL.TBLASTFUR = -10 * (180 / 200) = -9
  # NATGAS.TBLASTFUR = -10 * (20 / 200) = -1

  # all products in/out of coke oven transformation and energy demand, except
  # summary flows 'TOTAL' and 'MRENEW'
  data_COKEOVS <- data[, , c("ECOKEOVS", "TCOKEOVS")][, , c("TOTAL", "MRENEW"), invert = TRUE] %>%
    .clean_data() %>%
    group_by(!!!syms(c("iso3c", "year", "product"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  #### apply blast furnace replacement
  # Coke ovens and blast furnaces can be both inputs and outputs to one another at
  # the same time.  To untangle this, we first replace blast furnace outputs that
  # are inputs into coke ovens by coke oven outputs, which are netted with the
  # direct outputs (here), and then replace coke oven outputs that are blast
  # furnace inputs by coke oven inputs (further below).

  data_COKEOVS <- bind_rows(
    data_COKEOVS %>%
      anti_join(outputs_BLASTFUR,
                by = c('iso3c', 'year', 'product')),

    data_BLASTFUR_replacement %>%
      filter(.data$flow %in% c('ECOKEOVS', 'TCOKEOVS')) %>%
      select(-'flow')
  ) %>%
    group_by(!!!syms(c('iso3c', 'year', 'product'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ### coke oven inputs
  # inputs into transformation/energy system are negative
  data_COKEOVS_inputs <- data_COKEOVS %>%
    filter(0 > .data$value)

  ### coke oven outputs
  # outputs from transformation are positive
  data_COKEOVS_outputs <- data_COKEOVS %>%
    filter(0 < .data$value)

  ### coke oven output products
  # products blast furnaces supply to other flows
  outputs_COKEOVS <- data_COKEOVS_outputs %>%
    select(-'value')

  ### coke oven product use
  data_COKEOVS_use <- data[, , flow_COKEOVS_to_replace] %>%
    .clean_data(keep_zeros = TRUE) %>%
    right_join(outputs_COKEOVS, by = c('iso3c', 'year', 'product'))

  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use coke oven products
  data_COKEOVS_replacement <- right_join(
    data_COKEOVS_inputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-'value'),

    data_COKEOVS_use %>%
      select(-'product'),

    c('iso3c', 'year')
  ) %>%
    # assume that countries/years that have no inputs into coke ovens,
    # also have no outputs and use of coke oven products
    # (in reality these products may be imported, but we neglect this case)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid coke oven replacement data') %>%
    mutate(value = .data$value * .data$factor) %>%
    select('iso3c', 'year', 'product', 'flow', 'value')

  ## 1.5 Calculate CO Losses ----
  # coke oven losses (true losses from ECOKEOVS and transformation energy from
  # TCOKEOVS) are allotted to the IRONSTL sector
  # losses are the difference of inputs and outputs, weighted by input shares
  # right_join() filters out countries/years that do not use coke oven products

  # Example calculation of transformation losses:
  # CO outputs: COKEOVGS.TBLASTFUR = -10 (coke oven gas used in blast furnace)
  # CO inputs: COKCOAL.TCOKEOVS = -180, NATGAS.TCOKEOVS = -20
  # Transformation losses are calculated as difference between
  # inputs and outputs that are attributed to inputs by input shares:
  # Coke oven energy losses:
  # COKCOAL.IRONSTL = 180 - 10 * (180 / 200) = 171
  # NATGAS.IRONSTL = 20 - 10 * (20 / 200) = 18
  # Note coke oven losses are attributed to IRONSTL flow.

  data_COKEOVS_loss <- right_join(
    data_COKEOVS_inputs,

    data_COKEOVS_outputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      summarise(output = sum(.data$value), .groups = 'drop'),

    c('iso3c', 'year')
  ) %>%
    # assume that countries/years that have no inputs into coke ovens,
    # also have no transformation losses
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid coke oven loss data') %>%
    group_by(!!!syms(c('iso3c', 'year'))) %>%
    mutate(value = (sum(-.data$value) - .data$output)
           * .data$value / sum(.data$value),
           flow = 'IRONSTL') %>%
    ungroup() %>%
    select('iso3c', 'year', 'product', 'flow', 'value')

  ## 1.6 Recalculate BF inputs w/ CO replacements ----

  #### apply coke oven replacement
  # Coke ovens and blast furnaces can be both inputs and outputs to one another at
  # the same time.  To untangle this, we first replace blast furnace outputs that
  # are inputs into coke ovens by coke oven outputs, which are netted with the
  # direct outputs (above), and then replace coke oven outputs that are blast
  # furnace inputs by coke oven inputs (here).
  data_BLASTFUR <- bind_rows(
    data_BLASTFUR %>%
      anti_join(outputs_COKEOVS, by = c('iso3c', 'year', 'product')),

    data_COKEOVS_replacement %>%
      filter(.data$flow %in% c('EBLASTFUR', 'TBLASTFUR')) %>%
      select(-'flow')
  ) %>%
    group_by(!!!syms(c('iso3c', 'year', 'product'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ### blast furnace inputs
  # inputs into transformation/energy system are negative
  data_BLASTFUR_inputs <- data_BLASTFUR %>%
    filter(0 > .data$value)

  ### blast furnace replacement data
  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products
  data_BLASTFUR_replacement <- right_join(
    data_BLASTFUR_inputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-'value'),

    data_BLASTFUR_use %>%
      select(-'product'),

    c('iso3c', 'year')
  ) %>%
    # assume that countries/years that have no inputs into blast furnaces,
    # also have no outputs and use of blast furnace products (e.g. ISR 1973)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid blast furnace replacement data') %>%
    mutate(value = .data$value * .data$factor) %>%
    group_by(!!!syms(c('iso3c', 'year', 'product', 'flow'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  ## 1.7 Calculate BF Losses ----
  # blast furnace losses (true losses from EBLASTFUR and transformation energy
  # from TBLASTFUR) are allotted to the IRONSTL sector
  # losses are the difference of inputs and outputs, weighted by input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products

  # Example calculation of transformation losses:
  # BF outputs: BLFURGAS.MAINELEC = -20
  # BF inputs: COKCOAL.TBLASTFUR = -90, ELECTR.EBLASTFUR = -10
  # Transformation losses are calculated as difference between
  # inputs and outputs that are attributed to inputs by input shares:
  # Blast furnace energy losses:
  # COKCOAL.IRONSTL = 100 - 20 * (90 / 100) = 82
  # ELECTR.IRONSTL = 10 - 20 * (10 / 100) = 8
  # Note blast furnace losses are attributed to IRONSTL

  data_BLASTFUR_loss <- right_join(
    data_BLASTFUR_inputs,

    data_BLASTFUR_outputs %>%
      group_by(!!!syms(c('iso3c', 'year'))) %>%
      summarise(output = sum(.data$value), .groups = 'drop'),

    c('iso3c', 'year')
  ) %>%
    # assume that countries/years that have no inputs into blast furnaces,
    # also have no transformation losses
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = 'Only valid blast furnace loss data') %>%
    group_by(!!!syms(c('iso3c', 'year'))) %>%
    mutate(value = (sum(-.data$value) - .data$output)
           * .data$value / sum(.data$value),
           flow = 'IRONSTL') %>%
    ungroup() %>%
    select('iso3c', 'year', 'product', 'flow', 'value')

  ## 1.8 Replace IEA data with steel sector adjustments ----

  # bind all data of coke oven and blast furnace adjustment routine together
  df_CO_BF_adjustment <-  bind_rows(
    # filter already replaced data
    data_COKEOVS_replacement %>%
      filter(!.data$flow %in% c('EBLASTFUR', 'TBLASTFUR')),

    data_BLASTFUR_replacement %>%
      filter(!.data$flow %in% c('ECOKEOVS', 'TCOKEOVS')),

    data_COKEOVS_loss %>%
      sum_total_('product', name = 'TOTAL'),

    data_BLASTFUR_loss %>%
      sum_total_('product', name = 'TOTAL')
  ) %>%
    group_by(!!!syms(setdiff(colnames(.), 'value'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop')

  # take original IEA data and subtract flows that contain CO or BF outputs
  # these flows are now accounted for in the CO/BF adjusted data df_CO_BF_adjustment

  subtract <- bind_rows(data_BLASTFUR_use, data_COKEOVS_use) %>%
    filter(.data$value != 0) %>%
    as.magpie()
  subtract[is.na(subtract)] <- 0

  data[getRegions(subtract), getYears(subtract), getNames(subtract)] <-
    data[getRegions(subtract), getYears(subtract), getNames(subtract)] - subtract

  # add flows from CO/BF adjustment routine df_CO_BF_adjustment

  replace <- df_CO_BF_adjustment %>%
    filter(.data$value != 0) %>%
    as.magpie()

  replace[is.na(replace)] <- 0
  newProductFlow <- setdiff(getNames(replace), getNames(data))
  data <- add_columns(data, addnm = newProductFlow, dim = 3, fill = 0)

  data[getRegions(replace), getYears(replace), getNames(replace)] <-
    data[getRegions(replace), getYears(replace), getNames(replace)] + replace

  # set coke oven and blast furnace flows to zero
  # as the energy is already accounted for in others flows
  data[, , c("ECOKEOVS", "TCOKEOVS", "EBLASTFUR", "TBLASTFUR")] <- 0

  # recalculate summary flows after CO+BF adjustment
  # define additional summary flows to be recalculated
  additional_summary_flows <- tribble(
    ~summary.flow,       ~flow,
    # Manufacturing Industry
    'MANUFACT',          'IRONSTL',    # iron and steel
    'MANUFACT',          'CHEMICAL',   # chemical and petrochemical
    'MANUFACT',          'NONFERR',    # non-ferrous metals
    'MANUFACT',          'NONMET',     # non-metallic minerals
    'MANUFACT',          'TRANSEQ',    # transport equipment
    'MANUFACT',          'MACHINE',    # machinery
    'MANUFACT',          'FOODPRO',    # food production
    'MANUFACT',          'PAPERPRO',   # paper, pulp, and print
    'MANUFACT',          'WOODPRO',    # wood and wood products
    'MANUFACT',          'TEXTILES'    # textiles
  )

  IEA_flows <- IEA_flows %>%
    filter(!is.na(.data$summary.flow)) %>%
    rbind(additional_summary_flows)

  # sum all flows after CO+BF adjustment to get summary flows as defined in
  # IEA_flows table above

  sumFlows <- data %>%
    mselect(FLOW = unique(IEA_flows$flow)) %>%
    .clean_data() %>%
    inner_join(IEA_flows, by = 'flow') %>%
    group_by(!!!syms(c('iso3c', 'year', 'product', 'summary.flow'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop') %>%
    ungroup() %>%
    rename(flow = 'summary.flow') %>%
    as.magpie()

  sumFlows <- add_columns(sumFlows,
                          addnm = setdiff(getRegions(data), getRegions(sumFlows)),
                          dim = 1)
  sumFlows <- add_columns(sumFlows,
                          addnm = setdiff(getYears(data), getYears(sumFlows)),
                          dim = 2)

  # replace summary flows with sums of adjusted CO+BF data
  data <- data[, , intersect(getNames(data, dim = "FLOW"),
                             IEA_flows$summary.flow), invert = TRUE]
  data <- mbind(data, sumFlows)

  data[is.na(data)] <- 0

  if (!fixing) {
    return(data)
  }

  # 2. Prepare Industry Subsector Time Series ----

  ## 2.1 Define flows and mappings ----

  # all industry subsector flows
  flows_to_fix <- c('IRONSTL', 'CHEMICAL', 'NONFERR', 'NONMET', 'TRANSEQ',
                    'MACHINE','MINING', 'FOODPRO', 'PAPERPRO', 'WOODPRO',
                    'CONSTRUC', 'TEXTILES')

  # all products associated with those flows

  # these used to be retrieved from the iea mapping before
  # products_to_fix <- ieamatch %>%
  #   filter(.data$iea_flows %in% flows_to_fix) %>%
  #   getElement('iea_product') %>%
  #   unique()

  # now, we hard code them, as the mapping is being refactored, potentially causing
  # unwanted side effects in this tool function

  # for now, we use the products associated with the flows, according to the old mapping
  # TODO: revise and update the list of products to be fixed
  products_to_fix <- c(
    'ADDITIVE', 'ANTCOAL', 'AVGAS', 'BIODIESEL', 'BIOGASES', 'BIOGASOL', 'BITCOAL', 'BITUMEN',
    'BKB', 'BROWN', 'CHARCOAL', 'COKCOAL', 'CRNGFEED', 'CRUDEOIL', 'ELECTR', 'ETHANE',
    'GASCOKE', 'GASWKSGS', 'GEOTHERM', 'HARDCOAL', 'HEAT', 'INDWASTE', 'JETGAS', 'LIGNITE',
    'LPG', 'LUBRIC', 'MUNWASTEN', 'MUNWASTER', 'NAPHTHA', 'NATGAS', 'NGL', 'NONBIODIES',
    'NONBIOGASO', 'NONBIOJETK', 'OBIOLIQ', 'OILSHALE', 'ONONSPEC', 'OTHKERO', 'PARWAX', 'PATFUEL',
    'PEAT', 'PEATPROD', 'PETCOKE', 'PRIMSBIO', 'REFFEEDS', 'REFINGAS', 'RENEWNS', 'RESFUEL',
    'SUBCOAL', 'WHITESP'
  )

  # products to fix according to the latest mapping
  # products_to_fix <- c(
  #   'ANTCOAL', 'AVGAS', 'BIODIESEL', 'BIOGASES', 'BIOGASOL', 'BITCOAL', 'BITUMEN', 'BKB',
  #   'BROWN', 'CHARCOAL', 'COKCOAL', 'CRNGFEED', 'CRUDEOIL', 'ELECTR', 'ETHANE', 'GASCOKE',
  #   'GASWKSGS', 'GEOTHERM', 'HARDCOAL', 'HEAT', 'INDWASTE', 'LIGNITE', 'LPG', 'LUBRIC',
  #   'MUNWASTEN', 'MUNWASTER', 'NAPHTHA', 'NATGAS', 'NGL', 'NONBIODIES', 'NONBIOGASO', 'NONBIOJETK',
  #   'OBIOLIQ', 'OILSHALE', 'ONONSPEC', 'OTHKERO', 'PARWAX', 'PATFUEL', 'PEAT', 'PEATPROD',
  #   'PETCOKE', 'PRIMSBIO', 'REFINGAS', 'RENEWNS', 'RESFUEL', 'SUBCOAL', 'WHITESP'
  # )

  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional',
                                   where = 'mappingfolder') %>%
    as_tibble() %>%
    select('iso3c' = 'CountryCode', 'region' = 'RegionCode')

  ## 2.2 Extend industry subsector time series ----
  # subset of data containing industry subsector products and flows

  product_flow_to_fix <- cartesian(products_to_fix,
                                   c(flows_to_fix, 'TOTIND', 'INONSPEC'))
  data_industry <- data[, , intersect(getNames(data), product_flow_to_fix)] %>%
    .clean_data() %>%
    inner_join(region_mapping, 'iso3c')

  ## 2.3 Apply five-year moving average ----
  data_industry <- data_industry %>%
    group_by(.data$iso3c, .data$region, .data$product, .data$flow) %>%
    arrange(.data$year) %>%
    mutate(value = zoo::rollapply(
      # pad data with two leading and trailing NAs
      data = c(NA, NA, .data$value, NA, NA),
      width = 5,
      # ignoring NAs in mean() stumps the mean on the edges to four/three years
      FUN = function(x) { mean(x, na.rm = TRUE) })) %>%
    ungroup()

  # 3. Fix suspicious products in industry ----

  ## 3.1 Prepare data to fix ----

  # all products that use less then 1 % of total energy outside of non-specified
  # industry are 'suspicious' and will be fixed
  data_to_fix <- data_industry %>%
    filter(.data$flow %in% c('TOTIND', 'INONSPEC')) %>%
    spread(.data$flow, .data$value) %>%
    filter(1 - .data$INONSPEC / .data$TOTIND < 1e-2) %>%
    select('iso3c', 'region', 'year', 'product', 'TOTIND')

  # use all non-suspicious data to calculate regional and global averages
  data_for_fixing <- anti_join(
    data_industry %>%
      filter('TOTIND' != .data$flow),

    data_to_fix %>%
      select(-.data$TOTIND),

    c('iso3c', 'region', 'year', 'product')
  ) %>%
    as_tibble()

  data_for_fixing <- full_join(
    # compute global averages
    data_for_fixing %>%
      group_by(.data$year, .data$product, .data$flow) %>%
      summarise(value = sum(.data$value), .groups = 'drop_last') %>%
      mutate(global_share = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-.data$value) %>%
      # and expand to all regions
      mutate(region = NA_character_) %>%
      complete(nesting(!!!syms(c('year', 'product', 'flow', 'global_share'))),
               region = unique(region_mapping$region)),

    # compute regional averages
    data_for_fixing %>%
      group_by(.data$year, .data$region, .data$product, .data$flow) %>%
      summarise(value = sum(.data$value), .groups = 'drop_last') %>%
      mutate(regional_share = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-.data$value),

    c('year', 'region', 'product', 'flow')
  ) %>%
    # use regional averages if available, global averages otherwise
    mutate(value = ifelse(!is.na(.data$regional_share), .data$regional_share,
                          .data$global_share)) %>%
    select(-.data$regional_share, -.data$global_share) %>%
    interpolate_missing_periods_(
      periods = list(year = sub('^y([0-9]{4})$', '\\1', getYears(data)) %>%
                       as.integer() %>%
                       sort()),
      expand.values = TRUE, method = 'linear')

  # calculated fixed data
  data_industry_fixed <- left_join(
    data_to_fix,
    data_for_fixing,
    c('region', 'year', 'product')
  ) %>%
    # replace "suspicious" data with averages
    mutate(value = .data$TOTIND * .data$value) %>%
    select(.data$iso3c, .data$region, .data$year, .data$product, .data$flow,
           .data$value) %>%
    assert(not_na, .data$value) %>%
    overwrite(data_industry)

  ## 3.2 Redistribute products to industry-related flows ----
  # redistribute at least <threshold> of each product into each subsector
  data_industry_fixed <- data_industry_fixed %>%
    complete(nesting(!!!syms(c('iso3c', 'region', 'year', 'product'))),
             flow = c(flows_to_fix, 'INONSPEC'),
             fill = list(value = 0)) %>%
    group_by(.data$iso3c, .data$year, .data$product) %>%
    # which flow belongs to which subsector?
    right_join(
      tribble(
        ~subsector,    ~flow,
        'cement',      'NONMET',
        'chemicals',   'CHEMICAL',
        'steel',       'IRONSTL') %>%
        complete(flow = c(flows_to_fix, 'INONSPEC'),
                 fill = list(subsector = 'otherInd')),

      'flow'
    ) %>%
    # compute subsector totals
    group_by(.data$subsector, .add = TRUE) %>%
    mutate(subsector.total = sum(.data$value),
           subsector.count = n()) %>%
    ungroup(.data$subsector) %>%
    mutate(
      # each subsector consumes at least <threshold> of the total consumption of
      # each product (with the exception of heat, which is only consumed in the
      #  otherInd subsector)
      subsector.min = ifelse('HEAT' == .data$product, 0,
                             threshold * sum(.data$value)),
      # if total subsector consumption is below the minimum, consumption must be
      # added
      subsector.add = pmax(0, .data$subsector.min - .data$subsector.total),
      # each flow gets consumption added according to its share in total
      # subsector consumption
      flow.add = ifelse(0 != .data$subsector.total,
                        ( .data$subsector.add
                          / .data$subsector.total
                          * .data$value
                        ),
                        .data$subsector.add / .data$subsector.count),
      # if the additional flow is zero, consumption has to be subtracted from\
      # this flow, in relation to its share of all flows with
      # more-than-threshold consumption
      flow.add = ifelse(0 != .data$flow.add, .data$flow.add,
                        ( -sum(.data$flow.add)
                          * .data$value
                          / sum(.data$value[0 == .data$flow.add])
                        )),
      value = .data$value + .data$flow.add) %>%
    ungroup() %>%
    select('iso3c', 'region', 'year', 'product', 'flow', 'value')

  ## 3.3 Replace and append fixed data ----
  data_industry_fixed_overwrite <- data_industry_fixed %>%
    semi_join(
      data_industry,

      c('iso3c', 'region', 'year', 'product', 'flow')
    ) %>%
    select('iso3c', 'year', 'product', 'flow', 'value') %>%
    as.magpie(spatial = 1, temporal = 2, datacol = 5)

  data_industry_fixed_append <- data_industry_fixed %>%
    anti_join(
      data_industry,

      c('iso3c', 'region', 'year', 'product', 'flow')
    ) %>%
    select('iso3c', 'year', 'product', 'flow', 'value') %>%
    complete(nesting(!!!syms(c('product', 'flow'))),
             iso3c = getRegions(data),
             year = getYears(data, as.integer = TRUE),
             fill = list(value = 0)) %>%
    select('iso3c', 'year', 'product', 'flow', 'value') %>%
    as.magpie(spatial = 1, temporal = 2, datacol = 5)

  data[getRegions(data_industry_fixed_overwrite),
       getYears(data_industry_fixed_overwrite),
       getNames(data_industry_fixed_overwrite)] <- data_industry_fixed_overwrite


  existing_names <- intersect(getNames(data), getNames(data_industry_fixed_append))
  added_names <- setdiff(getNames(data_industry_fixed_append), getNames(data))
  tmp <- data[, , existing_names]
  tmp[is.na(tmp)] <- 0
  data[, , existing_names] <- tmp + data_industry_fixed_append[, , existing_names]

  data <- mbind(data, data_industry_fixed_append[, , added_names])

  return(data)
}
