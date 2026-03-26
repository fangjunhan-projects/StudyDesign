server <- function(input, output, session) {

  mod_ia_boundary_server("ia_boundary")
  mod_pfs_raw_server("pfs_raw")
  mod_pfs_sdtm_server("pfs_sdtm")
  mod_pfs_adam_compare_server("pfs_adam_compare")
  mod_event_projection_server("event_projection")

}
