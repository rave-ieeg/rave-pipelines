

module_server <- function(input, output, session, tools, ...) {

  server_step1(input, output, session, environment(), ...)

  server_step2(input, output, session, environment(), ...)

}
