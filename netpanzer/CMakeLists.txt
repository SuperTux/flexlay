# SWIG Ruby extension: netpanzer_wrap

set_property(SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/netpanzer.i PROPERTY CPLUSPLUS ON)
# Do NOT set SWIG_FLAGS "-ruby": swig_add_library(... LANGUAGE ruby) already
# passes -ruby. Specifying it again triggers:
#   Only one target language can be supported at a time (both -ruby and -ruby were specified).

swig_add_library(netpanzer_wrap
  TYPE MODULE
  LANGUAGE ruby
  SOURCES
    netpanzer.i
    netpanzer.cpp
)

set_target_properties(netpanzer_wrap PROPERTIES
  OUTPUT_NAME "netpanzer_wrap"
  PREFIX ""
)

target_include_directories(netpanzer_wrap PRIVATE
  ${CMAKE_SOURCE_DIR}/lib
  ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(netpanzer_wrap PRIVATE
  Flexlay::flexlay
  PkgConfig::RUBY
  PkgConfig::CLANLIB
)

target_compile_options(netpanzer_wrap PRIVATE
  -Wno-uninitialized
  -Wno-unused-variable
  -Wno-unused-but-set-variable
)

install(TARGETS netpanzer_wrap
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/flexlay
  RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/flexlay
)
