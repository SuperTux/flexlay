# SWIG Ruby extension: flexlay_wrap

set_property(SOURCE ${CMAKE_SOURCE_DIR}/lib/flexlay_wrap.i PROPERTY CPLUSPLUS ON)
set_property(SOURCE ${CMAKE_SOURCE_DIR}/lib/flexlay_wrap.i PROPERTY SWIG_FLAGS "-ruby")

swig_add_library(flexlay_wrap
  TYPE MODULE
  LANGUAGE ruby
  SOURCES
    ${CMAKE_SOURCE_DIR}/lib/flexlay_wrap.i
    ruby_functor.cpp
    ruby_meta_data.cpp
    ruby_sexpr_parser.cpp
    ruby_object.cpp
    ruby_converter.cpp
)

set_target_properties(flexlay_wrap PROPERTIES
  OUTPUT_NAME "flexlay_wrap"
  PREFIX ""
)

target_include_directories(flexlay_wrap PRIVATE
  ${CMAKE_SOURCE_DIR}/lib
  ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(flexlay_wrap PRIVATE
  Flexlay::flexlay
  PkgConfig::RUBY
  PkgConfig::CLANLIB
)

target_compile_options(flexlay_wrap PRIVATE
  -Wno-uninitialized
  -Wno-unused-variable
  -Wno-unused-but-set-variable
)

install(TARGETS flexlay_wrap
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/flexlay
  RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/flexlay
)
