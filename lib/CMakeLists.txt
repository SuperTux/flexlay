# Core Flexlay static library

file(GLOB_RECURSE FLEXLAY_SOURCES CONFIGURE_DEPENDS
  "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/gui/*.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/tools/*.cpp"
)

# simpleed is a standalone demo, not part of the library
list(FILTER FLEXLAY_SOURCES EXCLUDE REGEX ".*/simpleed\\.cpp$")

add_library(flexlay STATIC ${FLEXLAY_SOURCES})
add_library(Flexlay::flexlay ALIAS flexlay)

target_include_directories(flexlay
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/flexlay>
)

target_link_libraries(flexlay PUBLIC PkgConfig::CLANLIB)

target_compile_options(flexlay PRIVATE -Wall)

install(TARGETS flexlay
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

file(GLOB FLEXLAY_HEADERS CONFIGURE_DEPENDS
  "${CMAKE_CURRENT_SOURCE_DIR}/*.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/*.hxx"
)
install(FILES ${FLEXLAY_HEADERS}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/flexlay
)
