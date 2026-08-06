cmake_minimum_required(VERSION 3.16)
project(flexlay-classic VERSION 0.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

option(FLEXLAY_BUILD_NETPANZER "Build netPanzer Ruby extension" ON)

# ── Dependencies ──────────────────────────────────────────────────────────
find_package(PkgConfig REQUIRED)
find_package(SWIG REQUIRED)
include(UseSWIG)

pkg_check_modules(CLANLIB REQUIRED IMPORTED_TARGET
  clanCore-1.0
  clanDisplay-1.0
  clanGL-1.0
  clanSignals-1.0
  clanGUI-1.0
  clanGUIStyleSilver-1.0
)

# Ruby: prefer pkg-config (works with versioned names on Nix).
set(_ruby_pkg_candidates ruby ruby-3.4 ruby-3.3 ruby-3.2 ruby-3.1 ruby-3.0 ruby-2.7)
if(DEFINED ENV{RUBY_PKG})
  list(INSERT _ruby_pkg_candidates 0 $ENV{RUBY_PKG})
endif()
set(_ruby_found FALSE)
foreach(_pkg IN LISTS _ruby_pkg_candidates)
  pkg_check_modules(RUBY IMPORTED_TARGET ${_pkg})
  if(RUBY_FOUND)
    message(STATUS "Using Ruby pkg-config package: ${_pkg}")
    set(_ruby_found TRUE)
    break()
  endif()
endforeach()
if(NOT _ruby_found)
  message(FATAL_ERROR "No usable Ruby pkg-config package found (tried ${_ruby_pkg_candidates})")
endif()

# Interpreter used by generated launcher scripts (match the extension ABI).
find_program(RUBY_EXECUTABLE NAMES ruby REQUIRED)

# ── Subdirs ───────────────────────────────────────────────────────────────
add_subdirectory(lib)
add_subdirectory(ruby)
if(FLEXLAY_BUILD_NETPANZER)
  add_subdirectory(netpanzer)
endif()

# ── Install shared data ───────────────────────────────────────────────────
include(GNUInstallDirs)

install(DIRECTORY data/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/flexlay
  FILES_MATCHING
    PATTERN "*.rb"
    PATTERN "*.png"
    PATTERN "*.tga"
    PATTERN "*.xml"
)

install(DIRECTORY ruby/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/flexlay
  FILES_MATCHING PATTERN "*.rb"
)

install(DIRECTORY paint/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/flexlay-paint
  FILES_MATCHING PATTERN "*.rb"
)

install(DIRECTORY netpanzer/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/netpanzer-editor
  FILES_MATCHING
    PATTERN "*.rb"
    PATTERN "*.png"
    PATTERN "*.xml"
)

install(DIRECTORY supertux/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/supertux-editor
  FILES_MATCHING
    PATTERN "*.rb"
    PATTERN "*.png"
    PATTERN "*.xml"
)

install(DIRECTORY supertux-portable/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/supertux-portable
  FILES_MATCHING
    PATTERN "*.rb"
    PATTERN "*.png"
    PATTERN "*.xml"
)

# ── Launcher scripts ──────────────────────────────────────────────────────
# Paths baked in at install time (like the old Makefile).
set(FLEXLAY_DATADIR        "${CMAKE_INSTALL_FULL_DATADIR}/flexlay")
set(FLEXLAY_PAINT_DATADIR  "${CMAKE_INSTALL_FULL_DATADIR}/flexlay-paint")
set(NETPANZER_EDITOR_DATADIR "${CMAKE_INSTALL_FULL_DATADIR}/netpanzer-editor")
set(SUPERTUX_EDITOR_DATADIR  "${CMAKE_INSTALL_FULL_DATADIR}/supertux-editor")
set(SUPERTUX_PORTABLE_DATADIR "${CMAKE_INSTALL_FULL_DATADIR}/supertux-portable")
set(FLEXLAY_LIBDIR         "${CMAKE_INSTALL_FULL_LIBDIR}/flexlay")

function(flexlay_write_launcher name datadir_var script_relpath)
  set(_script "${CMAKE_CURRENT_BINARY_DIR}/launcher-${name}")
  file(WRITE "${_script}" "#!/bin/sh\n")
  file(APPEND "${_script}" "FLEXLAY_DATADIR=\"${FLEXLAY_DATADIR}\"\nexport FLEXLAY_DATADIR\n")
  if(NOT "${datadir_var}" STREQUAL "")
    file(APPEND "${_script}" "${datadir_var}=\"${${datadir_var}}\"\nexport ${datadir_var}\n")
  endif()
  if("${name}" STREQUAL "netpanzer-editor")
    file(APPEND "${_script}" "NETPANZER_DATADIR=\"\${NETPANZER_DATADIR:-/usr/share/games/netpanzer/}\"\nexport NETPANZER_DATADIR\n")
  endif()
  file(APPEND "${_script}" "LD_LIBRARY_PATH=\"\${LD_LIBRARY_PATH}:${FLEXLAY_LIBDIR}\"\nexport LD_LIBRARY_PATH\n")
  file(APPEND "${_script}" "RUBYLIB=\"\${RUBYLIB}:${FLEXLAY_DATADIR}:${FLEXLAY_LIBDIR}")
  if(NOT "${datadir_var}" STREQUAL "")
    file(APPEND "${_script}" ":${${datadir_var}}")
  endif()
  file(APPEND "${_script}" "\"\nexport RUBYLIB\n")
  file(APPEND "${_script}" "exec ${RUBY_EXECUTABLE} -w \"${${datadir_var}}/${script_relpath}\" \"\$@\"\n")
  install(PROGRAMS "${_script}" DESTINATION ${CMAKE_INSTALL_BINDIR} RENAME ${name})
endfunction()

flexlay_write_launcher(flexlay-paint FLEXLAY_PAINT_DATADIR "paint.rb")
flexlay_write_launcher(netpanzer-editor NETPANZER_EDITOR_DATADIR "netpanzer.rb")
flexlay_write_launcher(supertux-editor SUPERTUX_EDITOR_DATADIR "supertux.rb")
flexlay_write_launcher(supertux-portable SUPERTUX_PORTABLE_DATADIR "supertux-portable.rb")
