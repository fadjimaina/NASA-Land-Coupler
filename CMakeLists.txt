cmake_minimum_required(VERSION 3.15.5)

foreach(env_var IN ITEMS CMAKE_C_COMPILER CMAKE_CXX_COMPILER CMAKE_Fortran_COMPILER)
  if(NOT DEFINED ENV{${env_var}})
    message(FATAL_ERROR "${env_var} is not defined")
  endif()
endforeach()

set(CMAKE_C_COMPILER $ENV{CMAKE_C_COMPILER})
set(CMAKE_CXX_COMPILER $ENV{CMAKE_CXX_COMPILER})
set(CMAKE_Fortran_COMPILER $ENV{CMAKE_Fortran_COMPILER})
set(CMAKE_Platform $ENV{CMAKE_Platform})
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

project(NASA-Land-Coupler
        VERSION 1.0
        LANGUAGES Fortran)

include(ExternalProject)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)

find_package(ESMF MODULE REQUIRED)
if (NOT ESMF_FOUND)
  message(FATAL_ERROR "ESMF library not found. Please set environment variable ESMFMKFILE.")
endif (NOT ESMF_FOUND)

list(APPEND nlc_app_files
  src/driver/app.F90
  src/driver/driver.F90
  src/driver/fields.F90
  src/driver/mediator.F90
)

add_executable(NLC.exe ${nlc_app_files})
target_link_libraries(NLC.exe PUBLIC ESMF)
install(TARGETS NLC.exe DESTINATION .)
list(APPEND nlc_defs_private ESMF_VERSION_MAJOR=${ESMF_VERSION_MAJOR})
target_compile_definitions(NLC.exe PRIVATE "${nlc_defs_private}")

if (COMPONENT_LIST)
  string(REPLACE "," ";" COMPONENT_LIST "${COMPONENT_LIST}")
  foreach (component IN ITEMS ${COMPONENT_LIST})
    string(TOUPPER ${component} component)
    set(ENABLE_${component} ON)
  endforeach()
else()
  set(ENABLE_LIS ON)
  set(ENABLE_WRFHYDRO ON)
  set(ENABLE_PARFLOW ON)
endif()

if (ENABLE_LIS)
  set(NLC_LIS_INST $ENV{NLC_LIS_INST} CACHE FILEPATH "Path to NLC_LIS_INST")
  message("Build LIS:")
  message("  run: ${CMAKE_BUILD_TOOL} nuopcinstall INSTPATH=${NLC_LIS_INST}")
  message("   in: ${CMAKE_CURRENT_SOURCE_DIR}/src/LISF/lis/runmodes/nuopc_cpl_mode")
  message("")

  if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/src/LISF/lis/make/configure.lis")
    message(FATAL_ERROR "LIS configuration missing.")
  endif()

  add_custom_target(lis_nuopc
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/LISF/lis/runmodes/nuopc_cpl_mode"
    COMMAND ${CMAKE_BUILD_TOOL} nuopcinstall INSTPATH=${NLC_LIS_INST})

  add_dependencies(NLC.exe lis_nuopc)
  target_link_libraries(NLC.exe PUBLIC ${NLC_LIS_INST}/liblis_nuopc.a)
  target_include_directories(NLC.exe PUBLIC ${NLC_LIS_INST})
endif (ENABLE_LIS)

if (ENABLE_WRFHYDRO)
  set(NLC_WRFHYDRO_INST $ENV{NLC_WRFHYDRO_INST} CACHE FILEPATH "Path to NLC_WRFHYDRO_INST")
  message("Build WRFHYDRO:")
  message("  run: ${CMAKE_BUILD_TOOL} nuopcinstall INSTPATH=${NLC_WRFHYDRO_INST}")
  message("   in: ${CMAKE_CURRENT_SOURCE_DIR}/src/wrf_hydro_nwm/trunk/NDHMS/CPL/NUOPC_cpl")
  message("")

  if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/src/wrf_hydro_nwm/trunk/NDHMS/macros")
    message(FATAL_ERROR "WRFHYDRO configuration missing.")
  endif()

  add_custom_target(wrfhydro_nuopc
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/wrf_hydro_nwm/trunk/NDHMS/CPL/NUOPC_cpl"
    COMMAND ${CMAKE_BUILD_TOOL} nuopcinstall INSTPATH=${NLC_WRFHYDRO_INST})

  add_dependencies(NLC.exe wrfhydro_nuopc)
  target_link_libraries(NLC.exe PUBLIC ${NLC_WRFHYDRO_INST}/libwrfhydro_nuopc.a)
  target_include_directories(NLC.exe PUBLIC ${NLC_WRFHYDRO_INST})  
endif (ENABLE_WRFHYDRO)

if (ENABLE_PARFLOW)
  set(NLC_PARFLOW_INST $ENV{NLC_PARFLOW_INST} CACHE FILEPATH "Path to NLC_PARFLOW_INST")
  message("Build PARFLOW:")
  message("  run: ${CMAKE_BUILD_TOOL} INSTPATH=${NLC_PARFLOW_INST}")
  message("   in: ${CMAKE_CURRENT_SOURCE_DIR}/src/parflow")
  message("")

  set(PARFLOW_AMPS_LAYER $ENV{PARFLOW_AMPS_LAYER} CACHE FILEPATH "Setting for PARFLOW_AMPS_LAYER")
  set(PARFLOW_HAVE_CLM $ENV{PARFLOW_HAVE_CLM} CACHE FILEPATH "Setting for PARFLOW_HAVE_CLM")
  set(PARFLOW_HYPRE_DIR $ENV{PARFLOW_HYPRE_DIR} CACHE FILEPATH "Path to PARFLOW_HYPRE_DIR")
  set(PARFLOW_HDF5_DIR $ENV{PARFLOW_HDF5_DIR} CACHE FILEPATH "Path to PARFLOW_HDF5_DIR")
  set(PARFLOW_SILO_DIR $ENV{PARFLOW_SILO_DIR} CACHE FILEPATH "Path to PARFLOW_SILO_DIR")

  ExternalProject_Add(parflow
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}/parflow
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/parflow
    INSTALL_DIR ${NLC_PARFLOW_INST}
    CMAKE_ARGS -DPARFLOW_AMPS_LAYER=${PARFLOW_AMPS_LAYER}
               -DHYPRE_ROOT=${PARFLOW_HYPRE_DIR}
               -DHDF5_ROOT=${PARFLOW_HDF5_DIR}
               -DSILO_ROOT=${PARFLOW_SILO_DIR}
               -DPARFLOW_HAVE_CLM=${PARFLOW_HAVE_CLM}
               -DCMAKE_INSTALL_PREFIX=${NLC_PARFLOW_INST}
               -DPARFLOW_ENABLE_NUOPC=ON
               -DCMAKE_BUILD_TYPE=DEBUG
    BUILD_ALWAYS TRUE
  )

  add_dependencies(NLC.exe parflow)
endif (ENABLE_PARFLOW)

# find LIS dependencies
find_package(LISDEPS MODULE REQUIRED)
if (JASPER_FOUND) # JASPER
  target_link_libraries(NLC.exe PUBLIC JASPER::JASPER)
endif ()
if (OPENJPEG_FOUND) # OPENJPEG
  target_link_libraries(NLC.exe PUBLIC OPENJPEG::OPENJPEG)
endif ()
if (GRIBAPI_FOUND) # GRIBAPI
  target_link_libraries(NLC.exe PUBLIC GRIBAPI::GRIBAPI)
  target_link_libraries(NLC.exe PUBLIC GRIBAPI::GRIBAPI_F90)
elseif (ECCODES_FOUND) # ECCODES
  target_link_libraries(NLC.exe PUBLIC ECCODES::ECCODES)
  target_link_libraries(NLC.exe PUBLIC ECCODES::ECCODES_F90)
endif ()
if (HDF4_FOUND) # HDF4
  target_link_libraries(NLC.exe PUBLIC HDF4::MFHDF)
  target_link_libraries(NLC.exe PUBLIC HDF4::DF)
endif ()
if (HDF5_FOUND) # HDF5
  target_link_libraries(NLC.exe PUBLIC HDF5::HDF5)
  target_link_libraries(NLC.exe PUBLIC HDF5::HDF5_FORTRAN)
  target_link_libraries(NLC.exe PUBLIC HDF5::HDF5_HL)
endif ()
if (HDFEOS_FOUND) # HDFEOS
  target_link_libraries(NLC.exe PUBLIC HDFEOS::HDFEOS)
  target_link_libraries(NLC.exe PUBLIC HDFEOS::GCTP)
endif ()
if (NETCDF_FOUND) # NETCDF
  target_link_libraries(NLC.exe PUBLIC NETCDF::NETCDF)
  target_link_libraries(NLC.exe PUBLIC NETCDF::NETCDFF)
endif ()

