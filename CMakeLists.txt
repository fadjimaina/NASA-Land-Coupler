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

if (DEFINED ENV{NLC_LIS_INST} AND NOT DEFINED NLC_LIS_INST)
  set(NLC_LIS_INST $ENV{NLC_LIS_INST} CACHE FILEPATH "Path to NLC_LIS_INST")
endif ()
if (DEFINED ENV{NLC_WRFHYDRO_INST} AND NOT DEFINED NLC_WRFHYDRO_INST)
  set(NLC_WRFHYDRO_INST $ENV{NLC_WRFHYDRO_INST} CACHE FILEPATH "Path to NLC_WRFHYDRO_INST")
endif ()


message("Build LIS:")
message("  run: ${CMAKE_BUILD_TOOL} make nuopcinstall INSTPATH=${NLC_LIS_INST}")
message("   in: ${CMAKE_CURRENT_SOURCE_DIR}/src/LISF/lis/runmodes/nuopc_cpl_mode")
message("")

add_custom_target(lis_nuopc
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/LISF/lis/runmodes/nuopc_cpl_mode"
  COMMAND ${CMAKE_BUILD_TOOL} nuopcinstall INSTPATH=${NLC_LIS_INST})

message("Build WRFHYDRO:")
message("  run: ${CMAKE_BUILD_TOOL} make nuopcinstall INSTPATH=${NLC_WRFHYDRO_INST}")
message("   in: ${CMAKE_CURRENT_SOURCE_DIR}/src/wrf_hydro_nwm/trunk/NDHMS/CPL/NUOPC_cpl")
message("")

add_custom_target(wrfhydro_nuopc
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/wrf_hydro_nwm/trunk/NDHMS/CPL/NUOPC_cpl"
  COMMAND ${CMAKE_BUILD_TOOL} nuopcinstall INSTPATH=${NLC_WRFHYDRO_INST})

add_executable(NLC.exe ${nlc_app_files})
target_link_libraries(NLC.exe PUBLIC ESMF)
add_dependencies(NLC.exe lis_nuopc)
add_dependencies(NLC.exe wrfhydro_nuopc)
target_link_libraries(NLC.exe PUBLIC ${NLC_LIS_INST}/liblis_nuopc.a)
target_link_libraries(NLC.exe PUBLIC ${NLC_WRFHYDRO_INST}/libwrfhydro_nuopc.a)
target_include_directories(NLC.exe PUBLIC ${NLC_LIS_INST})
target_include_directories(NLC.exe PUBLIC ${NLC_WRFHYDRO_INST})
install(TARGETS NLC.exe DESTINATION .)
list(APPEND nlc_defs_private ESMF_VERSION_MAJOR=${ESMF_VERSION_MAJOR})
target_compile_definitions(NLC.exe PRIVATE "${nlc_defs_private}")

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

