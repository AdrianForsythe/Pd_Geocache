#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "PROJ4::proj" for configuration "Release"
set_property(TARGET PROJ4::proj APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(PROJ4::proj PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libproj.so.25.9.0.1"
  IMPORTED_SONAME_RELEASE "libproj.so.25"
  )

list(APPEND _IMPORT_CHECK_TARGETS PROJ4::proj )
list(APPEND _IMPORT_CHECK_FILES_FOR_PROJ4::proj "${_IMPORT_PREFIX}/lib/libproj.so.25.9.0.1" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
