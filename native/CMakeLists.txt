cmake_minimum_required(VERSION 3.16)
project(livecode_link_bridge LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(ABLETON_LINK_DIR "" CACHE PATH "Path to a local checkout of the Ableton Link SDK")

if(ABLETON_LINK_DIR)
  include("${ABLETON_LINK_DIR}/AbletonLinkConfig.cmake")
else()
  include(FetchContent)
  FetchContent_Declare(
    AbletonLink
    GIT_REPOSITORY https://github.com/Ableton/link.git
    GIT_TAG master
    GIT_SUBMODULES_RECURSE TRUE)
  FetchContent_GetProperties(AbletonLink)
  if(NOT abletonlink_POPULATED)
    FetchContent_Populate(AbletonLink)
  endif()
  include("${abletonlink_SOURCE_DIR}/AbletonLinkConfig.cmake")
endif()

add_library(livecode-link SHARED livecode-link-bridge.cpp)
target_link_libraries(livecode-link PRIVATE Ableton::Link)
set_target_properties(livecode-link PROPERTIES
  OUTPUT_NAME "livecode-link")
