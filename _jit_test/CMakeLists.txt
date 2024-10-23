cmake_minimum_required(VERSION 3.13.4)
project(CXXJITTest VERSION "0.1" LANGUAGES C CXX)

option(ENABLE_SANITIZER "Enable Address sanitizer" OFF)

if((CMAKE_CXX_COMPILER_ID STREQUAL "Clang") OR(CMAKE_CXX_COMPILER_ID STREQUAL "GNU"))
  add_compile_options(

    # -v
    -fvisibility=hidden
    -Wall
    -pedantic
    -Wno-unused-function # warning is triggered when ZSTD Threading feature is disabled (helper functions are no longer called)

    # -Werror
  )

  if(ENABLE_SANITIZER)
    add_compile_options(
      -fsanitize=address
      -fno-omit-frame-pointer
    )
    add_link_options(
      -fsanitize=address
      -fno-omit-frame-pointer
    )
  endif()
endif()

if(MSVC)
  add_compile_options(
    /W4

    # /WX
  )

  if(ENABLE_SANITIZER)
    add_compile_options(
      /fsanitize=address
    )
    add_link_options(
      /fsanitize=address
    )
  endif()
endif(MSVC)

add_library(llvm_plugin_test_utils INTERFACE)

set(CMAKE_CXX_STANDARD 20)

if(ENABLE_SANITIZER)
  target_compile_options(llvm_plugin_test_utils INTERFACE
    -fsanitize=address
    -fno-omit-frame-pointer
  )
  target_link_options(llvm_plugin_test_utils INTERFACE
    -fsanitize=address
    -fno-omit-frame-pointer
  )
endif()

add_executable(TestLoadPlugin
  src/test.cpp
)

# Link against the plugin
target_link_libraries(TestLoadPlugin PRIVATE
  llvm_plugin_test_utils
)

target_compile_options(TestLoadPlugin PRIVATE

  # Needed to intercept runtime value as template parameters
  # By default, Clang parses them early, BEFORE the plugin is even loaded
  # With this flag we force clang to delay template parsing so that the plugin can be loaded and take some action on them
  "-fdelayed-template-parsing"
)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  target_compile_options(TestLoadPlugin PUBLIC
    "-fplugin=/home/michael-roynard/dev/llvm-project/build/Clang18-Debug/lib/CXXJITPlugin.so"
  )
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Release" OR CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo" OR CMAKE_BUILD_TYPE STREQUAL "MinSizeRel")
  target_compile_options(TestLoadPlugin PUBLIC
    "-fplugin=/home/michael-roynard/dev/llvm-project/build/Clang18-Release/lib/CXXJITPlugin.so"
  )
endif()

# Use fplugin to load the plugin during the test
# target_compile_options(TestLoadPlugin PUBLIC
# "--verbose"
# "-fsyntax-only"
# )
add_test(NAME TestLoadPlugin COMMAND $<TARGET_FILE:TestLoadPlugin>)