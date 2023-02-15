cmake_minimum_required(VERSION 3.10)
set(CMAKE_BUILD_TYPE Debug)

project(CustomLanguage VERSION 0.01)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(CustomLanguage)

target_sources(CustomLanguage PUBLIC
	${CMAKE_SOURCE_DIR}/src/main.cpp
)
