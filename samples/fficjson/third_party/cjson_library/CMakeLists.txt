cmake_minimum_required(VERSION 3.7 FATAL_ERROR)
project(cjson_library VERSION 1.0.0 LANGUAGES C)
add_library(cjson_library SHARED cJSON.c)

set_target_properties(cjson_library PROPERTIES
    PUBLIC_HEADER cJSON.h
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    OUTPUT_NAME "cjson"
    XCODE_ATTRIBUTE_CODE_SIGN_IDENTITY "Hex_Identity_ID_Goes_Here"
)
