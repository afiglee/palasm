add_library(palasmlib OBJECT
        palasm.cpp)       

add_executable(palasm
        main.cpp

)

target_include_directories(palasm PRIVATE
        .
)       

target_link_libraries(palasm PRIVATE

)

