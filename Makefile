#Name
NAME = ST-VENANT_FINIT_VOLUME

# Compiler
FC = gfortran
AR = ar rcs
RM = rm -f

# Directories
SRC_DIR = ./src
SUB_DIR = ./sub
INCLUDE_DIR = ./include
BUILD_DIR = ./build

# Flags for debug and release builds
DEBUG_FFLAGS = -g -O0 -Wall -Wextra -Werror
RELEASE_FFLAGS = -O2 -Wall -Wextra -Werror

# Automatically find all source files
SRC_FILES = $(wildcard $(SRC_DIR)/*.f90)
SUB_FILES = $(wildcard $(SUB_DIR)/*.f90)
ALL_FILES = $(SRC_FILES) $(SUB_FILES)

# Object files
OBJS = $(patsubst $(SRC_DIR)/%.f90,$(BUILD_DIR)/%.o,$(SRC_FILES)) \
		$(patsubst $(SUB_DIR)/%.f90,$(BUILD_DIR)/%.o,$(SUB_FILES))

# Include modules
INCLUDES = -I$(INCLUDE_DIR)

# Group modules
GROUP = -J$(INCLUDE_DIR)

# Targets
DEBUG_TARGET = $(BUILD_DIR)/run_debug
RELEASE_TARGET = $(BUILD_DIR)/run_release

# Default target
all: $(DEBUG_TARGET)

# Debug build
debug: $(DEBUG_TARGET)

# Release build
release: $(RELEASE_TARGET)

# Linking for debug build
$(DEBUG_TARGET): $(OBJS) | $(BUILD_DIR)
	$(FC) $(DEBUG_FFLAGS) $(OBJS) -o $@

# Linking for release build
$(RELEASE_TARGET): $(OBJS) | $(BUILD_DIR)
	$(FC) $(RELEASE_FFLAGS) $(OBJS) -o $@ 

# Compilation for source files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(DEBUG_FFLAGS) $(INCLUDES) $(GROUP) -c $< -o $@

# Compilation for submodule files
$(BUILD_DIR)/%.o: $(SUB_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(DEBUG_FFLAGS) $(INCLUDES) $(GROUP) -c $< -o $@

# Creat build dir if not exist
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

exe_debug:
	$(DEBUG_TARGET)

exe_release:
	$[RELEASE_TARGET]

# Clean up
clean:
	$(RM) $(BUILD_DIR)/*.o $(BUILD_DIR)/run_debug $(BUILD_DIR)/run_release

.PHONY: all clean depend debug release







