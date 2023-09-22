#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct FfiResultData {
  const char *str_value;
  uintptr_t line_range_start;
  uintptr_t line_range_end;
  bool is_error;
} FfiResultData;

typedef struct FfiVec_FfiResultData {
  struct FfiResultData *array;
  uintptr_t len;
} FfiVec_FfiResultData;

uintptr_t create_calculator(void);

struct FfiVec_FfiResultData calculate(uintptr_t calculator, const char *input);

void free_results(struct FfiVec_FfiResultData results);

void free_calculator(uintptr_t ptr);
