static int64_t dummy_method_to_enforce_bundling(void) {
    int64_t dummy_var = 0;
    dummy_var ^= ((int64_t) (void*) create_calculator);
    dummy_var ^= ((int64_t) (void*) calculate);
    dummy_var ^= ((int64_t) (void*) free_results);
    dummy_var ^= ((int64_t) (void*) free_calculator);
    return dummy_var;
}
