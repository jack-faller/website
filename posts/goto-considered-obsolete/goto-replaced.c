// Locate some entries in an array.
T *find_using_goto(T *array, size_t length) {
  T *result = NULL;
  buffer_t buffer = make_buffer();

  for (int i = 0; i < length; ++i) {
    if (should_finish(&array[i], &buffer))
      goto found;
    else if (check(&array[i], &buffer))
      push(&result, &array[i]);
  }

  printf("ERROR: didn't locate all requested items.\n");
  result = NULL;
  goto cleanup;
 found:
  validate_result(result);
 cleanup:
  free_buffer(buffer);
  return result;
}
// The same as above, with functions instead of gotos.
T *find_functional(T *array, size_t length) {
  buffer_t buffer = make_buffer();
  T *cleanup(T *final) {
    free_buffer(buffer);
    return final;
  }
  T *found(T *result) {
    validate_result(result);
    return cleanup(result);
  }
  T *result = NULL;
  for (int i = 0; i < length; ++i) {
    if (should_finish(&array[i], &buffer))
      return found(result);
    else if (check(&array[i], &buffer))
      push(&result, &array[i]);
  }
  printf("ERROR: didn't locate all requested items.\n");
  return cleanup(NULL);
}
