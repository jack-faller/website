conversion_result convert_cat(cat src) {
  // Here f is explicitly named
  dog dst = run dog f(void) { switch (src) { ... } };
  // whereas here an unnamed function is implicitly introduced.
  dog dst = run switch (src) {
    case TABBY: return TERRIER;
    case MANX: return COLLIE;
    default:
      printf("Couldn't convert cat to dog.\n");
      return CONVERSION_FAIL from convert_cat;
  };
  run_conversion(src, dst);
  printf("Converted %s to %s.\n", cat_name(src), dog_name(dst));
  return CONVERSION_SUCCESS;
}
