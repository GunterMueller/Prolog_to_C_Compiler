//// copy a data-block from one file to another


// pass -1 for len to copy all
PRIMITIVE(file_copy, X len, X in, X out)
{
  FILE *fpin = get_input_port(in);
  FILE *fpout = get_output_port(out);
  int count = fixnum_to_word(check_fixnum(len));
  int all = 0;

  if(count < 0) {
    all = 1;
    count = 10000;
  }

  XCHAR *buf = malloc(count + 1);
  ASSERT(buf, "out of memory - can not allocate block of size %d", count);

  do {
    int n = fread(buf, 1, count, fpin);

    if(n < count && !feof(fpin)) 
      system_error("error reading data");

    if(fwrite(buf, 1, n, fpout) < n)
      system_error("error writing data");
  } while(all);

  free(buf);
  return 1;
}

