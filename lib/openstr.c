//// perform I/O on strings


//*** not portable, will probably only work on Linux


typedef struct MEMSTREAM_DATA
{
  XCHAR *ptr;
  size_t size;
} MEMSTREAM_DATA;


PRIMITIVE(open_input_string, X str, X data, X result)
{
  int len;
  XCHAR *b1 = to_string(str, &len);
  XCHAR *buf = xstrndup(b1, len + 1);
  FILE *fp = fmemopen(buf, len + 1, "r");

  if(fp == NULL)
    system_error(strerror(errno));

  X port = PORT(fp, ONE, ONE, data);
  return unify(port, result);  
}


PRIMITIVE(open_output_string, X data, X result)
{
  int len;
  MEMSTREAM_DATA *dp = malloc(sizeof(MEMSTREAM_DATA));
  FILE *fp = open_memstream(&(dp->ptr), &(dp->size));

  if(fp == NULL) {
    free(dp);
    system_error(strerror(errno));
  }

  X ptr = POINTER(dp);
  X sym = CSYMBOL("data");
  X str = STRUCTURE(sym, 1);
  SLOT_INIT(str, 1, ptr);
  X port = PORT(fp, ZERO, ONE, PAIR(str, data));
  return unify(port, result);  
}


PRIMITIVE(get_output_string, X port, X data, X result)
{
  MEMSTREAM_DATA *dp = slot_ref(data, 0);
  fclose(port_file(port));
  SLOT_SET(port, 2, ZERO);
  X str = string_to_list(dp->ptr, dp->size);
  free(dp->ptr);
  free(dp);
  return unify(result, str);
}
