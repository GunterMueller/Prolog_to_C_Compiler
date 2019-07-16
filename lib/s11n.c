//// serialization


#define relative_to(ptr, base)   ((X)((XWORD)(ptr) - (XWORD)(base)))

#define FAKE_END_OF_LIST_VAL     ((X)2)


static int evict_term_recursive(X x, X *dest, void *base, void *(*alloc)(size_t))
{
 restart:
  x = deref(x);

  if(is_FIXNUM(x)) {
    *dest = x;
    return 1;
  }

  if(x == END_OF_LIST_VAL) {
    *dest = FAKE_END_OF_LIST_VAL;
    return 1;
  }

  if(is_VAR(x)) {
    X *tp = lookup_shared_term(x, 1);

    if(*tp != NULL) {
      *dest = tp[ 1 ];
      return 1;
    }

    *tp = x;
    BLOCK *newvar = (BLOCK *)alloc(sizeof(XWORD) * (VAR_SIZE + 1));

    if(!newvar) return 0;

    newvar->h = VAR_TAG | VAR_SIZE;
    newvar->d[ 0 ] = (X)newvar;
    newvar->d[ 1 ] = word_to_fixnum(variable_counter++);
    newvar->d[ 2 ] = word_to_fixnum(0); /* timestamp */
    tp[ 1 ] = relative_to(newvar, base);
    *dest = tp[ 1 ];
#ifdef USE_DELAY
    x = slot_ref(x, 3);
    dest = &(newvar->d[ 3 ]);
    goto restart;
#endif
    return 1;
  }

  ASSERT(!is_DBREFERENCE(x), "db-references can not be evicted");

  if(is_byteblock(x)) {
    XWORD size = objsize(x);
    BYTEBLOCK *b = (BYTEBLOCK *)alloc(sizeof(XWORD) + size);

    if(!b) return 0;

    b->h = objbits(x);
    memcpy(b->d, objdata(x), size);
    *dest = relative_to(b, base);
    return 1;
  }

  if(is_SYMBOL(x)) {
    // just freeze string - it will be interned when unevicted
    X str = slot_ref(x, 0);
    X *tp = lookup_shared_term(str, 1);

    if(*tp != NULL) {
      *dest = tp[ 1 ];
      return 1;
    }

    *tp = x;

    if(!evict_term_recursive(str, &(tp[ 1 ]), base, alloc))
      return 0;

    *dest = tp[ 1 ];
    return 1;
  }

#ifndef NO_CHECK_CYCLES
  for(X *ptr = cycle_stack; ptr < cycle_stack_top; ptr += 2) {	
    if(*ptr == x) {
      *dest = ptr[ 1 ];
      return 1;
    }
  }			
#endif

  CHECK_CYCLE_STACK;
  PUSH_ON_CYCLE_STACK(x);
  XWORD size = objsize(x);
  XWORD i = 0;
  BLOCK *b = (BLOCK *)alloc(sizeof(XWORD) * (size + 1));

  if(!b) return 0;

  b->h = objbits(x);
  PUSH_ON_CYCLE_STACK(relative_to(b, base));

  if(is_specialblock(x)) {
    ++i;
    b->d[ 0 ] = slot_ref(x, 0);	/* XXX ??? */
  }

  *dest = relative_to(b, base);

#ifdef NO_CHECK_CYCLES
  if(i >= size) return 1;

  --size;
#endif

  while(i < size) {
    if(!evict_term_recursive(slot_ref(x, i), &(b->d[ i ]), base, alloc))
      return 0;

    ++i;
  }

#ifdef NO_CHECK_CYCLES
  x = slot_ref(x, i);
  dest = &(b->d[ i ]);
  goto restart;
#else
  POP_CYCLE_STACK;
  return 1;
#endif
}


// point into string_buffer
static void *serialization_ptr, *serialization_limit;


static void *serialize_alloc(size_t size)
{
  XWORD adr = ALIGN((XWORD)serialization_ptr);
  XWORD adr2 = adr + size;

  if(adr2 > (XWORD)serialization_limit) return NULL;

  serialization_ptr = (void *)adr2;
  return (void *)adr;
}


static void *serialize_term(X x, void **end, void *buffer, void *limit)
{
  for(;;) {
    INIT_CYCLE_STACK;
    X y;
    void *ptr = buffer;
    serialization_ptr = buffer;
    serialization_limit = limit;

    if(!evict_term_recursive(x, &y, buffer, serialize_alloc))
      ensure_string_buffer(string_buffer_length + 1, NULL); /* force resizing */
    else {
      clear_shared_term_table();
      *end = serialization_ptr;
      return ptr;
    }
      
    clear_shared_term_table();
  }
}


#undef relative_to
#define relative_to(ptr, base)   ((X)((XWORD)(ptr) + (XWORD)(base)))


// copy object back into GC'd memory - this will return 0, if heap-space is insufficient
// note: also re-interns symbols (atoms)
// another note: previously stored compound items will not be identical, with the exceptions of atoms
static int unevict_term_recursive(X *xp, void *base)
{
  X x;

 restart:
  x = *xp;

  if(is_FIXNUM(x)) return 1;

  if(x == FAKE_END_OF_LIST_VAL) {
    *xp = END_OF_LIST_VAL;
    return 1;
  }

  if(is_VAR(x)) {
    X *tp = lookup_shared_term(x, 1);

    if(*tp != NULL) {
      *xp = tp[ 1 ];
      return 1;
    }

    *tp = x;

    if(alloc_top + objsize(x) + 1 > fromspace_limit) return 0;
    
    tp[ 1 ] = *xp = make_var();
#ifdef USE_DELAY
    X al = slot_ref(x, 3);

    //XXX may not be fixnum
    if(al == FAKE_END_OF_LIST_VAL) al = END_OF_LIST_VAL;
    else al = relative_to(al, base);
    
    if(!unevict_term_recursive(&al, base)) return 0;

    SLOT_SET(*xp, 3, al);
#endif
    return 1;
  }

  if(is_byteblock(x)) {
    X *tp;

    if(is_STRING(x)) {
      tp = lookup_shared_term(x, 1);

      if(*tp != NULL) {
	*xp = tp[ 1 ];
	return 1;
      }

      *tp = x;
    }

    XWORD size = objsize(x);

    if(alloc_top + bytes_to_words(size + 1) > fromspace_limit)
      return 0;

    ALLOCATE_BYTEBLOCK(BYTEBLOCK *b, objtype(x), size);
    memcpy(b->d, objdata(x), size);

    if(is_STRING(x)) tp[ 1 ] = *xp = intern((X)b);
    else tp[ 1 ] = *xp = (X)b;

    return 1;
  }

#ifndef NO_CHECK_CYCLES
  for(X *ptr = cycle_stack; ptr < cycle_stack_top; ptr += 2) {	
    if(*ptr == x) {						
      *xp = ptr[ 1 ];						
      return 1; 
    }
  }
#endif

  CHECK_CYCLE_STACK;
  PUSH_ON_CYCLE_STACK(x);
  XWORD i = 0;
  XWORD size = objsize(x);

  if(alloc_top + size + 1 > fromspace_limit) return 0;

  ALLOCATE_BLOCK(BLOCK *b, objtype(x), size);
  PUSH_ON_CYCLE_STACK(b);
  
  // initialize, in case unevicting elements fails
  for(i = 0; i < size; ++i)
    b->d[ i ] = ZERO;
  
  if(is_specialblock(x)) {
    i = 1;
    b->d[ 0 ] = slot_ref(x, 0);	/* XXX ??? */
  }
  else i = 0;

  *xp = (X)b;
#ifdef NO_CHECK_CYCLES
  if(i >= size) return 1;

  --size;
#endif

  while(i < size) {
    X z = slot_ref(x, i);

    if(is_FIXNUM(z)) b->d[ i ] = z;
    else if(z == FAKE_END_OF_LIST_VAL) b->d[ i ] = END_OF_LIST_VAL;
    else {
      b->d[ i ] = relative_to(z, base);

      if(!unevict_term_recursive(&(b->d[ i ]), base)) {
	b->d[ i ] = ZERO;
	return 0;
      }
    }

    ++i;
  }

#ifdef NO_CHECK_CYCLES
  X z = slot_ref(x, i);

  if(is_FIXNUM(z)) b->d[ i ] = z;
  else if(z == FAKE_END_OF_LIST_VAL) b->d[ i ] = END_OF_LIST_VAL;
  else {
    b->d[ i ] = relative_to(z, base);
    xp = &(b->d[ i ]);
    goto restart;
  }

#else
  POP_CYCLE_STACK;
#endif
  return 1;
}


static X deserialize_term(void *buffer, int *failed)
{
#ifdef PROFILE_MEMORY
  XWORD oldheap = where->heap.total;
#endif
  X y = (X)buffer;
  INIT_CYCLE_STACK;
  *failed = !unevict_term_recursive(&y, buffer);
  clear_shared_term_table();

#ifdef PROFILE_MEMORY
  if(*failed) where->heap.total = oldheap;
#endif

  return y;
}


PRIMITIVE(serialize, X x, X result)
{
  void *end;
  serialize_term(x, &end, string_buffer, string_buffer + string_buffer_length);
  int len = (XWORD)end - (XWORD)string_buffer;
  X y = STRING(len);
  memcpy(objdata(y), string_buffer, len);
  return unify(result, intern(y));
}


PRIMITIVE(deserialize, X x, X result)
{
  int size;
  void *ptr = to_string(x, &size);
  int failed;
  X y = deserialize_term(ptr, &failed);
  return !failed && unify(result, y);
}
