//// debugging utilities


// dump trail-stack and how CPs point into it

static void dump_ts(CHOICE_POINT *C)
{
  X *tp = trail_stack;
  CHOICE_POINT *cp = choice_point_stack;

  while(tp < trail_top) {
    while(cp->T == tp) {
      fprintf(stderr, "%p (%ld from %ld) -> %p\n", cp, cp - choice_point_stack, C - choice_point_stack, tp);
      ++cp;
    }

    fprintf(stderr, "  %p: _%ld\n", tp, fixnum_to_word(slot_ref(*tp, 2)));
    ++tp;
  }

  fprintf(stderr, "tp: %p\n", trail_top);
}
