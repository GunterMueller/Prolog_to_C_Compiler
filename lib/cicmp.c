//// case-insensitive atom-compare


PRIMITIVE(compare_atoms_ci, X a, X b, X r) 
{
  X sa = slot_ref(check_type_SYMBOL(a), 0);
  X sb = slot_ref(check_type_SYMBOL(b), 0);
  XWORD lena = objsize(sa);
  XWORD lenb = objsize(sb);
  int c = 
    strncasecmp((XCHAR *)objdata(sa), (XCHAR *)objdata(sb), lena < lenb ? lena : lenb);

  if(c == 0) {
    if(lena > lenb) c = 1;
    else if(lenb > lena) c = -1;
  }

  return unify(r, word_to_fixnum(c));
}
