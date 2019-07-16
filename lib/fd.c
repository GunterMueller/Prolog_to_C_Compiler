/* some operations on file-descriptors (raw I/O) */


#include <poll.h>


PRIMITIVE(open_fd, X fd, X input, X mode, X data, X result) 
{
  int len;
  FILE *fp = fdopen(fixnum_to_word(fd), to_string(mode, &len));
  X port = PORT(fp, input, ONE, data);
  return unify(port, result);
}


PRIMITIVE(raw_read, X fd, X count, X bytes)
{
  int fno = fixnum_to_word(check_fixnum(fd));
  XWORD n = fixnum_to_word(check_fixnum(count));
  ensure_string_buffer(n, NULL);
  size_t total = read(fno, string_buffer, n);

  if(total == -1)
    system_error(strerror(errno));

  return unify(bytes, string_to_list(string_buffer, total));
}


PRIMITIVE(raw_write, X fd, X bytes, X written)
{
  int fno = fixnum_to_word(check_fixnum(fd));
  int len;
  XCHAR *ptr = to_string(bytes, &len);
  size_t total = write(fno, ptr, len);

  if(total == -1)
    system_error(strerror(errno));

  return unify(written, word_to_fixnum(total));
}


PRIMITIVE(poll_fds, X fdlist, X timeout, X rdylist)
{
  static struct pollfd fds[ 256 ];	/* max */
  int n = 0;

  while(fdlist != END_OF_LIST_VAL) {
    fds[ n ].fd = fixnum_to_word(check_fixnum(deref(slot_ref(fdlist, 0))));
    fds[ n ].events = POLLIN | POLLOUT;
    fdlist = deref(slot_ref(fdlist, 1));
    ++n;
  }

  int rn = poll(fds, n, fixnum_to_word(check_fixnum(timeout)));

  if(rn == -1) {
    if(errno == EINTR)
      return unify(rdylist, END_OF_LIST_VAL);

    system_error(strerror(errno));
  }

  //XXX does not check for full heap
  X lst = END_OF_LIST_VAL;

  for(int i = 0; n > 0; ++i) {
    if((fds[ i ].revents & (POLLERR | POLLIN | POLLOUT)) != 0)
      lst = PAIR(word_to_fixnum(fds[ i ].fd), lst);

    --n;
  }

  return unify(rdylist, lst);
}


PRIMITIVE(set_stream_buffer, X port, X buf)
{
  X stream = get_stream(port);
  XCHAR *buffer;
  int mode;
  size_t size;

  if(buf == ZERO) {
    buffer = NULL;
    mode = _IONBF;
    size = 0;
  } 
  else {
    buffer = malloc(size = fixnum_to_word(check_fixnum(buf)));
    mode = _IOFBF;
  }

  if(setvbuf(port_file(stream), buffer, mode, size) != 0)
    system_error(strerror(errno));

  return 1;
}
