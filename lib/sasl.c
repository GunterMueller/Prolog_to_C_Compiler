/* Using gsasl for authentification */


#include <gsasl.h>


static Gsasl *ctx = NULL;
static Gsasl_session *session;


static int sasl_init(CHOICE_POINT *C0) 
{
  return gsasl_init(&ctx) == GSASL_OK;
}


static int sasl_done(CHOICE_POINT *C0)
{
  gsasl_done(ctx);
  return 1;
}


PRIMITIVE(sasl_client_start, X mech, X user, X passwd, X token, X realm) 
{
  int len;
  int rc = gsasl_client_start(ctx, to_string(mech, &len), &session);

  if(rc != GSASL_OK)
    system_error((XCHAR *)gsasl_strerror(rc));

  gsasl_property_set(session, GSASL_AUTHID, to_string(user, &len));
  gsasl_property_set(session, GSASL_PASSWORD, to_string(passwd, &len));
  gsasl_property_set(session, GSASL_ANONYMOUS_TOKEN, to_string(token, &len));
  gsasl_property_set(session, GSASL_REALM, to_string(realm, &len));
  return 1;
}


static int sasl_finish(CHOICE_POINT *C0)
{
  gsasl_finish(session);
  return 1;
}


PRIMITIVE(sasl_step64, X input, X result)
{
  XCHAR *p;
  int len;
  int rc = gsasl_step64(session, to_string(input, &len), &p);

  if(rc == GSASL_NEEDS_MORE || rc == GSASL_OK) {
    // succeeds if more is needed
    rc = unify(result, CSYMBOL(p));
    gsasl_free(p);
    return 1;
  }

  system_error((XCHAR *)gsasl_strerror(rc));
  return 0;			/* never executed */
}
