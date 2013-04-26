/* Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This file could/should be generated by a stub generator from <ndbm.h>
 * This version has been written by hand.
 *
 * Revision Log
 * ------------
 * $Log: ndbm_stub.c,v $
 * Revision 1.2  1997/06/30 10:46:20  stephenb
 * [Bug #30029]
 * Propagate name changes made in mlw_ci.h
 *
 * Revision 1.1  1997/04/29  14:52:31  stephenb
 * new unit
 * [Bug #30030]
 *
 */

#include <ndbm.h>
#include <errno.h>
#include "mlw_ci.h"

static mlw_val mlw_stub_dbm_open(mlw_val arg)
{
  char *file_name= mlw_ci_char_ptr_to_charp(mlw_arg(arg, 0));
  int flags= mlw_ci_int_to_int(mlw_arg(arg, 1));
  int mode= mlw_ci_int_to_int(mlw_arg(arg, 2));
  DBM * dbm= dbm_open(file_name, flags, mode);
  return mlw_ci_void_ptr_from_voidp((void *)dbm);
}



static mlw_val mlw_stub_dbm_fetch(mlw_val arg)
{
  DBM *dbm= (DBM *)mlw_ci_void_ptr_to_voidp(mlw_arg(arg, 0));
  datum * kp= (datum *)mlw_ci_void_ptr_to_voidp(mlw_arg(arg, 1));
  datum * rp;
  datum key, result;
  memcpy(&key, kp, sizeof(datum));
  result= dbm_fetch(dbm, key);
  if ((rp= malloc(sizeof(datum))) == (datum *)0)
    mlw_ci_raise_syserr(errno);
  memcpy(rp, &result, sizeof(datum));
  return mlw_ci_void_ptr_from_voidp(rp);
}



static mlw_val mlw_stub_dbm_store(mlw_val arg)
{
  DBM *dbm= (DBM *)mlw_ci_void_ptr_to_voidp(mlw_arg(arg, 0));
  datum * kp= (datum *)mlw_ci_void_ptr_to_voidp(mlw_arg(arg, 1));
  datum * cp= (datum *)mlw_ci_void_ptr_to_voidp(mlw_arg(arg, 2));
  int flags = mlw_ci_int_to_int(mlw_arg(arg, 3));
  datum key, contents;
  int result;
  memcpy(&key, kp, sizeof(datum));
  memcpy(&contents, cp, sizeof(datum));
  result= dbm_store(dbm, key, contents, flags);
  return mlw_ci_int_from_int(result);
}



static mlw_val mlw_stub_dbm_delete(mlw_val arg)
{
  DBM *dbm= (DBM *)mlw_ci_void_ptr_to_voidp(mlw_arg(arg, 0));
  datum * kp= (datum *)mlw_ci_void_ptr_to_voidp(mlw_arg(arg, 1));
  datum key;
  int result;
  memcpy(&key, kp, sizeof(datum));
  result= dbm_delete(dbm, key);
  return mlw_ci_int_from_int(result);
}



static mlw_val mlw_stub_dbm_firstkey(mlw_val arg)
{
  DBM *dbm= (DBM *)mlw_ci_void_ptr_to_voidp(arg);
  datum * rp;
  datum result;
  result= dbm_firstkey(dbm);
  if ((rp= malloc(sizeof(datum))) == (datum *)0)
    mlw_ci_raise_syserr(errno);
  memcpy(rp, &result, sizeof(datum));
  return mlw_ci_void_ptr_from_voidp(rp);
}



static mlw_val mlw_stub_dbm_nextkey(mlw_val arg)
{
  DBM *dbm= (DBM *)mlw_ci_void_ptr_to_voidp(arg);
  datum * rp;
  datum result;
  result= dbm_nextkey(dbm);
  if ((rp= malloc(sizeof(datum))) == (datum *)0)
    mlw_ci_raise_syserr(errno);
  memcpy(rp, &result, sizeof(datum));
  return mlw_ci_void_ptr_from_voidp(rp);
}



static mlw_val mlw_stub_dbm_error(mlw_val arg)
{
  DBM *dbm= (DBM *)mlw_ci_void_ptr_to_voidp(arg);
  int result= dbm_error(dbm);
  return mlw_ci_int_from_int(result);
}




static mlw_val mlw_stub_dbm_clearerr(mlw_val arg)
{
  DBM *dbm= (DBM *)mlw_ci_void_ptr_to_voidp(arg);
  int result= dbm_clearerr(dbm);
  return mlw_ci_int_from_int(result);
}



static mlw_val mlw_stub_dbm_close(mlw_val arg)
{
  DBM *dbm= (DBM *)mlw_ci_void_ptr_to_voidp(arg);
  dbm_close(dbm);
  return mlw_val_unit;
}



mlw_ci_export void mlw_stub_init_ndbm(void)
{
  mlw_ci_register_function("dbm_open",     mlw_stub_dbm_open);
  mlw_ci_register_function("dbm_close",    mlw_stub_dbm_close);
  mlw_ci_register_function("dbm_fetch",    mlw_stub_dbm_fetch);
  mlw_ci_register_function("dbm_store",    mlw_stub_dbm_store);
  mlw_ci_register_function("dbm_delete",   mlw_stub_dbm_delete);
  mlw_ci_register_function("dbm_firstkey", mlw_stub_dbm_firstkey);
  mlw_ci_register_function("dbm_nextkey",  mlw_stub_dbm_nextkey);
  mlw_ci_register_function("dbm_error",    mlw_stub_dbm_error);
  mlw_ci_register_function("dbm_clearerr", mlw_stub_dbm_clearerr);
}
