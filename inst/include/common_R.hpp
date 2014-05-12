
// common_R.hpp -- this file is part of trustOptim, a nonlinear optimization package
// for the R statistical programming platform.
//
// Copyright (C) 2013 Michael Braun
//
// This Source Code Form is subject to the license terms of the Mozilla
// Public License v. 2.0. If a copy of the MPL was not distributed
// with this file, you can obtain one at http://mozilla.org/MPL/2.0/.


#ifndef COMMON_R
#define COMMON_R

#ifndef TRUST_COUT
#define TRUST_COUT Rcpp::Rcout
#endif

#define ERROR_HANDLER R_Interface_Error_Handler

#include <Rcpp.h>
#include <R_ext/Utils.h>
#include "exceptions.hpp"

#define BEGIN_R_INTERFACE try {

  //

#define END_R_INTERFACE  } catch (  const MyException& ex) { \
       ::Rf_error(ex.what());			\
  } catch( const std::exception& __ex__ ) {		\
          forward_exception_to_r( __ex__ );		\
       } catch(...) {				\
    TRUST_COUT << "Unknown error\n";				\
    ::Rf_error( "c++ exception (unknown reason)" );	\
  }  \
  return R_NilValue;


#define CONVERT_STRING2(x) #x
#define CONVERT_STRING(x) CONVERT_STRING2(x)
#define MAKE_STRING(x,y) x ## y
#define OBJ_FUNC(x) MAKE_STRING(x, _obj)
#define EVAL_FUNC(x) MAKE_STRING(x, _eval)
#define TRUST_RUN(x) MAKE_STRING(x, _trust_run)
#define SPARSE_TRUST_RUN(x) MAKE_STRING(x, _sparse_trust_run)
#define GET_FDFH(x) MAKE_STRING(x, _get_fdfh)
#define GET_FDFH_SPARSE(x) MAKE_STRING(x, _get_fdfh_sparse)
#define GET_FDF(x) MAKE_STRING(x, _get_fdf)
#define GET_F(x) MAKE_STRING(x, _get_f)
#define GET_DATA_LL(x) MAKE_STRING(x, _get_data_LL)
#define GET_F_BYPASS_TAPE(x) MAKE_STRING(x, _get_f_bypass_tape)
#define GET_F_DIRECT(x) MAKE_STRING(x, _get_f_direct)

template<typename T>
void R_Interface_Error_Handler(const T & ex) {

  // takes exception object and does R-friendly things to it
  ex.print_message();
  Rf_error("R error\n");

}


static inline void check_interrupt_impl(void* /*dummy*/) {
 R_CheckUserInterrupt();
}

inline bool check_interrupt() {
  return (R_ToplevelExec(check_interrupt_impl, NULL) == FALSE);
}

#endif
