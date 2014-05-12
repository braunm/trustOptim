// exceptions.hpp.  Part of the trustOptim package for the R programming language.

// This file is part of trustOptim, a nonlinear optimization package
// for the R statistical programming platform.
//
// Copyright (C) 2013 Michael Braun
//
// This Source Code Form is subject to the license terms of the Mozilla
// Public License v. 2.0. If a copy of the MPL was not distributed
// with this file, you can obtain one at http://mozilla.org/MPL/2.0/.


#ifndef MY_EXCEPTION
#define MY_EXCEPTION


#include <iostream>
#include <exception>
#include <stdexcept>
#include <algorithm>
#include <string>
#include <cstring>

#include <common_R.hpp>

class MyException : public std::exception {

public:

  const std::string reason;
  const std::string file;
  const int line;
  
  std::string message;
  

 MyException(const std::string reason_,
	     const std::string file_,
	      const int line_) :
    reason(reason_), file(file_), line(line_)
  {
    
    std::ostringstream oss;
	
	oss << "\nException thrown from File " << file << "  at Line " << line <<".\n";
	oss << "Reason : " << reason << "\n";
	
	message = oss.str();

  }

  virtual ~MyException() throw() {};

  virtual const char* what() const throw()
  {
    return message.c_str();
 
  }

  void print_message(){

    TRUST_COUT << message << std::endl;
  }


};




#endif
