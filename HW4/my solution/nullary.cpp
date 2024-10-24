#include "type.h"
#include "nullary.h"
#include "expr.h"

namespace sym 
{
	double Const::get_value() const { return value_; }
	
	Const::operator double() const { return value_; }

	bool Const::is_con() const { return true; }

	__expr_t* Const::eval(const var_map_t& var_map) const {return new Const(value_); }

	__expr_t* Const::diff(const std::string& v) const { return new Const(0); }

	std::ostream& Const::operator<< (std::ostream &out) const { return (out << value_); }

	bool Const::operator==(const __expr_t& other) const {
	   auto newP = dynamic_cast<const Const*>(&other);
	   if (newP != nullptr) {
	        if (value_ == newP->get_value()) {
	            return true;
	        }
	   }
	   auto newE = dynamic_cast<const Expr*>(&other);
	   if ((newE != nullptr) && (*newE == *this))
	        return true;
	   return false;
	    
	}
	
}

namespace sym 
{
	std::string Var::get_variable() const { return var_; }
	Var::operator std::string () const { return var_; }
	
	bool Var::is_var() const { return true; }

	__expr_t* Var::eval(const var_map_t& vars) const {
	    if (vars.count(var_)) return new Const(vars.at(var_));
	    else return new Var(var_);
	    
	}

	__expr_t* Var::diff(const std::string& v) const {
	    if (var_ == v) return new Const(1);
	    else return new Const(0);
	}
	
	std::ostream& Var::operator<< (std::ostream &out) const { return (out << var_); }

	bool Var::operator==(const __expr_t& other) const {
	    auto newP = dynamic_cast<const Var*>(&other);
	   if (newP != nullptr) {
	        if (var_.compare(newP->get_variable()) == 0) {
	            return true;
	        }
	   }
	   auto newE = dynamic_cast<const Expr*>(&other);
	   if ((newE != nullptr) && (*newE == *this))
	        return true;
	   
	   return false;
	    
	}
}
