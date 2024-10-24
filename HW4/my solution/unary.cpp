#include "nullary.h"
#include "unary.h"
#include "binary.h"
#include <math.h>
#include "expr.h"

namespace sym 
{
	bool NegOp::is_neg() const { return true; }

	__expr_t* NegOp::eval(const var_map_t& vars) const {
	    
	    auto ifConst = (dynamic_cast<const Const*>(operand));
	    if (ifConst != nullptr) return new Const(-(ifConst->get_value()));
	    else return new NegOp(operand->eval(vars));
	    
	}

	__expr_t* NegOp::diff(const std::string& v) const { return NegOp(operand->diff(v)).eval(); }

	std::ostream& NegOp::operator<< (std::ostream &out) const { 
	    
	    if ((dynamic_cast<const sym::__nullary_op_t*>(operand)) != nullptr) out << "-" << *operand;
	    else out << "-(" << *operand << ")";
	    
	    return out;
	}

	bool NegOp::operator==(const __expr_t& other_) const {
	    
	   auto newP = dynamic_cast<const NegOp*>(&other_);
	   if (newP != nullptr) {
	        if (*operand == *(newP->operand)) {
	            return true;
	        }
	   }
	   auto newE = dynamic_cast<const Expr*>(&other_);
	   if ((newE != nullptr) && (*newE == *this))
	        return true;
	   return false;
	}
}

namespace sym 
{
	bool ExpOp::is_exp() const { return true; }

	__expr_t* ExpOp::eval(const var_map_t& vars) const {
	    
	    auto ifConst = (dynamic_cast<const Const*>(operand));
	    if (ifConst != nullptr) return new Const(std::exp(ifConst->get_value()));
	    else return new ExpOp(operand->eval(vars));
	    
	}

	__expr_t* ExpOp::diff(const std::string& v) const { return MulOp(operand->diff(v), ExpOp(operand->eval()).eval()).eval(); }

	std::ostream& ExpOp::operator<< (std::ostream &out) const {
	    
	    if ((dynamic_cast<const sym::__nullary_op_t*>(operand)) != nullptr) out << "e^" << *operand;
	    else out << "e^(" << *operand << ")";
	    
	    return out;
	    
	}

	bool ExpOp::operator==(const __expr_t& other_) const {
	    
	    auto newP = dynamic_cast<const ExpOp*>(&other_);
	   if (newP != nullptr) {
	        if (*operand == *(newP->operand)) {
	            return true;
	        }
	   }
	   auto newE = dynamic_cast<const Expr*>(&other_);
	   if ((newE != nullptr) && (*newE == *this))
	        return true;
	   return false;
	   
	}
}
