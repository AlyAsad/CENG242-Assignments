#include "binary.h"
#include "nullary.h"
#include <math.h>
#include "expr.h"

namespace sym 
{
	bool AddOp::is_add() const { return true; }

	__expr_t* AddOp::eval(const var_map_t& vars) const {
	    
	    auto CLHS = (dynamic_cast<const Const*>(lhs_));
	    auto CRHS = (dynamic_cast<const Const*>(rhs_));
	    
	    if (CLHS != nullptr && CLHS->get_value() == 0)
	        return rhs_->eval(vars);
	    
	    if (CRHS != nullptr && CRHS->get_value() == 0)
	        return lhs_->eval(vars);
	    
	    if (CLHS != nullptr && CRHS != nullptr)
	        return new Const(CLHS->get_value() + CRHS->get_value());
	   
	    return new AddOp(lhs_->eval(vars),rhs_->eval(vars));
	    
	}

	__expr_t* AddOp::diff(const std::string& v) const { return AddOp(lhs_->diff(v), rhs_->diff(v)).eval(); }

	std::ostream& AddOp::operator<< (std::ostream &out) const {
	    
	    if ((dynamic_cast<const sym::__nullary_op_t*>(lhs_)) != nullptr) out << *lhs_ << " + ";
	    else out << "(" << *lhs_ << ") + ";
	    
	    if ((dynamic_cast<const sym::__nullary_op_t*>(rhs_)) != nullptr) out << *rhs_;
	    else out << "(" << *rhs_ << ")";
	    
	    return out;
	}

	bool AddOp::operator==(const __expr_t& other_) const {
	    
	    auto newP = dynamic_cast<const AddOp*>(&other_);
	   if (newP != nullptr) {
	        if ((*lhs_ == *(newP->lhs_) && *rhs_ == *(newP->rhs_)) || 
	           (*lhs_ == *(newP->rhs_) && *rhs_ == *(newP->lhs_))) {
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
	bool MulOp::is_mul() const { return true; }

	__expr_t* MulOp::eval(const var_map_t& vars) const { 
	    
	    auto CLHS = (dynamic_cast<const Const*>(lhs_));
	    auto CRHS = (dynamic_cast<const Const*>(rhs_));
	    
	    if (CLHS != nullptr) {
	        if (CLHS->get_value() == 0)
	            return new Const(0);
	        if (CLHS->get_value() == 1)
	            return rhs_->eval(vars);
	    }
	    
	    if (CRHS != nullptr) {
	        if (CRHS->get_value() == 0)
	            return new Const(0);
	        if (CRHS->get_value() == 1)
	            return lhs_->eval(vars);
	    }
	    
	    if (CLHS != nullptr && CRHS != nullptr)
	        return new Const(CLHS->get_value() * CRHS->get_value());
	   
	    return new MulOp(lhs_->eval(vars),rhs_->eval(vars));
	    
	}

	__expr_t* MulOp::diff(const std::string& v) const {
	    return AddOp(MulOp(lhs_->diff(v), rhs_->eval()).eval(), MulOp(lhs_->eval(), rhs_->diff(v)).eval()).eval(); 
	}

	std::ostream& MulOp::operator<< (std::ostream &out) const {
	    
	    if ((dynamic_cast<const sym::__nullary_op_t*>(lhs_)) != nullptr) out << *lhs_ << " * ";
	    else out << "(" << *lhs_ << ") * ";
	    
	    if ((dynamic_cast<const sym::__nullary_op_t*>(rhs_)) != nullptr) out << *rhs_;
	    else out << "(" << *rhs_ << ")";
	    
	    return out;
	    
	}

	bool MulOp::operator==(const __expr_t& other_) const {
	    
	    auto newP = dynamic_cast<const MulOp*>(&other_);
	   if (newP != nullptr) {
	        if ((*lhs_ == *(newP->lhs_) && *rhs_ == *(newP->rhs_)) || 
	           (*lhs_ == *(newP->rhs_) && *rhs_ == *(newP->lhs_))) {
	            return true;
	        }
	   }
	   auto newE = dynamic_cast<const Expr*>(&other_);
	   if ((newE != nullptr) && (*newE == *this))
	        return true;
	   return false;
	    
	}
}



















