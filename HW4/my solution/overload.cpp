#include "nullary.h"
#include "unary.h"
#include "binary.h"

namespace sym {
    
    //helper that detects which instance of __expr_t it is and returns a new pointer
    const __expr_t* helper(const __expr_t &op) {
        if (op.is_nullary()) return op.eval();
        return &op;
    }
    
	__expr_t& operator-(const __expr_t &op) { return *(new NegOp(helper(op))); }
	__expr_t& exp(const __expr_t &op) { return *(new ExpOp(helper(op))); }

	__expr_t& operator+(const __expr_t &lhs, const __expr_t &rhs) { return *(new AddOp(helper(lhs), helper(rhs))); }
	__expr_t& operator+(double lhs, const __expr_t &rhs) { return *(new AddOp(new Const(lhs), helper(rhs))); }
	__expr_t& operator+(const __expr_t &lhs, double rhs) { return *(new AddOp(helper(lhs), new Const(rhs))); }

	__expr_t& operator*(const __expr_t &lhs, const __expr_t &rhs) { return *(new MulOp(helper(lhs), helper(rhs))); }
	__expr_t& operator*(double lhs, const __expr_t &rhs) { return *(new MulOp(new Const(lhs), helper(rhs))); }
	__expr_t& operator*(const __expr_t &lhs, double rhs) { return *(new MulOp(helper(lhs), new Const(rhs))); }
	
}


