#include "type.h"

BOOLEAN isPointerOrArrayType(A_TYPE *t) {     //page 321에 정의 됨 -> 안하면 정의 안되어있다고 오류나옴
	if(t && (t->kind == T_POINTER || t->kind == T_ARRAY))
		return(TRUE);
	else
		return(FALSE);
}