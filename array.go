// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import "rsc.io/c2go/cc"

// fixArrays rewrites uses of the untyped "Array" container defined in cmd/gc
// to use native Go slices.
// It has nothing to do with standard C arrays.
func fixArray(fn *cc.Decl, x *cc.Expr) {
	// arraynew(n, sizeof(T)) becomes Go make([]T, 0, n).
	if isCall(x, "arraynew") {
		if len(x.List) != 2 {
			fprintf(x.Span, "wrong number of arguments to arraynew")
			return
		}
		if x.List[1].Op != cc.SizeofType {
			fprintf(x.Span, "second argument to arraynew must be sizeof(T)")
			return
		}
		x.Left.Text = "make"
		x.Left.XDecl = nil
		typ := &cc.Type{Kind: Slice, Base: x.List[1].Type}
		x.XType = typ
		x.List = append(x.List, x.List[0])
		x.List[1] = &cc.Expr{Op: cc.Number, Text: "0"}
		x.List[0] = &cc.Expr{Op: ExprType, Type: typ}
		return
	}

	// arraylength(x) becomes len(x)
	if isCall(x, "arraylength") {
		x.Left.Text = "len"
		x.Left.XDecl = nil
		return
	}

	// arrayset is unused in practice!

	// arrayadd(x, &elem) becomes x = append(x, elem).
	// Strictly speaking, this is not a complete translation,
	// because in the C code x might be a pointer taken from
	// another place, and arrayadd changes the len at that
	// other place too. In cmd/gc this does not happen.
	if isCall(x, "arrayadd") {
		if len(x.List) != 2 {
			fprintf(x.Span, "wrong number of arguments to arrayadd")
			return
		}
		if x.List[1].Op != cc.Addr {
			fprintf(x.Span, "second argument to arrayadd must be &x, have %v", x.List[1])
			return
		}
		append := copyExpr(x)
		append.Left.Text = "append"
		append.Left.XDecl = nil
		x.Op = cc.Eq
		x.Left = append.List[0]
		x.Right = append
		append.List[1] = append.List[1].Left
		return
	}

	// *(T**)(arrayget(x, i)) turns into x[i].
	// Record that x should have translated type T*.
	if x.Op == cc.Indir && x.Left.Op == cc.Cast && x.Left.Type.Kind == cc.Ptr && x.Left.Type.Base.Kind == cc.Ptr && isCall(x.Left.Left, "arrayget") {
		call := x.Left.Left
		x.Op = cc.Index
		x.XType = x.Left.Type.Base
		x.Left = call.List[0]
		x.Right = call.List[1]
		saveSliceType(x.Left, x.XType)
		return
	}

	// TODO: arraysort
}

func fixArrayStmt(fn *cc.Decl, x *cc.Stmt) {
	// Turn call to arrayfree into empty statement.
	// This is the only statment-level operation.
	// All other rewrites are done at the expression level
	// (or pseudo-expression, since assignments count as
	// expressions in our representation).
	if x.Op == cc.StmtExpr && isCall(x.Expr, "arrayfree") {
		x.Op = cc.Empty
		x.Expr = nil
	}
}

func isCall(x *cc.Expr, name string) bool {
	return x != nil && x.Op == cc.Call && x.Left.Op == cc.Name && x.Left.Text == name
}

func saveSliceType(x *cc.Expr, elem *cc.Type) {
	switch x.Op {
	case cc.Name, cc.Arrow, cc.Dot:
		if x.XDecl != nil {
			x.XDecl.Type = &cc.Type{Kind: Slice, Base: elem}
		}
	}
}
