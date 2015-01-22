// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"os"
	"strings"

	"rsc.io/c2go/cc"
)

// rewriteTypes rewrites C types appearing in the program to equivalent Go types.
func rewriteTypes(cfg *Config, prog cc.Syntax) {
	// Cache is needed to cut off translation of recursive types.
	cache := make(map[*cc.Type]*cc.Type)

	cc.Postorder(prog, func(x cc.Syntax) {
		if t, ok := x.(*cc.Type); ok {
			if t.Kind == cc.Struct || t.Kind == cc.Enum {
				for _, d := range t.Decls {
					d.OuterType = t
				}
			}
			if t.Kind == cc.Func && len(t.Decls) == 1 && t.Decls[0].Type.Is(cc.Void) {
				t.Decls = nil
			}
		}
		if d, ok := x.(*cc.Decl); ok {
			if d.Type != nil && d.Type.Kind == cc.Func {
				for _, d1 := range d.Type.Decls {
					d1.CurFn = d
				}
				if d.Body != nil {
					for _, s := range d.Body.Block {
						if s.Op == cc.StmtDecl {
							s.Decl.CurFn = d
						}
					}
				}
			}
		}
	})

	/*
		t := g.canon.Def()
		if cc.Char <= t.Kind && t.Kind <= cc.Enum {
			// Convert to an appropriately sized number.
			// Canon is largest rank from C; convert to Go.
			g.goType = &cc.Type{Kind: c2goKind[t.Kind]}
			continue
		}
		if t.Kind == cc.Ptr || t.Kind == cc.Array {
			// Default is convert to pointer.
			// If there are any arrays or any pointer arithmetic, convert to slice instead.
			k := cc.Ptr
			for _, d := range g.decls {
				if d.Type != nil && d.Type.Kind == cc.Array {
					k = Slice
				}
			}
			for _, f := range g.syntax {
				if f.ptrAdd {
					k = Slice
				}
			}
			if t.Base.Kind == cc.Char {
				g.goType = &cc.Type{Kind: String}
				continue
			}
			g.goType = &cc.Type{Kind: k, Base: toGoType(cfg, nil, nil, t.Base, cache)}
			continue
		}
	*/

	cc.Postorder(prog, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Decl:
			d := x
			if d.Name == "..." || d.Type == nil {
				return
			}
			if d.Name == "" && d.Type.Is(cc.Enum) && len(d.Type.Decls) > 0 {
				for _, dd := range d.Type.Decls {
					dd.Type = idealType
				}
				return
			}
			if d.Init != nil && len(d.Init.Braced) > 0 && d.Type != nil && d.Type.Kind == cc.Array {
				// Initialization of array - do not override type.
				// But if size is not given explicitly, change to slice.
				d.Type.Base = toGoType(cfg, nil, d.Type.Base, cache)
				if d.Type.Width == nil {
					d.Type.Kind = Slice
				}
				return
			}
			d.Type = toGoType(cfg, d, d.Type, cache)

		case *cc.Expr:
			if x.Type != nil {
				t := toGoType(cfg, nil, x.Type, cache)
				if t == nil {
					fprintf(x.Span, "cannot convert %v to go type\n", GoString(x.Type))
				}
				x.Type = t
			}
		}
	})
}

var (
	boolType   = &cc.Type{Kind: Bool}
	byteType   = &cc.Type{Kind: Byte}
	intType    = &cc.Type{Kind: Int}
	uintType   = &cc.Type{Kind: Uint}
	uint32Type = &cc.Type{Kind: Uint32}
	int64Type  = &cc.Type{Kind: Int64}
	uint64Type = &cc.Type{Kind: Uint64}
	idealType  = &cc.Type{Kind: Ideal}
	stringType = &cc.Type{Kind: String}
)

var c2goKind = map[cc.TypeKind]cc.TypeKind{
	cc.Char:      Int8,
	cc.Uchar:     Uint8,
	cc.Short:     Int16,
	cc.Ushort:    Uint16,
	cc.Int:       Int,
	cc.Uint:      Uint,
	cc.Long:      Int, // long is used too indiscriminately to assign 32-bit meaning to it
	cc.Ulong:     Uint32,
	cc.Longlong:  Int64,
	cc.Ulonglong: Uint64,
	cc.Float:     Float32,
	cc.Double:    Float64,
	cc.Enum:      Int,
}

var c2goName = map[string]cc.TypeKind{
	"uchar":  Uint8,
	"int32":  Int32,
	"uint32": Uint32,
	"int64":  Int64,
	"uint64": Uint64,
}

func toGoType(cfg *Config, x cc.Syntax, typ *cc.Type, cache map[*cc.Type]*cc.Type) *cc.Type {
	if typ == nil {
		return nil
	}

	if cache[typ] != nil {
		return cache[typ]
	}

	switch typ.Kind {
	default:
		panic(fmt.Sprintf("unexpected C type %s", typ))

	case Ideal:
		return typ

	case cc.Void:
		return &cc.Type{Kind: cc.Struct} // struct{}

	case cc.Char, cc.Uchar, cc.Short, cc.Ushort, cc.Int, cc.Uint, cc.Long, cc.Ulong, cc.Longlong, cc.Ulonglong, cc.Float, cc.Double, cc.Enum:
		t := &cc.Type{Kind: c2goKind[typ.Kind]}
		if d, ok := x.(*cc.Decl); ok {
			if cfg.bool[declKey(d)] {
				t.Kind = Bool
			}
		}
		return t

	case cc.TypedefType:
		if cfg.typeMap[typ.Name] != "" {
			t := &cc.Type{Kind: cc.TypedefType, Name: cfg.typeMap[typ.Name], TypeDecl: typ.TypeDecl}
			cache[typ] = t
			return t
		}

		// If this is a typedef like uchar, translate the type by name.
		// Otherwise fall back to base.
		def := typ.Base
		if cc.Char <= def.Kind && def.Kind <= cc.Enum {
			if c2goName[typ.Name] != 0 {
				return &cc.Type{Kind: c2goName[typ.Name]}
			}
			return &cc.Type{Kind: c2goKind[typ.Base.Kind]}
		}

		if typ.Name == "va_list" {
			return &cc.Type{Kind: cc.TypedefType, Name: "[]interface{}"}
		}

		// Otherwise assume it is a struct or some such,
		// and preserve the name but translate the base.
		t := &cc.Type{Kind: cc.TypedefType, Name: typ.Name, TypeDecl: typ.TypeDecl}
		cache[typ] = t
		t.Base = toGoType(cfg, nil, typ.Base, cache)
		return t

	case cc.Array:
		if typ.Base.Def().Kind == cc.Char {
			return &cc.Type{Kind: String}
		}
		t := &cc.Type{Kind: cc.Array, Width: typ.Width}
		cache[typ] = t
		t.Base = toGoType(cfg, nil, typ.Base, cache)
		return t

	case cc.Ptr:
		t := &cc.Type{Kind: cc.Ptr}
		cache[typ] = t
		base := x
		if typ.Base.Kind != cc.Func {
			base = nil
		}
		t.Base = toGoType(cfg, base, typ.Base, cache)

		// Convert void* to interface{}.
		if typ.Base.Kind == cc.Void {
			t.Base = nil
			t.Kind = cc.TypedefType
			t.Name = "interface{}"
			return t
		}

		if typ.Base.Kind == cc.Char {
			t.Kind = String
			t.Base = nil
			return t
		}

		d, ok := x.(*cc.Decl)

		if typ.Base.Def().Kind == cc.Uchar && (!ok || !cfg.ptr[declKey(d)]) {
			t.Kind = Slice
			t.Base = byteType
		}
		if ok && cfg.slice[declKey(d)] {
			t.Kind = Slice
		}

		return t

	case cc.Func:
		// A func Type contains Decls, and we don't fork the Decls, so don't fork the Type.
		// The Decls themselves appear in the group lists, so they'll be handled by rewriteTypes.
		// The return value has no Decl and needs to be converted.
		if !typ.Base.Is(cc.Void) {
			if d, ok := x.(*cc.Decl); ok {
				x = &cc.Decl{
					Name:  "return",
					CurFn: d,
				}
			} else {
				x = nil
			}
			typ.Base = toGoType(cfg, x, typ.Base, cache)
		}
		return typ

	case cc.Struct:
		// A struct Type contains Decls, and we don't fork the Decls, so don't fork the Type.
		// The Decls themselves appear in the group lists, so they'll be handled by rewriteTypes.
		return typ
	}
}

// fixGoTypes corrects type errors in Go programs by applying the rules for implicit C conversions.
// It is basically a Go type checker that rewrites the program instead of printing errors.
func fixGoTypes(cfg *Config, prog *cc.Prog) {
	// prog.Decls may contain duplicates, due to forward declarations.
	// TODO(rsc): Should probably remove them.
	did := make(map[*cc.Decl]bool)

	for i := 0; i < len(prog.Decls); i++ {
		decl := prog.Decls[i]
		if did[decl] {
			continue
		}
		did[decl] = true
		if decl.Init != nil {
			fixGoTypesInit(decl, decl.Init)
		}
		if decl.Body != nil {
			t := decl.Type
			if t != nil && t.Kind == cc.Func && t.Base.Is(Int) && len(t.Decls) >= 1 && t.Decls[0].Type.String() == "Fmt*" {
				fixFormatter(decl)
			}
			fixGoTypesStmt(prog, decl, decl.Body)
		}
	}

	// fixGoTypes might have introduced an (x = fmt.Sprintf(...)) in a larger expression.
	// rewrite it out.
	rewriteSyntax(cfg, prog)
}

func fixGoTypesInit(decl *cc.Decl, x *cc.Init) {
	if x.Expr != nil {
		fixGoTypesExpr(nil, x.Expr, x.XType)
	}
	for _, init := range x.Braced {
		fixGoTypesInit(decl, init)
	}
}

func fixGoTypesStmt(prog *cc.Prog, fn *cc.Decl, x *cc.Stmt) {
	if x == nil {
		return
	}

	fixArrayStmt(fn, x)
	fixFormatStmt(fn, x)

	switch x.Op {
	case cc.StmtDecl:
		fixGoTypesExpr(fn, x.Expr, nil)

	case cc.StmtExpr:
		if x.Expr != nil && x.Expr.Op == cc.Call && x.Expr.Left.Op == cc.Name {
			switch x.Expr.Left.Text {
			case "qsort":
				fixQsort(prog, x.Expr)
				return
			case "memset":
				fixMemset(prog, fn, x)
				return
			case "free":
				x.Op = cc.Empty
				x.Expr = nil
				return
			}
		}
		fixGoTypesExpr(fn, x.Expr, nil)

	case cc.If, cc.For:
		fixGoTypesExpr(fn, x.Pre, nil)
		fixGoTypesExpr(fn, x.Post, nil)
		fixGoTypesExpr(fn, x.Expr, boolType)

	case cc.Switch:
		fixGoTypesExpr(fn, x.Expr, nil)

	case cc.Return:
		if x.Expr != nil {
			forceGoType(fn, x.Expr, fn.Type.Base)
		}
	}
	for _, stmt := range x.Block {
		fixGoTypesStmt(prog, fn, stmt)
	}
	if len(x.Block) > 0 && x.Body != nil {
		panic("block and body")
	}
	fixGoTypesStmt(prog, fn, x.Body)
	fixGoTypesStmt(prog, fn, x.Else)

	for _, lab := range x.Labels {
		// TODO: use correct type
		fixGoTypesExpr(fn, lab.Expr, nil)
	}
}

func fixGoTypesExpr(fn *cc.Decl, x *cc.Expr, targ *cc.Type) (ret *cc.Type) {
	if x == nil {
		return nil
	}

	defer func() {
		x.XType = ret
	}()

	if x.Op == cc.Paren {
		return fixGoTypesExpr(fn, x.Left, targ)
	}

	// Make explicit C's implicit conversions from boolean to non-boolean and vice versa.
	switch x.Op {
	case cc.AndAnd, cc.OrOr, cc.Not, cc.EqEq, cc.Lt, cc.LtEq, cc.Gt, cc.GtEq, cc.NotEq:
		if targ != nil && targ.Kind != Bool {
			old := copyExpr(x)
			if targ.Kind == Int {
				x.Op = cc.Call
				x.Left = &cc.Expr{Op: cc.Name, Text: "bool2int"}
				x.List = []*cc.Expr{old}
				x.Right = nil
			} else {
				x.Op = cc.Cast
				x.Left = &cc.Expr{Op: cc.Call, Left: &cc.Expr{Op: cc.Name, Text: "bool2int"}, List: []*cc.Expr{old}}
				x.Type = targ
			}
			fixGoTypesExpr(fn, old, boolType)
			return targ
		}
	default:
		if targ != nil && targ.Kind == Bool {
			old := copyExpr(x)
			left := fixGoTypesExpr(fn, old, nil)
			if left != nil && left.Kind == Bool {
				return targ
			}
			if old.Op == cc.Number {
				switch old.Text {
				case "1":
					x.Op = cc.Name
					x.Text = "true"
					return targ
				case "0":
					x.Op = cc.Name
					x.Text = "false"
					return targ
				}
			}
			x.Op = cc.NotEq
			x.Left = old
			x.Right = zeroFor(left)
			return targ
		}
	}

	fixArray(fn, x)

	switch x.Op {
	default:
		panic(fmt.Sprintf("unexpected construct %v in fixGoTypesExpr - %v - %v", GoString(x), x.Op, x.Span))

	case ExprType:
		// inserted by a rewrite
		return nil

	case ExprSlice:
		// inserted by rewriteLen
		left := fixGoTypesExpr(fn, x.List[0], targ)
		fixGoTypesExpr(fn, x.List[1], nil)
		fixGoTypesExpr(fn, x.List[2], nil)
		return left

	case cc.Comma:
		for i, y := range x.List {
			t := targ
			if i+1 < len(x.List) {
				t = nil
			}
			fixGoTypesExpr(fn, y, t)
		}
		return nil

	case ExprBlock:
		for _, stmt := range x.Block {
			fixGoTypesStmt(nil, fn, stmt)
		}
		return nil

	case cc.Add, cc.And, cc.Div, cc.Mod, cc.Mul, cc.Or, cc.Sub, cc.Xor:
		if x.Op == cc.Sub && isPtrSliceOrArray(x.Left.XType) && isPtrSliceOrArray(x.Right.XType) {
			left := fixGoTypesExpr(fn, x.Left, nil)
			right := fixGoTypesExpr(fn, x.Right, nil)
			if left != nil && right != nil && left.Kind != right.Kind {
				if left.Kind == Slice {
					forceConvert(fn, x.Right, right, left)
				} else {
					forceConvert(fn, x.Left, left, right)
				}
			}
			x.Left = &cc.Expr{Op: cc.Minus, Left: &cc.Expr{Op: cc.Call, Left: &cc.Expr{Op: cc.Name, Text: "cap"}, List: []*cc.Expr{x.Left}}}
			x.Right = &cc.Expr{Op: cc.Call, Left: &cc.Expr{Op: cc.Name, Text: "cap"}, List: []*cc.Expr{x.Right}}
			x.Op = cc.Add
			return intType
		}

		left := fixGoTypesExpr(fn, x.Left, targ)

		if x.Op == cc.And && x.Right.Op == cc.Twid {
			x.Op = AndNot
			x.Right = x.Right.Left
		}

		if x.Op == cc.Add && isSliceStringOrArray(left) {
			fixGoTypesExpr(fn, x.Right, nil)
			x.Op = ExprSlice
			x.List = []*cc.Expr{x.Left, x.Right, nil}
			x.Left = nil
			x.Right = nil
			if left.Kind == cc.Array {
				left = &cc.Type{Kind: Slice, Base: left.Base}
			}
			return left
		}

		right := fixGoTypesExpr(fn, x.Right, targ)
		return fixBinary(fn, x, left, right, targ)

	case cc.AddEq, cc.AndEq, cc.DivEq, cc.Eq, cc.ModEq, cc.MulEq, cc.OrEq, cc.SubEq, cc.XorEq:
		left := fixGoTypesExpr(fn, x.Left, nil)

		if x.Op == cc.AndEq && x.Right.Op == cc.Twid {
			x.Op = AndNotEq
			x.Right = x.Right.Left
		}

		if x.Op == cc.AddEq && isSliceOrString(left) {
			fixGoTypesExpr(fn, x.Right, nil)
			old := copyExpr(x.Left)
			x.Op = cc.Eq
			x.Right = &cc.Expr{Op: ExprSlice, List: []*cc.Expr{old, x.Right, nil}}
			return left
		}

		if x.Op == cc.Eq && x.Left.Op == cc.Index && sameType(x.Left.Left.XType, stringType) && GoString(x.Left.Right) == "0" && GoString(x.Right) == "0" {
			x.Left = x.Left.Left
			x.Right = &cc.Expr{Op: cc.Name, Text: `""`}
			return x.Left.XType
		}

		forceGoType(fn, x.Right, left)

		if x.Op == cc.Eq && x.Left != nil && x.Right != nil && x.Right.XType != nil && isCall(x.Right, "make") && x.Left.XDecl != nil && x.Left.XDecl.Type != nil && x.Left.XDecl.Type.Kind == cc.Ptr && sameType(x.Left.XDecl.Type.Base, x.Right.XType.Base) {
			x.Left.XDecl.Type = x.Right.XType
			x.Left.XType = x.Right.XType
			left = x.Right.XType
		}

		return left

	case ColonEq:
		left := fixGoTypesExpr(fn, x.Right, nil)
		x.Left.XType = left
		x.Left.XDecl.Type = left
		return left

	case cc.Addr:
		left := fixGoTypesExpr(fn, x.Left, nil)
		if left == nil {
			return nil
		}

		if targ != nil && targ.Kind == Slice && sameType(targ.Base, left) {
			l := x.Left
			l.Op = ExprSlice
			l.List = []*cc.Expr{l.Left, l.Right, nil}
			l.Left = nil
			l.Right = nil
			fixMerge(x, l)
			return targ
		}

		return &cc.Type{Kind: cc.Ptr, Base: left}

	case cc.AndAnd, cc.OrOr, cc.Not:
		fixGoTypesExpr(fn, x.Left, boolType)
		if x.Right != nil {
			fixGoTypesExpr(fn, x.Right, boolType)
		}
		return boolType

	case cc.Arrow, cc.Dot:
		left := fixGoTypesExpr(fn, x.Left, nil)

		if x.Op == cc.Arrow && isSliceOrString(left) {
			x.Left = &cc.Expr{Op: cc.Index, Left: x.Left, Right: &cc.Expr{Op: cc.Number, Text: "0"}}
		}

		return x.XDecl.Type

	case cc.Call:
		if fixPrintf(fn, x) {
			return x.XType
		}
		if fixSpecialCall(fn, x, targ) {
			return x.XType
		}
		left := fixGoTypesExpr(fn, x.Left, nil)
		for i, y := range x.List {
			if left != nil && left.Kind == cc.Func && i < len(left.Decls) {
				forceGoType(fn, y, left.Decls[i].Type)
			} else {
				fixGoTypesExpr(fn, y, nil)
			}
		}
		if left != nil && left.Kind == cc.Func {
			return left.Base
		}
		if left != nil && left.Kind == cc.Ptr && left.Base.Kind == cc.Func {
			return left.Base.Base
		}
		return nil

	case cc.Cast:
		fixGoTypesExpr(fn, x.Left, nil)
		if isEmptyInterface(x.Left.XType) {
			x.Op = TypeAssert
		}
		return x.Type

	case cc.CastInit:
		fixGoTypesInit(nil, x.Init)
		return x.Type

	case cc.EqEq, cc.Gt, cc.GtEq, cc.Lt, cc.LtEq, cc.NotEq:
		if fixSpecialCompare(fn, x) {
			return boolType
		}
		left := fixGoTypesExpr(fn, x.Left, nil)
		if x.Right.Op == cc.Number && x.Right.Text == "0" || x.Right.Op == cc.Name && x.Right.Text == "nil" {
			if isSliceOrPtr(left) {
				x.Right.Op = cc.Name
				x.Right.Text = "nil"
				return boolType
			}
			if left != nil && left.Kind == String {
				x.Right.Op = cc.String
				x.Right.Texts = []string{`""`}
				return boolType
			}
		}
		right := fixGoTypesExpr(fn, x.Right, nil)

		if isSliceOrArray(x.Left.XType) && isSliceOrArray(x.Right.XType) {
			x.Left = &cc.Expr{Op: cc.Minus, Left: &cc.Expr{Op: cc.Call, Left: &cc.Expr{Op: cc.Name, Text: "cap"}, List: []*cc.Expr{x.Left}}}
			x.Right = &cc.Expr{Op: cc.Minus, Left: &cc.Expr{Op: cc.Call, Left: &cc.Expr{Op: cc.Name, Text: "cap"}, List: []*cc.Expr{x.Right}}}
			return boolType
		}

		fixBinary(fn, x, left, right, nil)
		return boolType

	case cc.Index, cc.Indir:
		left := fixGoTypesExpr(fn, x.Left, nil)
		if x.Right != nil {
			fixGoTypesExpr(fn, x.Right, nil)
		}
		if left == nil {
			return nil
		}

		if isSliceOrString(left) && x.Op == cc.Indir {
			x.Op = cc.Index
			x.Right = &cc.Expr{Op: cc.Number, Text: "0"}
		}

		switch left.Kind {
		case cc.Ptr, Slice, cc.Array:
			if x.Op == cc.Indir && left.Kind == cc.Ptr && left.Base.Kind == cc.Func {
				*x = *x.Left
			}
			return left.Base

		case String:
			return byteType
		}
		return nil

	case cc.Lsh, cc.Rsh:
		left := fixGoTypesExpr(fn, x.Left, targ)
		if left != nil && targ != nil && Int8 <= left.Kind && left.Kind <= Float64 && targ.Kind > left.Kind {
			forceConvert(fn, x.Left, left, targ)
			left = targ
		}
		fixShiftCount(fn, x.Right)
		return left

	case cc.LshEq, cc.RshEq:
		left := fixGoTypesExpr(fn, x.Left, nil)
		fixShiftCount(fn, x.Right)
		return left

	case cc.Name:
		if x.Text == "nelem" {
		}
		switch x.Text {
		case "T", "S", "N", "L", "P":
			x.Text = "nil"
			x.XDecl = nil
			return nil
		case "nelem":
			x.Text = "len"
			x.XDecl = nil
			fallthrough
		case "len":
			return &cc.Type{Kind: cc.Func, Base: intType}
		}
		if x.XDecl == nil {
			return nil
		}
		return x.XDecl.Type

	case cc.Number:
		return idealType

	case cc.Minus, cc.Plus, cc.Twid:
		return fixGoTypesExpr(fn, x.Left, targ)

	case cc.Offsetof:
		// TODO
		return nil

	case cc.Paren:
		return fixGoTypesExpr(fn, x.Left, targ)

	case cc.PostDec, cc.PostInc:
		left := fixGoTypesExpr(fn, x.Left, nil)

		if x.Op == cc.PostInc && isSliceOrString(left) {
			old := copyExpr(x.Left)
			x.Op = cc.Eq
			x.Right = &cc.Expr{Op: ExprSlice, List: []*cc.Expr{old, &cc.Expr{Op: cc.Number, Text: "1"}, nil}}
		}

		return nil

	case cc.SizeofExpr:
		left := fixGoTypesExpr(fn, x.Left, nil)
		if left != nil && (left.Kind == cc.Array || left.Kind == Slice) && left.Base.Def().Is(Uint8) {
			x.Op = cc.Call
			x.List = []*cc.Expr{x.Left}
			x.Left = &cc.Expr{Op: cc.Name, Text: "len"}
			return intType
		}
		return nil

	case cc.SizeofType:
		return nil

	case cc.String:
		return &cc.Type{Kind: String}

	case cc.VaArg:
		// TODO
		return nil
	}
}

func forceGoType(fn *cc.Decl, x *cc.Expr, targ *cc.Type) {
	actual := fixGoTypesExpr(fn, x, targ)
	forceConvert(fn, x, actual, targ)
}

func forceConvert(fn *cc.Decl, x *cc.Expr, actual, targ *cc.Type) {
	if isEmptyInterface(targ) {
		return
	}
	if isEmptyInterface(actual) {
		old := copyExpr(x)
		x.Op = TypeAssert
		x.Left = old
		x.Right = nil
		x.List = nil
		x.Type = targ
		x.XType = targ
		return
	}

	if isNumericConst(x) && targ != nil {
		switch targ.Kind {
		case cc.Ptr, Slice:
			if x.Op == cc.Number && x.Text == "0" {
				x.Op = cc.Name
				x.Text = "nil"
				x.XType = targ
			}
		case String:
			if x.Op == cc.Number && x.Text == "0" {
				x.Op = cc.Name
				x.Text = `""`
				x.XType = targ
			}
		}
		return
	}

	if x.Op == cc.Name && x.Text == "nil" && targ != nil {
		switch targ.Kind {
		case cc.Func, cc.Ptr, Slice:
			return
		case String:
			x.Text = `""`
			x.XType = targ
			x.XDecl = nil
			return
		}
	}

	if actual == nil || targ == nil {
		return
	}

	if actual.Kind == Ideal && Int8 <= targ.Kind && targ.Kind <= Float64 {
		return
	}

	if x != nil && x.Op == cc.Name && x.Text == "nil" {
		if targ.Kind == cc.Func || targ.Kind == cc.Ptr || targ.Kind == Slice {
			return
		}
	}

	// Func conversions are never useful.
	// If the func types are different, the conversion will fail;
	// if not, the conversion is unnecessary.
	// Either way the conversion is an eyesore.
	if targ.Kind == cc.Func || targ.Kind == cc.Ptr && targ.Base.Kind == cc.Func {
		return
	}

	if actual.Kind == Bool && Int8 <= targ.Kind && targ.Kind <= Float64 {
		old := copyExpr(x)
		if targ.Kind == Int {
			x.Op = cc.Call
			x.Left = &cc.Expr{Op: cc.Name, Text: "bool2int"}
			x.List = []*cc.Expr{old}
			x.Right = nil
		} else {
			x.Op = cc.Cast
			x.Left = &cc.Expr{Op: cc.Call, Left: &cc.Expr{Op: cc.Name, Text: "bool2int"}, List: []*cc.Expr{old}}
			x.Type = targ
		}
		return
	}

	if actual.Kind == cc.Array && targ.Kind == Slice && sameType(actual.Base, targ.Base) {
		old := copyExpr(x)
		x.Op = ExprSlice
		x.List = []*cc.Expr{old, nil, nil}
		x.Left = nil
		x.Right = nil
		return
	}

	if actual.Kind == Slice && targ.Kind == cc.Ptr && sameType(actual.Base, targ.Base) {
		if isCall(x, "make") {
			return
		}
		old := copyExpr(x)
		x.Op = cc.Addr
		x.Left = &cc.Expr{Op: cc.Index, Left: old, Right: &cc.Expr{Op: cc.Number, Text: "0"}}
		return
	}

	if !sameType(actual, targ) {
		old := copyExpr(x)
		// for debugging:
		// old = &cc.Expr{Op: cc.Cast, Left: old, Type: actual, XType: actual}
		x.Op = cc.Cast
		x.Left = old
		x.Right = nil
		x.List = nil
		x.Type = targ
		x.XType = targ
		if actual.Kind == cc.Array && targ.Kind == Slice {
			x.Op = ExprSlice
			x.List = []*cc.Expr{old, nil, nil}
			x.Left = nil
			x.Type = nil
		}
	}
}

func fixShiftCount(fn *cc.Decl, x *cc.Expr) {
	typ := fixGoTypesExpr(fn, x, nil)
	if typ == nil {
		return
	}
	switch typ.Kind {
	case Uint8, Uint16, Uint32, Uint64, Uint, Uintptr, Byte:
		return
	}
	if typ.Kind == Int64 {
		forceConvert(fn, x, typ, uint64Type)
		return
	}
	forceConvert(fn, x, typ, uintType)
}

func fixBinary(fn *cc.Decl, x *cc.Expr, left, right, targ *cc.Type) *cc.Type {
	if left == nil || right == nil {
		return nil
	}

	if left.Kind != Ideal && (left.Kind < Int8 || left.Kind > Float64) {
		return nil
	}
	if right.Kind != Ideal && (right.Kind < Int8 || right.Kind > Float64) {
		return nil
	}

	if targ != nil && (targ.Kind < Int8 || targ.Kind > Float64) {
		return nil
	}

	// Want to do everything at as high a precision as possible for as long as possible.
	// If target is wider, convert early.
	// If target is narrower, don't convert at all - let next step do it.
	// Must make left and right match.
	// Convert to largest of three.
	t := left
	if t.Kind == Ideal || t.Kind < right.Kind && right.Kind != Ideal {
		t = right
	}
	if targ != nil && (t.Kind == Ideal || t.Kind < targ.Kind && targ.Kind != Ideal) {
		t = targ
	}
	if !sameType(t, left) {
		forceConvert(fn, x.Left, left, t)
	}
	if !sameType(t, right) {
		forceConvert(fn, x.Right, right, t)
	}
	return t
}

func fixSpecialCall(fn *cc.Decl, x *cc.Expr, targ *cc.Type) bool {
	if x.Left.Op != cc.Name {
		return false
	}
	switch x.Left.Text {
	case "memmove":
		if len(x.List) != 3 {
			fprintf(x.Span, "unsupported %v", x)
			return false
		}
		siz := x.List[2]
		if siz.Op == cc.Number && siz.Text == "4" {
			obj1, obj1Type := objIndir(fn, x.List[0])
			obj2, obj2Type := objIndir(fn, x.List[1])
			if obj1Type == nil || obj2Type == nil {
				fprintf(x.Span, "unsupported %v - missing types", x)
				return true
			}
			if (obj1Type.Kind == Uint32 || obj1Type.Kind == Int32) && obj2Type.Kind == Float32 {
				x.Op = cc.Eq
				x.Left = obj1
				x.Right = &cc.Expr{
					Op: cc.Call,
					Left: &cc.Expr{Op: cc.Name,
						Text: "math.Float32bits",
					},
					List: []*cc.Expr{obj2},
				}
				x.XType = uint32Type
				return true
			}
			fprintf(x.Span, "unsupported %v - size 4 type %v %v", x, GoString(obj1Type), GoString(obj2Type))
		}
		if siz.Op == cc.Number && siz.Text == "8" {
			obj1, obj1Type := objIndir(fn, x.List[0])
			obj2, obj2Type := objIndir(fn, x.List[1])
			if obj1Type == nil || obj2Type == nil {
				fprintf(x.Span, "unsupported %v - missing types", x)
				return true
			}
			if (obj1Type.Kind == Uint64 || obj1Type.Kind == Int64) && obj2Type.Kind == Float64 {
				x.Op = cc.Eq
				x.Left = obj1
				x.Right = &cc.Expr{
					Op: cc.Call,
					Left: &cc.Expr{Op: cc.Name,
						Text: "math.Float64bits",
					},
					List: []*cc.Expr{obj2},
				}
				x.XType = uint64Type
				return true
			}
			fprintf(x.Span, "unsupported %v - size 8 type %v %v", x, GoString(obj1Type), GoString(obj2Type))
		}
		if siz.Op == cc.SizeofExpr {
			obj1Type := fixGoTypesExpr(fn, x.List[0], nil)
			obj2Type := fixGoTypesExpr(fn, x.List[1], nil)
			sizeType := fixGoTypesExpr(fn, siz.Left, nil)
			if obj1Type == nil || obj2Type == nil {
				fprintf(x.Span, "unsupported %v - bad types", x)
				return true
			}
			if obj2Type.Kind == cc.Array && sameType(obj2Type, sizeType) || obj2Type.Kind == Slice && GoString(x.List[1]) == GoString(siz.Left) {
				x.Left.Text = "copy"
				x.Left.XDecl = nil
				x.List = x.List[:2]
				return true
			}
			fprintf(x.Span, "unsupported %v - not array %v %v", x, GoString(obj2Type), GoString(sizeType))
			return true
		}
		left := fixGoTypesExpr(fn, x.List[0], nil)
		right := fixGoTypesExpr(fn, x.List[1], nil)
		fixGoTypesExpr(fn, siz, nil)
		if isSliceOrArray(left) && isSliceOrArray(right) && left.Base.Is(Uint8) && right.Base.Is(Uint8) {
			x.Left.Text = "copy"
			x.Left.XDecl = nil
			if x.List[1].Op == ExprSlice && x.List[1].List[1] == nil {
				x.List[1].List[2] = siz
			} else {
				x.List[1] = &cc.Expr{Op: ExprSlice, List: []*cc.Expr{x.List[1], nil, siz}}
			}
			x.List = x.List[:2]
			return true
		}
		fprintf(x.Span, "unsupported %v (%v %v)", x, GoString(left), GoString(right))
		return true

	case "mal", "malloc", "emallocz", "xmalloc":
		if len(x.List) != 1 {
			fprintf(x.Span, "unsupported %v - too many args", x)
			return false
		}
		siz := x.List[0]
		var count *cc.Expr
		if siz.Op == cc.Mul {
			count = siz.Left
			siz = siz.Right
			if count.Op == cc.SizeofType || count.Op == cc.SizeofExpr {
				count, siz = siz, count
			}
		}
		var typ *cc.Type
		switch siz.Op {
		default:
			typ = byteType
			count = siz

		case cc.SizeofExpr:
			typ = fixGoTypesExpr(fn, siz.Left, nil)
			if typ == nil {
				fprintf(siz.Span, "failed to type check %v", siz.Left)
			}

		case cc.SizeofType:
			typ = siz.Type
			if typ == nil {
				fprintf(siz.Span, "sizeoftype missing type")
			}
		}
		if typ == nil {
			fprintf(x.Span, "unsupported %v - cannot understand type", x)
			return true
		}
		if count == nil {
			x.Left.Text = "new"
			x.Left.XDecl = nil
			x.List = []*cc.Expr{&cc.Expr{Op: ExprType, Type: typ}}
			x.XType = &cc.Type{Kind: cc.Ptr, Base: typ}
		} else {
			x.Left.Text = "make"
			x.Left.XDecl = nil
			x.XType = &cc.Type{Kind: Slice, Base: typ}
			x.List = []*cc.Expr{
				&cc.Expr{Op: ExprType, Type: x.XType},
				count,
			}
		}
		return true

	case "strdup", "estrdup":
		if len(x.List) != 1 {
			fprintf(x.Span, "unsupported %v - too many args", x)
			return false
		}
		fixGoTypesExpr(fn, x.List[0], stringType)
		fixMerge(x, x.List[0])
		x.XType = stringType
		return true

	case "strcpy", "strcat", "fmtstrcpy":
		if len(x.List) != 2 {
			fprintf(x.Span, "unsupported %v - too many args", x)
			return false
		}
		fixGoTypesExpr(fn, x.List[0], nil)
		fixGoTypesExpr(fn, x.List[1], stringType)
		x.Op = cc.Eq
		if x.Left.Text == "strcat" || x.Left.Text == "fmtstrcpy" {
			x.Op = cc.AddEq
		}
		x.Left = x.List[0]
		x.Right = x.List[1]
		x.XType = stringType
		return true

	case "strlen":
		x.Left.Text = "len"
		x.Left.XDecl = nil
		x.XType = intType
		return true

	case "TUP", "CASE":
		if len(x.List) != 2 {
			fprintf(x.Span, "unsupported %v - too many args", x)
			return false
		}
		left := fixGoTypesExpr(fn, x.List[0], targ)
		right := fixGoTypesExpr(fn, x.List[1], targ)
		forceConvert(fn, x.List[0], left, uint32Type)
		forceConvert(fn, x.List[1], right, uint32Type)
		x.Op = cc.Or
		x.Left = &cc.Expr{Op: cc.Lsh, Left: x.List[0], Right: &cc.Expr{Op: cc.Number, Text: "16"}, XType: left}
		x.Right = x.List[1]
		x.List = nil
		x.XType = uint32Type
		return true
	}

	return false
}

func fixMemset(prog *cc.Prog, fn *cc.Decl, stmt *cc.Stmt) {
	x := stmt.Expr
	if len(x.List) != 3 || x.List[1].Op != cc.Number || x.List[1].Text != "0" {
		fprintf(x.Span, "unsupported %v - nonzero", x)
		return
	}

	if x.List[2].Op == cc.SizeofExpr || x.List[2].Op == cc.SizeofType {
		obj, objType := objIndir(fn, x.List[0])
		if !matchSize(fn, obj, objType, x.List[2]) {
			fprintf(x.Span, "unsupported %v - wrong size", x)
			return
		}

		x.Op = cc.Eq
		x.Left = obj
		x.Right = zeroFor(objType)
		x.List = nil
		return
	}

	siz := x.List[2]
	var count *cc.Expr
	var objType *cc.Type
	if siz.Op == cc.Mul {
		count = siz.Left
		siz = siz.Right
		if siz.Op != cc.SizeofExpr && siz.Op != cc.SizeofType {
			fprintf(x.Span, "unsupported %v - wrong array size", x)
			return
		}

		switch siz.Op {
		case cc.SizeofExpr:
			p := unparen(siz.Left)
			if p.Op != cc.Indir && p.Op != cc.Index || !sameType(p.Left.XType, x.List[0].XType) {
				fprintf(x.Span, "unsupported %v - wrong size", x)
			}
			objType = fixGoTypesExpr(fn, x.List[0], nil)
		case cc.SizeofType:
			objType = fixGoTypesExpr(fn, x.List[0], nil)
			if !sameType(siz.Type, objType.Base) {
				fprintf(x.Span, "unsupported %v - wrong size", x)
			}
		}
	} else {
		count = siz
		objType = fixGoTypesExpr(fn, x.List[0], nil)
		if !objType.Base.Is(Byte) && !objType.Base.Is(Uint8) {
			fprintf(x.Span, "unsupported %v - wrong size form for non-byte type", x)
			return
		}
	}

	// Found it. Replace with zeroing for loop.
	stmt.Op = cc.For
	stmt.Pre = &cc.Expr{
		Op: cc.Eq,
		Left: &cc.Expr{
			Op:    cc.Name,
			Text:  "i",
			XType: intType,
		},
		Right: &cc.Expr{
			Op:    cc.Number,
			Text:  "0",
			XType: intType,
		},
		XType: boolType,
	}
	stmt.Expr = &cc.Expr{
		Op: cc.Lt,
		Left: &cc.Expr{
			Op:    cc.Name,
			Text:  "i",
			XType: intType,
		},
		Right: count,
		XType: boolType,
	}
	stmt.Post = &cc.Expr{
		Op: cc.PostInc,
		Left: &cc.Expr{
			Op:    cc.Name,
			Text:  "i",
			XType: intType,
		},
		XType: intType,
	}
	stmt.Body = &cc.Stmt{
		Op: cc.Block,
		Block: []*cc.Stmt{
			{
				Op: cc.StmtExpr,
				Expr: &cc.Expr{
					Op: cc.Eq,
					Left: &cc.Expr{
						Op:    cc.Index,
						Left:  x.List[0],
						Right: &cc.Expr{Op: cc.Name, Text: "i"},
					},
					Right: zeroFor(objType.Base),
				},
			},
		},
	}
	return
}

func fixSpecialCompare(fn *cc.Decl, x *cc.Expr) bool {
	if (x.Right.Op != cc.Number || x.Right.Text != "0") && x.Right.String() != "nil" || x.Left.Op != cc.Call || x.Left.Left.Op != cc.Name {
		return false
	}

	call := x.Left
	switch call.Left.Text {
	case "memcmp":
		if len(call.List) != 3 {
			fprintf(x.Span, "unsupported %v", x)
			return false
		}
		obj1, obj1Type := objIndir(fn, call.List[0])
		obj2, obj2Type := objIndir(fn, call.List[1])
		if obj1Type == nil || !sameType(obj1Type, obj2Type) {
			fprintf(x.Span, "unsupported %v", call)
			return true
		}

		if !matchSize(fn, obj1, obj1Type, call.List[2]) && !matchSize(fn, obj2, obj2Type, call.List[2]) {
			fprintf(x.Span, "unsupported %v - wrong size", call)
			return true
		}

		x.Left = obj1
		x.Right = obj2
		x.List = nil
		x.XType = boolType
		return true

	case "strncmp":
		if len(call.List) != 3 {
			fprintf(x.Span, "unsupported %v", x)
			return false
		}
		call.Left = &cc.Expr{Op: cc.Name, Text: "strings.HasPrefix"}
		call.List = call.List[:2]
		call.XType = boolType
		if x.Op == cc.EqEq {
			*x = *call
		} else if x.Op == cc.NotEq {
			x.Op = cc.Not
			x.Right = nil
		}
		x.XType = boolType
		return true

	case "strstr":
		if len(call.List) != 2 {
			fprintf(x.Span, "unsupported %v", x)
			return false
		}
		call.Left = &cc.Expr{Op: cc.Name, Text: "strings.Contains"}
		call.XType = boolType
		if x.Op == cc.NotEq {
			*x = *call
		} else if x.Op == cc.EqEq {
			x.Op = cc.Not
			x.Right = nil
		}
		x.XType = boolType
		return true

	case "strcmp":
		if len(call.List) != 2 {
			fprintf(x.Span, "unsupported %v", x)
			return false
		}
		obj1 := call.List[0]
		obj2 := call.List[1]

		x.Left = obj1
		x.Right = obj2
		x.List = nil
		x.XType = boolType
		return true
	}

	return false
}

func fixBools(prog *cc.Prog) {
	cc.Preorder(prog, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Expr:
			if x.Op == cc.Not {
				switch x.Left.Op {
				case cc.OrOr, cc.AndAnd, cc.EqEq, cc.NotEq, cc.LtEq, cc.GtEq, cc.Lt, cc.Gt, cc.Paren:
					x.Left = negate(x.Left)
					fixMerge(x, x.Left)
				}
			}
		}
	})
}

func unparen(x *cc.Expr) *cc.Expr {
	for x != nil && x.Op == cc.Paren {
		x = x.Left
	}
	return x
}

func negate(x *cc.Expr) *cc.Expr {
	switch x.Op {
	case cc.Paren:
		return negate(x.Left)
	case cc.OrOr:
		x.Op = cc.AndAnd
		x.Left = negate(x.Left)
		x.Right = negate(x.Right)
	case cc.AndAnd:
		x.Op = cc.OrOr
		x.Left = negate(x.Left)
		x.Right = negate(x.Right)
	case cc.EqEq:
		x.Op = cc.NotEq
	case cc.NotEq:
		x.Op = cc.EqEq
	case cc.Lt:
		x.Op = cc.GtEq
	case cc.GtEq:
		x.Op = cc.Lt
	case cc.Gt:
		x.Op = cc.LtEq
	case cc.LtEq:
		x.Op = cc.Gt
	default:
		x = &cc.Expr{Op: cc.Not, Left: x}
	}
	return x
}

func objIndir(fn *cc.Decl, x *cc.Expr) (*cc.Expr, *cc.Type) {
	objType := fixGoTypesExpr(fn, x, nil)
	obj := x
	if obj.XType != nil && obj.XType.Kind == cc.Array {
		// obj stays as is
	} else if obj.Op == cc.Addr {
		obj = obj.Left
		if objType != nil {
			objType = objType.Base
		}
	} else {
		obj = &cc.Expr{Op: cc.Indir, Left: obj}
		if objType != nil {
			objType = objType.Base
		}
	}
	if objType == nil {
		objType = obj.XType
	}
	return obj, objType
}

func matchSize(fn *cc.Decl, obj *cc.Expr, objType *cc.Type, siz *cc.Expr) bool {
	switch siz.Op {
	default:
		return false

	case cc.SizeofType:
		// ok if sizeof type of first arg
		return sameType(siz.Type, objType)

	case cc.SizeofExpr:
		// ok if sizeof *firstarg
		y := siz.Left
		if y.Op == cc.Paren {
			y = y.Left
		}
		return obj.String() == y.String()
	}
}

func sameType(t, u *cc.Type) bool {
	t = t.Def()
	u = u.Def()
	if t == u {
		return true
	}
	if t == nil || u == nil {
		return false
	}
	if t.Kind != u.Kind {
		return false
	}
	if t.Name != "" || u.Name != "" {
		return t.Name == u.Name
	}
	if !sameType(t.Base, u.Base) || len(t.Decls) != len(u.Decls) {
		return false
	}
	for i, td := range t.Decls {
		ud := u.Decls[i]
		if !sameType(td.Type, ud.Type) || t.Kind == cc.Struct && td.Name != ud.Name {
			return false
		}
	}
	return true
}

func isSliceOrString(typ *cc.Type) bool {
	return typ != nil && (typ.Kind == Slice || typ.Kind == String)
}

func isSliceStringOrArray(typ *cc.Type) bool {
	return typ != nil && (typ.Kind == Slice || typ.Kind == String || typ.Kind == cc.Array)
}

func isSliceOrPtr(typ *cc.Type) bool {
	return typ != nil && (typ.Kind == Slice || typ.Kind == cc.Ptr)
}

func isPtrSliceOrArray(typ *cc.Type) bool {
	return typ != nil && (typ.Kind == cc.Ptr || typ.Kind == cc.Array || typ.Kind == Slice)
}

func isSliceOrArray(typ *cc.Type) bool {
	return typ != nil && (typ.Kind == Slice || typ.Kind == cc.Array)
}

func isNumericConst(x *cc.Expr) bool {
	switch x.Op {
	case cc.Number:
		return true
	case cc.Name:
		// TODO
	case cc.Add, cc.And, cc.Div, cc.Mod, cc.Mul, cc.Or, cc.Sub, cc.Xor, cc.Lsh, cc.Rsh:
		return isNumericConst(x.Left) && isNumericConst(x.Right)
	case cc.Plus, cc.Minus, cc.Twid:
		return isNumericConst(x.Left)
	}
	return false
}

func isEmptyInterface(t *cc.Type) bool {
	return t != nil && t.Kind == cc.TypedefType && t.Name == "interface{}"
}

func zeroFor(targ *cc.Type) *cc.Expr {
	if targ != nil {
		k := targ.Def().Kind
		switch k {
		case String:
			return &cc.Expr{Op: cc.String, Texts: []string{`""`}}

		case Slice, cc.Ptr:
			return &cc.Expr{Op: cc.Name, Text: "nil"}

		case cc.Struct, cc.Array:
			return &cc.Expr{Op: cc.CastInit, Type: targ, Init: &cc.Init{}}

		case Bool:
			return &cc.Expr{Op: cc.Name, Text: "false"}
		}

		if Int8 <= k && k <= Float64 {
			return &cc.Expr{Op: cc.Number, Text: "0"}
		}
		return &cc.Expr{Op: cc.Number, Text: "0 /*" + targ.String() + "*/"}
	}

	return &cc.Expr{Op: cc.Number, Text: "0 /*untyped*/"}
}

// rewriteLen rewrites references to length/capacity fields to use len(f) and cap(f) instead.
func rewriteLen(cfg *Config, prog *cc.Prog) {
	cc.Postorder(prog, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Expr:
			// Assign to len, generated by len rewrite. Change to reslice.
			if x.Op == cc.Eq && x.Left.Op == cc.Call && x.Left.Left.Op == cc.Name && x.Left.Left.Text == "len" && len(x.Left.List) > 0 {
				arg := x.Left.List[0]
				x.Left = arg
				x.Right = &cc.Expr{
					Op:   ExprSlice,
					List: []*cc.Expr{arg, nil, x.Right},
				}
				return
			}

			if x.Op == cc.Call {
				// Rewrite call with args x, len(x) to drop len(x).
				var out []*cc.Expr
				for _, arg := range x.List {
					if arg.Op == cc.Call && arg.Left.Op == cc.Name && arg.Left.Text == "len" && len(arg.List) == 1 && len(out) > 0 && arg.List[0].String() == out[len(out)-1].String() {
						continue
					}
					out = append(out, arg)
				}
				x.List = out
			}

			if (x.Op == cc.Arrow || x.Op == cc.Dot) && x.XDecl != nil {
				k := declKey(x.XDecl)
				name := cfg.len[k]
				op := "len"
				if name == "" {
					name = cfg.cap[k]
					op = "cap"
					if name == "" {
						return
					}
				}
				d := x.XDecl
				if d.OuterType == nil {
					fmt.Fprintf(os.Stderr, "found use of %s but missing type\n", k)
					return
				}
				t := d.OuterType
				var other *cc.Decl
				if i := strings.Index(name, "."); i >= 0 {
					name = name[i+1:]
				}
				for _, dd := range t.Decls {
					if dd.Name == name {
						other = dd
						break
					}
				}
				if other == nil {
					fmt.Fprintf(os.Stderr, "found use of %s but cannot find field %s\n", k, name)
					return
				}
				left := x.Left
				x.Op = cc.Call
				x.Left = &cc.Expr{
					Op:    cc.Name,
					Text:  op,
					XType: &cc.Type{Kind: cc.Func, Base: intType},
				}
				x.List = []*cc.Expr{
					{
						Op:    cc.Dot,
						Left:  left,
						Text:  name,
						XDecl: other,
					},
				}
			}
		}
	})

	cc.Postorder(prog, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Type:
			out := x.Decls[:0]
			for _, d := range x.Decls {
				k := declKey(d)
				if cfg.len[k] == "" && cfg.cap[k] == "" && !cfg.delete[k] {
					out = append(out, d)
				}
			}
			x.Decls = out
		}
	})
}
