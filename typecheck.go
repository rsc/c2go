// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"

	"rsc.io/c2go/cc"
)

// Rewrite C types appearing in the program to equivalent Go types.
func rewriteTypes(cfg *Config, prog cc.Syntax) {
	// Cache is needed to cut off translation of recursive types.
	cache := make(map[*cc.Type]*cc.Type)

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
			g.goType = &cc.Type{Kind: k, Base: toGoType(nil, nil, t.Base, cache)}
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
				d.Type.Base = toGoType(nil, d.Type.Base, cache)
				if d.Type.Width == nil {
					d.Type.Kind = Slice
				}
				return
			}
			d.Type = toGoType(d, d.Type, cache)

		case *cc.Expr:
			if x.Type != nil {
				t := toGoType(nil, x.Type, cache)
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

func toGoType(x cc.Syntax, typ *cc.Type, cache map[*cc.Type]*cc.Type) *cc.Type {
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
		return &cc.Type{Kind: c2goKind[typ.Kind]}

	case cc.TypedefType:
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
		t.Base = toGoType(nil, typ.Base, cache)
		return t

	case cc.Array:
		if typ.Base.Def().Kind == cc.Char {
			return &cc.Type{Kind: String}
		}
		t := &cc.Type{Kind: cc.Array, Width: typ.Width}
		cache[typ] = t
		t.Base = toGoType(nil, typ.Base, cache)
		return t

	case cc.Ptr:
		t := &cc.Type{Kind: cc.Ptr}
		cache[typ] = t
		t.Base = toGoType(nil, typ.Base, cache)

		if typ.Base.Kind == cc.Char {
			t.Kind = String
			t.Base = nil
			return t
		}

		return t

	case cc.Func:
		// A func Type contains Decls, and we don't fork the Decls, so don't fork the Type.
		// The Decls themselves appear in the group lists, so they'll be handled by rewriteTypes.
		// The return value has no Decl and needs to be converted.
		if !typ.Base.Is(cc.Void) {
			typ.Base = toGoType(nil, typ.Base, cache)
		}
		return typ

	case cc.Struct:
		// A struct Type contains Decls, and we don't fork the Decls, so don't fork the Type.
		// The Decls themselves appear in the group lists, so they'll be handled by rewriteTypes.
		return typ
	}
}
