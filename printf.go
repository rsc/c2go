// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"rsc.io/c2go/cc"
)

func tryPrintf(curfn *cc.Decl, x *cc.Expr, name string, fmtpos int, newName string) bool {
	if (x.Left.Text == name || (strings.Contains(name, ".") || strings.Contains(name, "->")) && x.Left.String() == name) && len(x.List) >= fmtpos+1 && x.List[fmtpos].Op == cc.String {
		x.List = append(x.List[:fmtpos+1], fixPrintFormat(curfn, x.List[fmtpos], x.List[fmtpos+1:])...)
		if newName != "" {
			x.Left.Text = newName
			x.Left.XDecl = nil
		}
		return true
	}
	return false
}

func fixPrintf(curfn *cc.Decl, x *cc.Expr) bool {
	if x.Op != cc.Call {
		return false
	}
	if tryPrintf(curfn, x, "sprint", 1, "fmt.Sprintf") {
		targ := x.List[0]
		x.List = x.List[1:]
		x.Right = copyExpr(x)
		x.List = nil
		x.Left = targ
		x.Op = cc.Eq

		// sprint(strchr(s, 0), "hello") => s += "hello"
		if targ.Op == cc.Call && len(targ.List) == 2 && targ.Left.Text == "strchr" && targ.List[1].Text == "0" {
			x.Left = targ.List[0]
			x.Op = cc.AddEq
		} else if targ.Op == cc.Add && targ.Right.Op == cc.Call && targ.Right.Left.Text == "strlen" && len(targ.Right.List) == 1 && GoString(targ.Left) == GoString(targ.Right.List[0]) {
			x.Left = targ.Left
			x.Op = cc.AddEq
		}
		return true
	}
	if tryPrintf(curfn, x, "snprint", 2, "fmt.Sprintf") {
		targ := x.List[0]
		x.List = x.List[2:]
		x.Right = copyExpr(x)
		x.List = nil
		x.Left = targ
		x.Op = cc.Eq
		return true
	}
	if tryPrintf(curfn, x, "fmtprint", 1, "fmt.Sprintf") {
		targ := x.List[0]
		x.List = x.List[1:]
		x.Right = copyExpr(x)
		x.List = nil
		x.Left = targ
		x.Op = cc.AddEq
		if x.Left.Op == cc.Addr {
			x.Left = x.Left.Left
		}
		return true
	}
	if tryPrintf(curfn, x, "smprint", 0, "fmt.Sprintf") {
		x.XType = stringType
		return true
	}
	if tryPrintf(curfn, x, "print", 0, "fmt.Printf") {
		return true
	}
	if tryPrintf(curfn, x, "sysfatal", 0, "log.Fatalf") {
		return true
	}
	if tryPrintf(curfn, x, "fatal", 0, "") {
		return true
	}
	if tryPrintf(curfn, x, "ctxt->diag", 0, "") {
		return true
	}
	if tryPrintf(curfn, x, "yyerror", 0, "") {
		return true
	}
	if tryPrintf(curfn, x, "Bprint", 1, "fmt.Fprintf") {
		return true
	}

	if isCall(x, "exprfmt") && len(x.List) == 3 {
		// exprfmt(fp, x, y) becomes fp += exprfmt(x, y)
		x.Op = cc.AddEq
		x.Right = &cc.Expr{Op: cc.Call, Left: x.Left, List: x.List[1:]}
		x.Left = x.List[0]
		x.List = nil
		x.XType = stringType
		return true
	}

	if isCall(x, "fmtstrinit") && len(x.List) == 1 && x.List[0].Op == cc.Addr {
		x.Op = cc.Eq
		x.Left = x.List[0].Left
		x.List = nil
		x.Right = &cc.Expr{Op: cc.Name, Text: `""`}
	}

	if isCall(x, "fmtstrflush") && len(x.List) == 1 && x.List[0].Op == cc.Addr {
		x.Op = cc.Paren
		x.Left = x.List[0].Left
		x.List = nil
	}

	return false
}

func fixPrintFormat(curfn *cc.Decl, fx *cc.Expr, args []*cc.Expr) []*cc.Expr {
	for _, arg := range args {
		fixGoTypesExpr(curfn, arg, nil)
		cc.Preorder(arg, func(x cc.Syntax) {
			if x, ok := x.(*cc.Expr); ok && x.Op == cc.Name && strings.HasPrefix(x.Text, "bigP") {
				x.Text = "p"
				x.XDecl = nil
			}
		})
	}
	
	isGC := strings.Contains(fx.Span.Start.File, "cmd/gc")
	isCompiler := isGC || strings.Contains(fx.Span.Start.File, "cmd/6g") || strings.Contains(fx.Span.Start.File, "cmd/8g") ||  strings.Contains(fx.Span.Start.File, "cmd/5g") ||  strings.Contains(fx.Span.Start.File, "cmd/9g")

	narg := 0
	for j, text := range fx.Texts {
		format, err := strconv.Unquote(text)
		if err != nil {
			fprintf(fx.Span, "cannot parse quoted string: %v", err)
			return args
		}

		suffix := ""

		var buf bytes.Buffer
		start := 0
		for i := 0; i < len(format); i++ {
			if format[i] != '%' {
				continue
			}
			buf.WriteString(format[start:i])
			start = i
			i++
			if i < len(format) && format[i] == '%' {
				buf.WriteByte('%')
				start = i + 1
				continue
			}
			for i < len(format) {
				c := format[i]
				switch c {
				case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '#', '-', '.', ',', ' ', 'h', 'l', 'u', '*':
					i++
					continue
				}
				break
			}
			if i >= len(format) {
				fprintf(fx.Span, "print format ends mid-verb")
				return args
			}
			flags, verb := format[start:i], format[i]
			start = i + 1

			allFlags := flags
			_ = allFlags

			flags = strings.Replace(flags, "h", "", -1)
			flags = strings.Replace(flags, "l", "", -1)
			flags = strings.Replace(flags, "u", "", -1)

			if j := strings.Index(flags, "#0"); j >= 0 && verb == 'x' {
				k := j + 2
				for k < len(flags) && '0' <= flags[k] && flags[k] <= '9' {
					k++
				}
				n, _ := strconv.Atoi(flags[j+2 : k])
				flags = flags[:j+2] + fmt.Sprint(n-2) + flags[k:]
			}

			narg += strings.Count(allFlags, "*")

			convert := ""
			switch verb {
			default:
				fprintf(fx.Span, "unrecognized format %s%c", flags, verb)
				buf.WriteString("%")
				buf.WriteString(flags)
				buf.WriteString(string(verb))

			case 'f', 'e', 'E', 'g', 'G', 's', 'c', 'p':
				// usual meanings
				buf.WriteString(flags)
				buf.WriteString(string(verb))

			case 'x', 'X', 'o', 'd', 'b':
				// usual meanings, but force unsigned if u is given
				buf.WriteString(flags)
				buf.WriteString(string(verb))
				if narg >= len(args) || !strings.Contains(allFlags, "u") {
					break
				}
				arg := args[narg]
				if t := arg.XType.Def(); t != nil {
					switch t.Kind {
					case Int8:
						convert = "uint8"
					case Int16:
						convert = "uint16"
					case Int32:
						convert = "uint32"
					case Int64:
						convert = "uint64"
					case Int:
						convert = "uint"
					}
				}

			case 'q': // plan 9 rc(1) quoted string
				buf.WriteString(flags)
				buf.WriteString("%v")
				convert = "plan9quote"

			case 'A': // asm opcode
				if allFlags != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")
				if narg < len(args) {
					forceConvert(nil, args[narg], args[narg].XType, intType)
				}
				convert = "Aconv" + suffix

			case 'L':
				if allFlags != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")
				if narg >= len(args) {
					break
				}
				arg := args[narg]
				if (arg.Op == cc.Dot || arg.Op == cc.Arrow) && arg.Text == "lineno" {
					arg.Text = "Line"
					arg.XDecl = nil
					args[narg] = &cc.Expr{Op: cc.Call, Left: arg, XType: stringType}
					break
				}
				convert = "ctxt.Line"
				if isCompiler {
					if isGC {
						convert = "Ctxt.Line"
					} else {
						convert = "gc.Ctxt.Line"
					}
					forceConvert(curfn, arg, arg.XType, intType)
				}

			case '@':
				if allFlags != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")
				convert = "RAconv" + suffix

			case '^':
				if allFlags != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")
				convert = "DRconv" + suffix

			case 'D':
				if allFlags != "%" && allFlags != "%l" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")
				if narg >= len(args) {
					break
				}
				flag := &cc.Expr{Op: cc.Name, Text: "0"}
				if strings.Contains(allFlags, "l") {
					flag.Text = "fmtLong"
				}
				arg := args[narg]
				args[narg] = &cc.Expr{
					Op:   cc.Call,
					Left: &cc.Expr{Op: cc.Name, Text: "Dconv" + suffix},
					List: []*cc.Expr{
						&cc.Expr{Op: cc.Name, Text: "p"},
						flag,
						arg,
					},
					XType: stringType,
				}
				if isCompiler {
					args[narg].List = args[narg].List[2:]
					args[narg].Left.Text = "Ctxt.Dconv"
					if !isGC {
						args[narg].Left.Text = "gc.Ctxt.Dconv"
					}
				}

			case 'M':
				if allFlags != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")
				convert = "Mconv" + suffix

			case 'R':
				if allFlags != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")
				forceConvert(curfn, args[narg], args[narg].XType, intType)
				convert = "Rconv" + suffix
				if isCompiler {
					convert = "Ctxt." + convert
					if !isGC {
						convert = "gc." + convert
					}
				}

			case '$':
				if allFlags != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%q")

			case 'P':
				if allFlags != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")

			case 'r': // plan 9 errstr
				buf.WriteString("%v")
				if narg > len(args) {
					break
				}
				args = append(append(args[:narg:narg], &cc.Expr{Op: cc.Name, Text: "err"}), args[narg:]...)

			case 'B', 'F', 'H', 'J', 'N', 'O', 'Q', 'S', 'T', 'V', 'Z':
				switch verb {
				case 'O':
					convert = "int"
				}
				f := allFlags
				mod := "0"
				if strings.Contains(f, "-") {
					mod += "|fmtMinus"
					f = strings.Replace(f, "-", "", 1)
				}
				if strings.Contains(f, "h") {
					mod += "|fmtShort"
					f = strings.Replace(f, "h", "", 1)
					if strings.Contains(f, "h") {
						mod += "|fmtByte"
						f = strings.Replace(f, "h", "", 1)
					}
				}
				if strings.Contains(f, "#") {
					mod += "|fmtSharp"
					f = strings.Replace(f, "#", "", 1)
				}
				if strings.Contains(f, "l") {
					mod += "|fmtLong"
					f = strings.Replace(f, "l", "", 1)
				}
				if strings.Contains(f, ",") {
					mod += "|fmtComma"
					f = strings.Replace(f, ",", "", 1)
				}
				if strings.Contains(f, "+") {
					mod += "|fmtPlus"
					f = strings.Replace(f, "+", "", 1)
				}
				if strings.Contains(f, "u") {
					mod += "|fmtUnsigned"
					f = strings.Replace(f, "u", "", 1)
				}
				if f != "%" {
					fprintf(fx.Span, "format %s%c", allFlags, verb)
				}
				buf.WriteString("%v")
				if narg >= len(args) {
					break
				}
				if mod != "0" {
					mod = mod[2:]
				}
				flag := &cc.Expr{Op: cc.Name, Text: mod}
				if convert != "" {
					args[narg] = &cc.Expr{Op: cc.Call, Left: &cc.Expr{Op: cc.Name, Text: convert}, List: []*cc.Expr{args[narg]}, XType: stringType}
					convert = ""
				}
				arg := args[narg]
				args[narg] = &cc.Expr{
					Op:   cc.Call,
					Left: &cc.Expr{Op: cc.Name, Text: string(verb) + "conv"},
					List: []*cc.Expr{
						arg,
						flag,
					},
					XType: stringType,
				}
				if !isGC {
					args[narg].Left.Text = "gc." + args[narg].Left.Text
				}
			}

			if convert != "" && narg < len(args) {
				arg := args[narg]
				args[narg] = &cc.Expr{Op: cc.Call, Left: &cc.Expr{Op: cc.Name, Text: convert}, List: []*cc.Expr{arg}, XType: stringType}
			}

			narg++
		}
		buf.WriteString(format[start:])
		fx.Texts[j] = strconv.Quote(buf.String())
	}

	return args
}

func fixFormatter(fn *cc.Decl) {
	// Find va_arg assignment.
	var arg *cc.Expr
	var argType *cc.Type
	var ps []*cc.Expr
	cc.Preorder(fn.Body, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Expr:
			if x.Op == cc.Name && strings.HasPrefix(x.Text, "bigP") {
				ps = append(ps, x)
			}
		case *cc.Stmt:
			stmt := x
			if stmt.Op != cc.StmtExpr {
				return
			}
			expr := stmt.Expr
			if expr.Op != cc.Eq {
				return
			}
			if expr.Left.Op == cc.Name && strings.HasPrefix(expr.Left.Text, "bigP") {
				stmt.Op = cc.Empty
				stmt.Expr = nil
				return
			}
			if expr.Op != cc.Eq || expr.Right.Op != cc.VaArg {
				return
			}
			if arg != nil {
				fprintf(fn.Span, "multiple va_arg in formatter")
			}
			arg = expr.Left
			argType = expr.Right.Type
			stmt.Op = cc.Empty
		}
	})

	fp := fn.Type.Decls[0]
	fp.Type = stringType
	fn.Type.Base = stringType
	if arg != nil {
		fn.Type.Decls[0] = arg.XDecl
	} else {
		if len(fn.Type.Decls) == 1 {
			fprintf(fn.Span, "missing va_arg in formatter")
			return
		}
		fn.Type.Decls = fn.Type.Decls[1:]
		decl := &cc.Stmt{
			Op:   cc.StmtDecl,
			Decl: fp,
		}
		fn.Body.Block = append([]*cc.Stmt{decl}, fn.Body.Block...)
	}

	if strings.HasPrefix(fn.Name, "Dconv") && len(ps) > 0 {
		pd := &cc.Decl{Name: "p", Type: ps[0].XDecl.Type}
		fd := &cc.Decl{Name: "flag", Type: intType}
		for _, p := range ps {
			p.XDecl = pd
		}
		fn.Type.Decls = []*cc.Decl{
			pd,
			fd,
			arg.XDecl,
		}
	}
	if len(fn.Name) == 5 && strings.HasSuffix(fn.Name, "conv") {
		switch fn.Name[0] {
		case 'B', 'F', 'H', 'J', 'N', 'O', 'Q', 'S', 'T', 'V', 'Z':
			fn.Type.Decls = append(fn.Type.Decls, &cc.Decl{
				Name: "flag",
				Type: intType,
			})
		}
	}

	cc.Preorder(fn.Body, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Stmt:
			if arg != nil && x.Op == cc.StmtDecl && x.Decl == arg.XDecl {
				x.Decl = fp
			}

			if x.Op == cc.Return && x.Expr != nil && x.Expr.Text == "0" {
				x.Expr = &cc.Expr{Op: cc.Name, Text: fp.Name, XDecl: fp}
			}

		case *cc.Expr:
			if x.Op == cc.Arrow && x.Text == "flags" {
				x.Op = cc.Name
				x.Text = "flag"
				x.XDecl = nil
				x.Left = nil
			}

			if x.Op == cc.Name {
				switch x.Text {
				case "FmtLong":
					x.Text = "fmtLong"
					x.XDecl = nil
				}
			}
		}
	})

}

func fixFormatStmt(fn *cc.Decl, x *cc.Stmt) {
	if x.Op == cc.StmtDecl && GoString(x.Decl.Type) == "Fmt" {
		x.Decl.Type = stringType
		return
	}
}
