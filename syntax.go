// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"

	"rsc.io/c2go/cc"
)

var numRewrite int

// Rewrite from C constructs to Go constructs.
func rewriteSyntax(cfg *Config, prog *cc.Prog) {
	numRewrite++
	cc.Preorder(prog, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Stmt:
			rewriteStmt(x)

		case *cc.Expr:
			switch x.Op {
			case cc.Name:
				switch x.Text {
				case "nil":
					x.XDecl = nil // just nil, not main.Nil
				case "nelem":
					x.Text = "len"
					x.XDecl = nil
				}
			case cc.Number:
				// Rewrite char literal.
				// In general we'd need to rewrite all string and char literals
				// but these are the only forms that comes up.
				switch x.Text {
				case `'\0'`:
					x.Text = `'\x00'`
				case `'\"'`:
					x.Text = `'"'`
				}

			case cc.Paren:
				switch x.Left.Op {
				case cc.Number, cc.Name:
					fixMerge(x, x.Left)
				}

			case cc.OrEq, cc.AndEq, cc.Or, cc.Eq, cc.EqEq, cc.NotEq, cc.LtEq, cc.GtEq, cc.Lt, cc.Gt:
				cutParen(x, cc.Or, cc.And, cc.Lsh, cc.Rsh)
			}

		case *cc.Type:
			// Rewrite int f(void) to int f().
			if x.Kind == cc.Func && len(x.Decls) == 1 && x.Decls[0].Name == "" && x.Decls[0].Type.Is(cc.Void) {
				x.Decls = nil
			}
		}
	})

	// Apply changed struct tags to typedefs.
	// Excise dead pieces.
	cc.Postorder(prog, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Type:
			if x.Kind == cc.TypedefType && x.Base != nil && x.Base.Tag != "" {
				x.Name = x.Base.Tag
			}

		case *cc.Stmt:
			if x.Op == cc.StmtExpr && x.Expr.Op == cc.Comma && len(x.Expr.List) == 0 {
				x.Op = cc.Empty
			}
			x.Block = filterBlock(x.Block)

		case *cc.Expr:
			if x.Op == ExprBlock {
				x.Block = filterBlock(x.Block)
			}

			switch x.Op {
			case cc.Add, cc.Sub:
				// Turn p + y - z, which is really (p + y) - z, into p + (y - z),
				// so that there is only one pointer addition (which will turn into
				// a slice operation using y-z as the index).
				if x.XType != nil && x.XType.Kind == cc.Ptr {
					switch x.Left.Op {
					case cc.Add, cc.Sub:
						if x.Left.XType != nil && x.Left.XType.Kind == cc.Ptr {
							p, op1, y, op2, z := x.Left.Left, x.Left.Op, x.Left.Right, x.Op, x.Right
							if op1 == cc.Sub {
								y = &cc.Expr{Op: cc.Minus, Left: y, XType: y.XType}
							}
							x.Op = cc.Add
							x.Left = p
							x.Right = &cc.Expr{Op: op2, Left: y, Right: z, XType: x.XType}
						}
					}
				}
			}

			// Turn c + p - q, which is really (c + p) - q, into c + (p - q),
			// so that there is no int + ptr addition, only a ptr - ptr subtraction.
			if x.Op == cc.Sub && x.Left.Op == cc.Add && !isPtrOrArray(x.XType) && isPtrOrArray(x.Left.XType) && !isPtrOrArray(x.Left.Left.XType) {
				c, p, q := x.Left.Left, x.Left.Right, x.Right
				expr := x.Left
				expr.Left = p
				expr.Right = q
				expr.Op = cc.Sub
				x.Op = cc.Add
				x.Left = c
				x.Right = expr
				expr.XType = x.XType
			}
		}
	})
}

func cutParen(x *cc.Expr, ops ...cc.ExprOp) {
	if x.Left != nil && x.Left.Op == cc.Paren {
		for _, op := range ops {
			if x.Left.Left.Op == op {
				fixMerge(x.Left, x.Left.Left)
				break
			}
		}
	}
	if x.Right != nil && x.Right.Op == cc.Paren {
		for _, op := range ops {
			if x.Right.Left.Op == op {
				fixMerge(x.Right, x.Right.Left)
				break
			}
		}
	}
}

func isPtrOrArray(t *cc.Type) bool {
	return t != nil && (t.Kind == cc.Ptr || t.Kind == cc.Array)
}

func filterBlock(x []*cc.Stmt) []*cc.Stmt {
	all := x[:0]
	for _, stmt := range x {
		if stmt.Op != cc.Empty || len(stmt.Comments.Before)+len(stmt.Comments.After)+len(stmt.Labels) > 0 {
			all = append(all, stmt)
		}
	}
	return all
}

func rewriteStmt(stmt *cc.Stmt) {
	// TODO: Double-check stmt.Labels

	switch stmt.Op {
	case cc.ARGBEGIN:
		panic(fmt.Sprintf("unexpected ARGBEGIN"))

	case cc.Do:
		// Rewrite do { ... } while(x)
		// to for(;;) { ... if(!x) break }
		// Since rewriteStmt is called in a preorder traversal,
		// the recursion into the children will clean up x
		// in the if condition as needed.
		stmt.Op = cc.For
		x := stmt.Expr
		stmt.Expr = nil
		stmt.Body = forceBlock(stmt.Body)
		stmt.Body.Block = append(stmt.Body.Block, &cc.Stmt{
			Op:   cc.If,
			Expr: &cc.Expr{Op: cc.Not, Left: x},
			Body: &cc.Stmt{Op: cc.Break},
		})

	case cc.While:
		stmt.Op = cc.For
		fallthrough

	case cc.For:
		before1, _ := extractSideEffects(stmt.Pre, sideStmt|sideNoAfter)
		before2, _ := extractSideEffects(stmt.Expr, sideNoAfter)
		if len(before2) > 0 {
			x := stmt.Expr
			stmt.Expr = nil
			stmt.Body = forceBlock(stmt.Body)
			top := &cc.Stmt{
				Op:   cc.If,
				Expr: &cc.Expr{Op: cc.Not, Left: x},
				Body: &cc.Stmt{Op: cc.Break},
			}
			stmt.Body.Block = append(append(before2, top), stmt.Body.Block...)
		}
		if len(before1) > 0 {
			old := copyStmt(stmt)
			stmt.Pre = nil
			stmt.Expr = nil
			stmt.Post = nil
			stmt.Body = nil
			stmt.Op = BlockNoBrace
			stmt.Block = append(before1, old)
		}
		before, after := extractSideEffects(stmt.Post, sideStmt)
		if len(before)+len(after) > 0 {
			all := append(append(before, &cc.Stmt{Op: cc.StmtExpr, Expr: stmt.Post}), after...)
			stmt.Post = &cc.Expr{Op: ExprBlock, Block: all}
		}

	case cc.If, cc.Return:
		if stmt.Op == cc.If && stmt.Else == nil {
			fixAndAndAssign(stmt)
		}
		before, _ := extractSideEffects(stmt.Expr, sideNoAfter)
		if len(before) > 0 {
			old := copyStmt(stmt)
			stmt.Expr = nil
			stmt.Body = nil
			stmt.Else = nil
			stmt.Op = BlockNoBrace
			stmt.Block = append(before, old)
		}

	case cc.StmtExpr:
		before, after := extractSideEffects(stmt.Expr, sideStmt)
		if len(before)+len(after) > 0 {
			old := copyStmt(stmt)
			stmt.Expr = nil
			stmt.Op = BlockNoBrace
			stmt.Block = append(append(before, old), after...)
		}

	case cc.Goto:
		// TODO: Figure out where the goto goes and maybe rewrite
		// to labeled break/continue.
		// Otherwise move code or something.

	case cc.Switch:
		// TODO: Change default fallthrough to default break.
		before, _ := extractSideEffects(stmt.Expr, sideNoAfter)
		rewriteSwitch(stmt)
		if len(before) > 0 {
			old := copyStmt(stmt)
			stmt.Expr = nil
			stmt.Body = nil
			stmt.Else = nil
			stmt.Op = BlockNoBrace
			stmt.Block = append(before, old)
		}
	}
}

// fixAndAndAssign rewrites if(x && (y = z) ...) ...  to if(x) { y = z; if(...) ... }
func fixAndAndAssign(stmt *cc.Stmt) {
	changed := false
	clauses := splitExpr(stmt.Expr, cc.AndAnd)
	for i := len(clauses) - 1; i > 0; i-- {
		before, _ := extractSideEffects(clauses[i], sideNoAfter)
		if len(before) == 0 {
			continue
		}
		changed = true
		stmt.Body = &cc.Stmt{
			Op: BlockNoBrace,
			Block: append(before, &cc.Stmt{
				Op:   cc.If,
				Expr: joinExpr(clauses[i:], cc.AndAnd),
				Body: stmt.Body,
			}),
		}
		clauses = clauses[:i]
	}
	if changed {
		stmt.Expr = joinExpr(clauses, cc.AndAnd)
	}
}

func splitExpr(x *cc.Expr, op cc.ExprOp) []*cc.Expr {
	if x == nil {
		return nil
	}
	var list []*cc.Expr
	for x.Op == op {
		list = append(list, x.Right)
		x = x.Left
	}
	list = append(list, x)
	for i, j := 0, len(list)-1; i < j; i, j = i+1, j-1 {
		list[i], list[j] = list[j], list[i]
	}
	return list
}

func joinExpr(list []*cc.Expr, op cc.ExprOp) *cc.Expr {
	if len(list) == 0 {
		return nil
	}
	x := list[0]
	for _, y := range list[1:] {
		x = &cc.Expr{Op: op, Left: x, Right: y}
	}
	return x
}

func rewriteSwitch(swt *cc.Stmt) {
	var out []*cc.Stmt
	for _, stmt := range swt.Body.Block {
		// Put names after cases, so that they go to the same place.
		var names, cases []*cc.Label
		for _, lab := range stmt.Labels {
			if lab.Op == cc.LabelName {
				names = append(names, lab)
			} else {
				cases = append(cases, lab)
			}
		}
		if len(cases) > 0 && len(names) > 0 {
			stmt.Labels = append(cases, names...)
		}
		if len(cases) > 0 {
			// Add fallthrough if needed.
			if len(out) > 0 && numRewrite == 1 {
				last := out[len(out)-1]
				if last.Op == cc.Break && len(last.Labels) == 0 {
					last.Op = cc.Empty
				} else if fallsThrough(last) {
					out = append(out, &cc.Stmt{Op: cc.StmtExpr, Expr: &cc.Expr{Op: cc.Name, Text: "fallthrough"}})
				}
			}
		}
		out = append(out, stmt)
	}
	swt.Body.Block = out
}

func fallsThrough(x *cc.Stmt) bool {
	switch x.Op {
	case cc.Break, cc.Continue, cc.Return, cc.Goto:
		return false
	case cc.StmtExpr:
		if x.Expr.Op == cc.Call && x.Expr.Left.Op == cc.Name && x.Expr.Left.Text == "sysfatal" {
			return false
		}
	}
	return true
}

func forceBlock(x *cc.Stmt) *cc.Stmt {
	if x.Op != cc.Block {
		x = &cc.Stmt{Op: cc.Block, Block: []*cc.Stmt{x}}
	}
	return x
}

const (
	sideStmt = 1 << iota
	sideNoAfter
)

func extractSideEffects(x *cc.Expr, mode int) (before, after []*cc.Stmt) {
	doSideEffects(x, &before, &after, mode)
	return
}

var tmpGen = make(chan int)

func init() {
	go func() {
		for i := 1; ; i++ {
			tmpGen <- i
		}
	}()
}

func doSideEffects(x *cc.Expr, before, after *[]*cc.Stmt, mode int) {
	if x == nil {
		return
	}

	// Cannot hoist side effects from conditionally evaluated expressions
	// into unconditionally evaluated statement lists.
	// For now, detect but do not handle.
	switch x.Op {
	case cc.Cond:
		doSideEffects(x.List[0], before, after, mode&^sideStmt|sideNoAfter)
		checkNoSideEffects(x.List[1], 0)
		checkNoSideEffects(x.List[2], 0)

	case cc.AndAnd, cc.OrOr:
		doSideEffects(x.Left, before, after, mode&^sideStmt|sideNoAfter)
		checkNoSideEffects(x.Right, 0)

	case cc.Comma:
		var leftover []*cc.Expr
		for i, y := range x.List {
			m := mode | sideNoAfter
			if i+1 < len(x.List) {
				m |= sideStmt
			}
			doSideEffects(y, before, after, m)
			switch y.Op {
			case cc.PostInc, cc.PostDec, cc.Eq, cc.AddEq, cc.SubEq, cc.MulEq, cc.DivEq, cc.ModEq, cc.XorEq, cc.OrEq, cc.AndEq, cc.LshEq, cc.RshEq:
				*before = append(*before, &cc.Stmt{Op: cc.StmtExpr, Expr: y})
			default:
				leftover = append(leftover, y)
			}
		}
		x.List = leftover

	default:
		doSideEffects(x.Left, before, after, mode&^sideStmt)
		doSideEffects(x.Right, before, after, mode&^sideStmt)
		for _, y := range x.List {
			doSideEffects(y, before, after, mode&^sideStmt)
		}
	}

	if mode&sideStmt != 0 {
		// Expression as statement.
		// Can leave x++ alone, can rewrite ++x to x++, can leave x [op]= y alone.
		switch x.Op {
		case cc.PreInc:
			x.Op = cc.PostInc
			return
		case cc.PreDec:
			x.Op = cc.PostDec
			return
		case cc.PostInc, cc.PostDec:
			return
		case cc.Eq, cc.AddEq, cc.SubEq, cc.MulEq, cc.DivEq, cc.ModEq, cc.XorEq, cc.OrEq, cc.AndEq, cc.LshEq, cc.RshEq:
			return
		case cc.Call:
			return
		}
	}

	switch x.Op {
	case cc.Eq, cc.AddEq, cc.SubEq, cc.MulEq, cc.DivEq, cc.ModEq, cc.XorEq, cc.OrEq, cc.AndEq, cc.LshEq, cc.RshEq:
		x.Left = forceCheap(before, x.Left)
		old := copyExpr(x)
		*before = append(*before, &cc.Stmt{Op: cc.StmtExpr, Expr: old})
		fixMerge(x, x.Left)

	case cc.PreInc, cc.PreDec:
		x.Left = forceCheap(before, x.Left)
		old := copyExpr(x)
		old.SyntaxInfo = cc.SyntaxInfo{}
		if old.Op == cc.PreInc {
			old.Op = cc.PostInc
		} else {
			old.Op = cc.PostDec
		}
		*before = append(*before, &cc.Stmt{Op: cc.StmtExpr, Expr: old})
		fixMerge(x, x.Left)

	case cc.PostInc, cc.PostDec:
		x.Left = forceCheap(before, x.Left)
		if mode&sideNoAfter != 0 {
			// Not allowed to generate fixups afterward.
			d := &cc.Decl{
				Name: fmt.Sprintf("tmp%d", <-tmpGen),
				Type: x.Left.XType,
			}
			eq := &cc.Expr{
				Op:    ColonEq,
				Left:  &cc.Expr{Op: cc.Name, Text: d.Name, XDecl: d},
				Right: x.Left,
			}
			old := copyExpr(x.Left)
			old.SyntaxInfo = cc.SyntaxInfo{}
			*before = append(*before,
				&cc.Stmt{Op: cc.StmtExpr, Expr: eq},
				&cc.Stmt{Op: cc.StmtExpr, Expr: &cc.Expr{Op: x.Op, Left: old}},
			)
			x.Op = cc.Name
			x.Text = d.Name
			x.XDecl = d
			x.Left = nil
			break
		}
		old := copyExpr(x)
		old.SyntaxInfo = cc.SyntaxInfo{}
		*after = append(*after, &cc.Stmt{Op: cc.StmtExpr, Expr: old})
		fixMerge(x, x.Left)

	case cc.Cond:
		// Rewrite c ? y : z into tmp with initialization:
		//	var tmp typeof(c?y:z)
		//	if c {
		//		tmp = y
		//	} else {
		//		tmp = z
		//	}
		d := &cc.Decl{
			Name: "tmp",
			Type: x.XType,
		}
		*before = append(*before,
			&cc.Stmt{Op: cc.StmtDecl, Decl: d},
			&cc.Stmt{Op: cc.If, Expr: x.List[0],
				Body: &cc.Stmt{
					Op: cc.StmtExpr,
					Expr: &cc.Expr{
						Op:    cc.Eq,
						Left:  &cc.Expr{Op: cc.Name, Text: d.Name, XDecl: d},
						Right: x.List[1],
					},
				},
				Else: &cc.Stmt{
					Op: cc.StmtExpr,
					Expr: &cc.Expr{
						Op:    cc.Eq,
						Left:  &cc.Expr{Op: cc.Name, Text: d.Name, XDecl: d},
						Right: x.List[2],
					},
				},
			},
		)
		x.Op = cc.Name
		x.Text = d.Name
		x.XDecl = d
		x.List = nil

	case cc.Call:
		if x.Left.Text == "fmtstrcpy" || x.Left.Text == "fmtprint" {
			old := copyExpr(x)
			old.SyntaxInfo = cc.SyntaxInfo{}
			*before = append(*before, &cc.Stmt{Op: cc.StmtExpr, Expr: old})
			x.Op = cc.Number
			x.Text = "0"
			x.XDecl = nil
			x.Left = nil
			x.List = nil
		}
	}
}

func copyExpr(x *cc.Expr) *cc.Expr {
	old := *x
	old.SyntaxInfo = cc.SyntaxInfo{}
	return &old
}

func copyStmt(x *cc.Stmt) *cc.Stmt {
	old := *x
	old.SyntaxInfo = cc.SyntaxInfo{}
	old.Labels = nil
	return &old
}

func forceCheap(before *[]*cc.Stmt, x *cc.Expr) *cc.Expr {
	// TODO
	return x
}

func fixMerge(dst, src *cc.Expr) {
	syn := dst.SyntaxInfo
	syn.Comments.Before = append(syn.Comments.Before, src.Comments.Before...)
	syn.Comments.After = append(syn.Comments.After, src.Comments.After...)
	syn.Comments.Suffix = append(syn.Comments.Suffix, src.Comments.Suffix...)
	*dst = *src
	dst.SyntaxInfo = syn
}

func checkNoSideEffects(x *cc.Expr, mode int) {
	var before, after []*cc.Stmt
	old := x.String()
	doSideEffects(x, &before, &after, mode)
	if len(before)+len(after) > 0 {
		fprintf(x.Span, "cannot handle side effects in %s", old)
	}
}
