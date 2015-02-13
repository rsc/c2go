// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"strings"
	"unicode/utf8"

	"rsc.io/c2go/cc"
)

func exportDecls(cfg *Config, prog *cc.Prog) {
	for _, d := range cfg.topDecls {
		if shouldExport(cfg, d.Name) {
			exportDecl(d)
		}
		pkg := d.GoPackage
		if pkg == "" {
			continue
		}
		cc.Preorder(d, func(x cc.Syntax) {
			switch x := x.(type) {
			case *cc.Expr:
				if x.Op == cc.Name && x.XDecl != nil && x.XDecl.GoPackage != "" && x.XDecl.GoPackage != pkg {
					exportDecl(x.XDecl)
				}
			case *cc.Type:
				if x.Kind == cc.TypedefType && x.TypeDecl != nil && x.TypeDecl.GoPackage != "" && x.TypeDecl.GoPackage != pkg {
					exportDecl(x.TypeDecl)
					x.Name = x.TypeDecl.Name
				}
			}
		})
	}

	for _, d := range cfg.topDecls {
		renameDecl(cfg, d)
	}
}

func shouldExport(cfg *Config, name string) bool {
	for _, s := range cfg.exports {
		if s == name {
			return true
		}
	}
	return false
}

func exportDecl(d *cc.Decl) {
	d.Name = exportName(d.Name)
	if d.Storage&cc.Typedef != 0 && d.Type.Kind == cc.Struct {
		for _, dd := range d.Type.Decls {
			exportDecl(dd)
			// type became type_, became Type_. Drop underscore now that it's not needed.
			if strings.HasSuffix(dd.Name, "_") && goKeyword[strings.ToLower(dd.Name[:len(dd.Name)-1])] {
				dd.Name = dd.Name[:len(dd.Name)-1]
			}
			if dd.Name == "U" {
				for _, dd := range dd.Type.Decls {
					exportDecl(dd)
				}
			}
		}
	}
}

func exportName(name string) string {
	_, size := utf8.DecodeRuneInString(name)
	return strings.ToUpper(name[:size]) + name[size:]
}

func renameDecl(cfg *Config, d *cc.Decl) {
	key := declKey(d)
	if cfg.rename[key] != "" {
		d.Name = cfg.rename[key]
	}
	if d.Storage&cc.Typedef != 0 && d.Type.Kind == cc.Struct {
		for _, dd := range d.Type.Decls {
			renameDecl(cfg, dd)
			if dd.Name == "U" {
				for _, dd := range dd.Type.Decls {
					renameDecl(cfg, dd)
				}
			}
		}
	}
	
	if d.Type != nil && d.Type.Kind == cc.Func && d.Body != nil {
		for _, s := range d.Body.Block {
			if s.Op == cc.StmtDecl && s.Decl.Storage&cc.Static != 0 {
				renameDecl(cfg, s.Decl)
			}
		}
	}
}
