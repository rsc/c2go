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
