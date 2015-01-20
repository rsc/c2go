// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/rsc/c2go/cc"
)

var goKeyword = map[string]bool{
	"chan":        true,
	"defer":       true,
	"fallthrough": true,
	"func":        true,
	"go":          true,
	"import":      true,
	"interface":   true,
	"iota":        true,
	"map":         true,
	"package":     true,
	"range":       true,
	"select":      true,
	"type":        true,
	"var":         true,

	// not keywords but still need renaming
	"fmt": true,
}

// renameDecls renames file-local declarations to make them
// unique across the whole set of files being considered.
// For now, it appends the file base name to the declared name.
// Eventually it could be smarter and not do that when not necessary.
// It also renames names like 'type' and 'func' to avoid Go keywords.
func renameDecls(cfg *Config, prog *cc.Prog) {
	// Rewrite C identifiers to avoid important Go words (keywords, iota, etc).
	cc.Preorder(prog, func(x cc.Syntax) {
		switch x := x.(type) {
		case *cc.Decl:
			if goKeyword[x.Name] {
				// NOTE: Must put _ last so that name can be upper-cased for export.
				x.Name += "_"
			}

		case *cc.Stmt:
			for _, lab := range x.Labels {
				if goKeyword[lab.Name] {
					lab.Name += "_"
				}
			}
			switch x.Op {
			case cc.Goto:
				if goKeyword[x.Text] {
					x.Text += "_"
				}
			}
		}
	})

	// Assign to packages (needed below but also in writeGoFiles).
	for _, d := range prog.Decls {
		if d.Body != nil && d.Body.Span.Start.File != "" {
			d.Span = d.Body.Span
		}
		d.GoPackage = cfg.filePackage(d.Span.Start.File)
	}

	// Build list of declared top-level names.
	// Not just prog.Decls because of enums and struct definitions.
	typedefs := map[*cc.Type]bool{}
	for _, d := range prog.Decls {
		if d.Storage&cc.Typedef != 0 {
			typedefs[d.Type] = true
		}
	}

	var decls []*cc.Decl
	for _, d := range prog.Decls {
		if d.Name == "" {
			if typedefs[d.Type] {
				continue
			}
			switch d.Type.Kind {
			case cc.Struct:
				if d.Type.Tag != "" {
					decls = append(decls, d)
					d.Name = d.Type.Tag
					d.Storage = cc.Typedef
				}
				if d.Type.TypeDecl == nil {
					d.Type.TypeDecl = d
				}
			case cc.Enum:
				d.Type.Tag = "" // enum tags are worthless
				for _, dd := range d.Type.Decls {
					decls = append(decls, dd)
				}
			}
			continue
		}
		decls = append(decls, d)
		if d.Storage&cc.Typedef != 0 && d.Type != nil && d.Type.TypeDecl == nil {
			d.Type.TypeDecl = d
		}
	}

	// Assign declarations to packages and identify conflicts.
	count := make(map[string]int)
	src := make(map[string]string)
	for _, d := range decls {
		// TODO(rsc): I don't understand why this is necessary given the above.
		if d.Body != nil && d.Body.Span.Start.File != "" {
			d.Span = d.Body.Span
		}
		d.GoPackage = cfg.filePackage(d.Span.Start.File)
		key := d.GoPackage + "." + d.Name
		if count[key]++; count[key] > 1 {
			if d.Span.String() == src[key] {
				// Assume this is a nested header and ignore duplicates.
				count[key] = 1
				continue
			}
			fprintf(d.Span, "conflicting name %s in %s (last at %s)", d.Name, d.GoPackage, src[key])
			continue
		}
		src[key] = fmt.Sprintf("%s:%d", d.Span.Start.File, d.Span.Start.Line)
	}

	// Rename static, conflicting names.
	for _, d := range decls {
		key := d.GoPackage + "." + d.Name
		if count[key] > 1 {
			file := filepath.Base(d.Span.Start.File)
			if i := strings.Index(file, "."); i >= 0 {
				file = file[:i]
			}
			d.Name += "_" + file
		}
	}

	cfg.topDecls = decls
}
