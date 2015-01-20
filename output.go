// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"flag"
	"go/format"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/rsc/c2go/cc"
)

var dst = flag.String("dst", "/tmp/c2go", "GOPATH root of destination")

// writeGoFiles writes prog to Go source files in a tree of packages.
func writeGoFiles(cfg *Config, prog *cc.Prog) {
	printers := map[string]*Printer{}
	for _, decl := range prog.Decls {
		if decl.GoPackage == "" {
			decl.GoPackage = "other"
		}
		cfile := decl.Span.Start.File
		gofile := strings.TrimSuffix(strings.TrimSuffix(cfile, ".c"), ".h") + ".go"
		gofile = decl.GoPackage + "/" + filepath.Base(gofile)
		p := printers[gofile]
		if p == nil {
			p = new(Printer)
			p.Package = decl.GoPackage
			p.Print("package ", path.Base(p.Package), "\n\n")
			if p.Package != "liblink1" {
				p.Print("import \"liblink1\"\n\n")
			}
			printers[gofile] = p
		}

		off := len(p.Bytes())
		repl, ok := cfg.replace[decl.Name]
		if !ok {
			repl, ok = cfg.replace[strings.ToLower(decl.Name)]
		}
		if cfg.delete[decl.Name] || cfg.delete[strings.ToLower(decl.Name)] {
			repl, ok = "", true
		}
		if ok {
			// Use replacement text from config but keep surrounding comments.
			p.Print(decl.Comments.Before)
			p.Print(repl)
			p.Print(decl.Comments.Suffix, decl.Comments.After)
		} else {
			p.Print(decl)
		}
		if len(p.Bytes()) > off {
			p.Print(Newline)
			p.Print(Newline)
		}
	}

	for gofile, p := range printers {
		dstfile := filepath.Join(*dst+"/src", gofile)
		os.MkdirAll(filepath.Dir(dstfile), 0777)
		buf := p.Bytes()

		// Not entirely sure why these lines get broken.
		buf = bytes.Replace(buf, []byte("\n,"), []byte(","), -1)
		buf = bytes.Replace(buf, []byte("\n {"), []byte(" {"), -1)

		buf1, err := format.Source(buf)
		if err == nil {
			buf = buf1
		}

		for i, d := range cfg.diffs {
			if bytes.Contains(buf, d.before) {
				buf = bytes.Replace(buf, d.before, d.after, -1)
				cfg.diffs[i].used++
			}
		}

		if err := ioutil.WriteFile(dstfile, buf, 0666); err != nil {
			log.Print(err)
		}
	}
}
