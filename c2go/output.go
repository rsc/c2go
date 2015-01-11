// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"go/format"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"

	"rsc.io/cc"
)

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
			p.Print("package main\n\n")
			printers[gofile] = p
		}
		p.Print(decl)
		p.Print(Newline)
		p.Print(Newline)
	}

	for gofile, p := range printers {
		dstfile := filepath.Join("/tmp/c2go", gofile)
		os.MkdirAll(filepath.Dir(dstfile), 0777)
		buf := p.Bytes()

		// Not entirely sure why these lines get broken.
		buf = bytes.Replace(buf, []byte("\n,"), []byte(","), -1)
		buf = bytes.Replace(buf, []byte("\n {"), []byte(" {"), -1)

		buf1, err := format.Source(buf)
		if err == nil {
			buf = buf1
		}
		if err := ioutil.WriteFile(dstfile, buf, 0666); err != nil {
			log.Print(err)
		}
	}
}
