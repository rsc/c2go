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

	"rsc.io/c2go/cc"
)

var dst = flag.String("dst", "/tmp/c2go", "GOPATH root of destination")

// writeGoFiles writes prog to Go source files in a tree of packages.
func writeGoFiles(cfg *Config, prog *cc.Prog) {
	printers := map[string]*Printer{}
	cfiles := map[string]string{}
	for _, decl := range prog.Decls {
		if decl.GoPackage == "" {
			decl.GoPackage = "other"
		}
		cfile := decl.Span.Start.File
		gofile := strings.TrimSuffix(strings.TrimSuffix(cfile, ".c"), ".h") + ".go"
		gofile = decl.GoPackage + "/" + filepath.Base(gofile)
		cfiles[gofile] = cfile
		p := printers[gofile]
		if p == nil {
			p = new(Printer)
			p.Package = decl.GoPackage
			pkg := path.Base(p.Package)
			if strings.Count(p.Package, "/") == 1 && strings.HasPrefix(p.Package, "cmd/") {
				pkg = "main"
			}
			p.Print("package ", pkg, "\n\n")
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

		buf = fixCopyright(gofile, cfiles[gofile], buf)

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

var copyrightPrefixes = []string{
	"// Copyright",
	"// Inferno",
	"// Derived",
	"// cmd/",
	"/*\n * The authors of this software",
	"/*\nhttp://code.google.com",
}

// fixCopyright hoists the copyright notice to the top of the file.
// The package statement has been printed above it.
// If fixCopyright cannot find a notice, it calls copyCopyright to copy it
// from the original C file.
func fixCopyright(gofile, cfile string, buf []byte) []byte {
	i := -1
	for _, prefix := range copyrightPrefixes {
		if j := bytes.Index(buf, []byte(prefix)); j >= 0 && (i < 0 || j < i) {
			i = j
		}
	}
	if i < 0 {
		//log.Printf("%s: cannot find copyright notice", gofile)
		return copyCopyright(gofile, cfile, buf)
	}

	k := bytes.Index(buf[i:], []byte("\n\n"))
	if k < 0 {
		log.Printf("%s: cannot find end of copyright notice", gofile)
		return buf
	}
	k += i
	for l := k - 1; l >= 0; l-- {
		if buf[l] == '\n' {
			if buf[l+1] != '/' || buf[l+2] != '/' {
				log.Printf("%s: copyright notice not followed by blank line", gofile)
				return buf
			}
			break
		}
	}

	var out []byte
	out = append(out, buf[i:k+2]...)
	out = append(out, buf[:i]...)
	out = append(out, buf[k+1:]...) // k+1 to include an extra \n
	return out
}

// copyCopyright inserts the copyright from cfile at the beginning of buf
// and returns the result.
func copyCopyright(gofile, cfile string, buf []byte) []byte {
	if strings.HasPrefix(cfile, "internal/") {
		return buf
	}
	data, err := ioutil.ReadFile(cfile)
	if err != nil {
		log.Printf("%s: reading copyright from C: %v", gofile, err)
		return buf
	}

	i := -1
	for _, prefix := range copyrightPrefixes {
		if j := bytes.Index(data, []byte(prefix)); j >= 0 && (i < 0 || j < i) {
			i = j
		}
	}
	if i < 0 {
		log.Printf("%s: cannot find copyright notice in C file %s", gofile, cfile)
		return buf
	}

	j := bytes.Index(data[i:], []byte("\n\n"))
	if j < 0 {
		log.Printf("%s: cannot find end of copyright notice in C file %s", gofile, cfile)
	}
	j += i
	return append(data[i:j+2], buf...)
}
