// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"path"
	"path/filepath"
	"runtime"
	"strings"

	"rsc.io/c2go/cc"
)

type Config struct {
	pkgRules []pkgRule
	exports  []string
	replace  map[string]string
	delete   map[string]bool
	diffs    []diff

	// derived during analysis
	topDecls []*cc.Decl
}

type pkgRule struct {
	pattern string
	pkg     string
}

type diff struct {
	line   string
	before []byte
	after  []byte
	used   int
}

func (cfg *Config) filePackage(file string) (pkg string) {
	file = filepath.ToSlash(strings.TrimPrefix(file, runtime.GOROOT()+string(filepath.Separator)))
	pkg = "main"
	for _, rule := range cfg.pkgRules {
		matched, err := path.Match(rule.pattern, file)
		if err != nil {
			log.Printf("invalid pattern: %v", err)
			continue
		}
		if matched {
			pkg = rule.pkg
		}
	}
	return pkg
}

func (cfg *Config) read(file string) {
	data, err := ioutil.ReadFile(file)
	if err != nil {
		log.Fatal(err)
	}
	lineno := 0
	lines := strings.Split(string(data), "\n")
	cfg.replace = make(map[string]string)
	cfg.delete = make(map[string]bool)
	for len(lines) > 0 {
		line := lines[0]
		lines = lines[1:]
		lineno++
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "#") {
			continue
		}
		f := strings.Fields(line)
		if len(f) == 0 {
			continue
		}
		switch f[0] {
		case "package":
			pkg := f[len(f)-1]
			for i := 1; i < len(f)-1; i++ {
				cfg.pkgRules = append(cfg.pkgRules, pkgRule{f[i], pkg})
			}

		case "export":
			cfg.exports = append(cfg.exports, f[1:]...)

		case "delete":
			for _, name := range f[1:] {
				cfg.delete[name] = true
			}

		case "func", "type":
			if len(f) < 2 {
				log.Printf("%s:%d: short func/type declaration", file, lineno)
			}
			var buf bytes.Buffer
			buf.WriteString(line + "\n")
			if strings.HasSuffix(line, "{") {
				for {
					lineno++
					if len(lines) == 0 {
						log.Fatalf("%s:%d: unexpected EOF reading func/type body", file, lineno)
					}
					line = lines[0]
					lines = lines[1:]
					buf.WriteString(line + "\n")
					if line == "}" {
						break
					}
				}
			}
			name := f[1]
			if i := strings.Index(name, "("); i >= 0 {
				name = name[:i]
			}
			cfg.replace[name] = buf.String()

		case "diff":
			if line != "diff {" {
				log.Printf("%s:%d: invalid diff opening", file, lineno)
				break
			}

			var old, new bytes.Buffer
			fileline := fmt.Sprintf("%s:%d", file, lineno)
			for {
				lineno++
				if len(lines) == 0 {
					log.Fatalf("%s:%d: unexpected EOF reading diff", file, lineno)
				}
				line = lines[0]
				lines = lines[1:]
				if line == "}" {
					break
				}
				switch {
				case strings.HasPrefix(line, "+"):
					line = strings.TrimPrefix(line[1:], " ")
					new.WriteString(line + "\n")

				case strings.HasPrefix(line, "-"):
					line = strings.TrimPrefix(line[1:], " ")
					old.WriteString(line + "\n")

				default:
					line = strings.TrimPrefix(strings.TrimPrefix(line, " "), " ")
					old.WriteString(line + "\n")
					new.WriteString(line + "\n")
				}
			}
			cfg.diffs = append(cfg.diffs, diff{
				line:   fileline,
				before: old.Bytes(),
				after:  new.Bytes(),
			})

		default:
			log.Printf("%s:%d: unknown verb %s", file, lineno, f[0])
		}
	}
}
