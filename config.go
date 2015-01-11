// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"io/ioutil"
	"log"
	"path"
	"path/filepath"
	"runtime"
	"strings"

	"rsc.io/cc"
)

type Config struct {
	pkgRules []pkgRule
	exports  []string

	// derived during analysis
	topDecls []*cc.Decl
}

type pkgRule struct {
	pattern string
	pkg     string
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
	for _, line := range strings.Split(string(data), "\n") {
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
		default:
			log.Printf("%s:%d: unknown verb %s", file, lineno, f[0])
		}
	}
}
