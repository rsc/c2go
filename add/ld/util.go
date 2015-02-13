// Copyright 2015 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ld

func cstring(x []byte) string {
	return string(bytes.TrimRight(x, "\x00"))
}

func plan9quote(s string) string {
	if s == "" {
		goto needquote
	}
	for i := 0; i < len(s); i++ {
		if s[i] <= ' ' || s[i] == '\'' {
			goto needquote
		}
	}
	return s

needquote:
	return "'" + strings.Replace(s, "'", "''", -1) + "'"
}

func cutStringAtNUL(s string) string {
	if i := strings.Index(s, "\x00"); i >= 0 {
		s = s[:i]
	}
	return s
}
