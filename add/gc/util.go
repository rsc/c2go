// +build ignore

package gc

import (
	"cmd/internal/obj"
	"strconv"
)

const (
	fmtMinus = 1 << iota
	fmtShort
	fmtSharp
	fmtByte
	fmtLong
	fmtComma
	fmtPlus
	fmtUnsigned
)

func bool2int(b bool) int {
	if b {
		return 1
	}
	return 0
}

func (n *Node) Line() string {
	return obj.Linklinefmt(Ctxt, int(n.Lineno), false, false)
}

func atoi(s string) int {
	n, _ := strconv.Atoi(s)
	return n
}
