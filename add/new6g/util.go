// +build ignore

package main

import (
	"cmd/internal/obj"
	"strconv"
)

func bool2int(b bool) int {
	if b {
		return 1
	}
	return 0
}
