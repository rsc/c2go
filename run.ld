#!/bin/bash

set -e
eval $(go env)
export GOROOT
go install
rm -f c2go
dst=$GOROOT # !!!
rm -rf $dst/src/cmd/internal/ld* $dst/src/cmd/new?l

for i in 6 5 8 9
do
	rm -rf $dst/src/cmd/internal/ld
	c2go -c c2go.ld.cfg -dst $dst -I $GOROOT/include -I $GOROOT/src/cmd/ld -I $GOROOT/src/cmd/${i}l $GOROOT/src/liblink/*.c $GOROOT/src/cmd/ld/*.c $GOROOT/src/cmd/${i}l/*.c
	mv $dst/src/cmd/internal/ld $dst/src/cmd/internal/ld.$i
done

mv $dst/src/cmd/internal/ld.6 $dst/src/cmd/internal/ld
rm -rf $dst/src/cmd/internal/ld.?

for i in add/new6l/*.go; do
	base=$(echo $i | sed 's;add/;;')
	cat $i | grep -v 'build ignore' >$dst/src/cmd/$base
done
for i in add/ld/*.go; do
	base=$(echo $i | sed 's;.*/;;')
	cat $i | grep -v 'build ignore' >$dst/src/cmd/internal/ld/$base
done

gofmt -w $dst/src/cmd/internal/ld $dst/src/cmd/new?l # check for syntax errors, mainly
rm -rf /tmp/c2gomain
mv $dst/src/main /tmp/c2gomain
export GOPATH=""
rm $dst/src/cmd/internal/ld/bio.go
echo 'X ,s/main\./obj./g
X ,s/obj\.Argv0/os.Args[0]/g
X ,s/obj\.Exits\(".+"\)/Exit(2)/g
X ,s/gc\.Zconv/Zconv/g
X ,s/BGETC/Bgetc/g
X ,s/\*Endian/binary.ByteOrder/g
X ,s/\&le([^a-zA-Z0-9_]|\n)/binary.LittleEndian\1/g
X ,s/\&be([^a-zA-Z0-9_]|\n)/binary.BigEndian\1/g
X ,s/ be\./ binary.BigEndian./g
X ,s/ le\./ binary.LittleEndian./g
X ,s/\.e64/.Uint64/g
X ,s/\.e32/.Uint32/g
X ,s/\.e16/.Uint16/g
X ,s/obj\.Werrstr/err = fmt.Errorf/g
X ,s/(err = fmt.Errorf.*)\n(	+)return -1\n/\1\n\2return err\n/g
X ,s/argv\[argc\] = (.*)\n	+argc\+\+\n/argv = append(argv, \1)\n/g
X/'"'"'/w
q' | sam -d $(find $dst/src/cmd/{internal/ld,new?l} -type f -a -name '*.go')
goimports -w $dst/src/cmd/new?l $dst/src/cmd/internal/ld
wc $(find $dst/src/cmd/{internal/ld,new?l} -type f -a -name '*.go')
go install -gcflags -e cmd/internal/ld cmd/new{5,6,8,9}l

go tool 6g ~/hellocgo.go
go tool new6l helloworld.6
./6.out

for i in 5l 6l 8l 9l
do
	cp $(which new$i) $(toolstash -n $i)
done
exit 0
