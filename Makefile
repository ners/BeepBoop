build:
	stack build

EXECUTABLE := ${shell egrep -o -e '\w+-exe' package.yaml}
exec: build
	stack exec -- ${EXECUTABLE}
