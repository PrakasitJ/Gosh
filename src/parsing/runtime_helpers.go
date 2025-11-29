package parsing

const RuntimeImportAlias = "goshrt"
const runtimeImportPath = "github.com/B1gdawg0/Gosh/src/runtime"

var runtimeArraysNeeded bool
var runtimePrintNeeded bool
var runtimeTypeCheckNeeded bool

// ResetRuntimeUsage clears runtime feature tracking before a new transpilation.
func ResetRuntimeUsage() {
	runtimeArraysNeeded = false
	runtimePrintNeeded = false
	runtimeTypeCheckNeeded = false
}

func NeedsRuntimeImport() bool {
	return runtimeArraysNeeded || runtimePrintNeeded || runtimeTypeCheckNeeded
}

func markRuntimeArraysUsage() {
	runtimeArraysNeeded = true
}

func markRuntimePrintUsage() {
	runtimePrintNeeded = true
}

func markRuntimeTypeCheckUsage() {
	runtimeTypeCheckNeeded = true
}

func runtimeHelperRef(fn string) string {
	return RuntimeImportAlias + "." + fn
}

// RuntimeImportPath returns the Go module path for the runtime helpers.
func RuntimeImportPath() string {
	return runtimeImportPath
}
