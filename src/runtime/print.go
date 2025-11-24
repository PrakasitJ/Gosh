package runtime

import "fmt"

// Println is the default console output helper exposed to Gosh code.
func Println(args ...interface{}) {
	fmt.Println(args...)
}
