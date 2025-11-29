package runtime

import (
	"fmt"
	"reflect"
)

func signedType(typeName string) reflect.Kind {
	types := map[string]reflect.Kind{
		"int":    reflect.Int,
		"float":  reflect.Float32,
		"double": reflect.Float64,
		"bool":   reflect.Bool,
		"string": reflect.String,
		"byte":   reflect.Uint8,
		"long":   reflect.Int64,
		"short":  reflect.Int16,
	}
	
	if kind, ok := types[typeName]; ok {
		return kind
	}
	return reflect.Invalid
}

func CheckType(varName interface{}, varType string, line int) error {
	expectedKind := signedType(varType)
	if expectedKind == reflect.Invalid {
		return fmt.Errorf("[Error] unknown type '%s' at line %d", varType, line)
	}

	actualType := reflect.TypeOf(varName)
	if actualType == nil {
		return fmt.Errorf("[Error] nil value cannot be checked against type '%s' at line %d", varType, line)
	}
	
	actualKind := actualType.Kind()
	

	if !isCompatibleType(actualKind, expectedKind) {
		return fmt.Errorf(
			"[Error] type mismatch at line %d: expected '%s' (%v) but got %v",
			line, varType, expectedKind, actualKind,
		)
	}
	
	return nil
}

func isCompatibleType(actual, expected reflect.Kind) bool {
	if actual == expected {
		return true
	}

	numericKinds := map[reflect.Kind]int{
		reflect.Uint8:   1,
		reflect.Int16:   2,
		reflect.Int:     3,
		reflect.Int64:   4,
		reflect.Float32: 5,
		reflect.Float64: 6,
	}
	
	actualLevel, actualIsNumeric := numericKinds[actual]
	expectedLevel, expectedIsNumeric := numericKinds[expected]

	if actualIsNumeric && expectedIsNumeric {
		return actualLevel <= expectedLevel
	}
	
	return false
}
