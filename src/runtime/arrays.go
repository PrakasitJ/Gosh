package runtime

// SlicePush appends one or more values to the end of slice.
func SlicePush[T any](slice []T, values ...T) []T {
	if len(values) == 0 {
		return slice
	}
	return append(slice, values...)
}

// SliceInsert inserts value at the specified index, clamping the index to valid range.
func SliceInsert[T any](slice []T, index int, value T) []T {
	if index < 0 {
		index = 0
	}
	if index > len(slice) {
		index = len(slice)
	}
	result := make([]T, 0, len(slice)+1)
	result = append(result, slice[:index]...)
	result = append(result, value)
	result = append(result, slice[index:]...)
	return result
}

// SlicePop removes the last element if present.
func SlicePop[T any](slice []T) []T {
	if len(slice) == 0 {
		return slice
	}
	return slice[:len(slice)-1]
}

// SlicePopAt removes the element at index if valid.
func SlicePopAt[T any](slice []T, index int) []T {
	if index < 0 || index >= len(slice) {
		return slice
	}
	return append(slice[:index], slice[index+1:]...)
}

// SliceLen returns the length of the slice.
func SliceLen[T any](slice []T) int {
	return len(slice)
}
