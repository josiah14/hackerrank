#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <stdarg.h>

void free_2d_array(int** arr) {
  int i = 0;
  while (arr[i][0]) {
    free(arr[i]);
    i++;
  }
  free(arr[i]);

  free(arr);
}

void flag_error(char msg[]) {
  puts(msg);
  exit(EXIT_FAILURE);
}

// Gets the number of slices for this test according to the first input value from stdin.
int read_num_slices() {
  int num_slices = 0;

  scanf("%i", &num_slices);

  if (num_slices == 0) {
    goto error;
  }

  return num_slices;

error:
  flag_error("ERROR: Could not parse the number of entries from first input line.");
  return 1;
}

// Gets a single line from stdin and attempts to parse it into a 2D int array representing the dimensions of a slice.
//  Returns [0,0] on error.
int* read_slice_dimension() {
  static int loaf_dimension[2] = {0};

  scanf("%i %i", &loaf_dimension[0], &loaf_dimension[1]);

  return loaf_dimension;
}

// Gets all of the bread slices to be processed.
//
// This function reads from stdin.  The first line should be a single integer that specifies the number of slices to be
// processed by this current test.  The subsequent lines should be two integers separated by a space which represent
// the 2D dimensions of each slice.
//
// The last dimension pair in the array will always be [0,0] so that it is easy to find the end of it.
int** get_slices() {
  int num_slices = read_num_slices();
  static int** slices;
  slices = (int**) malloc((num_slices + 1) * sizeof(int*));

  int i;
  for (i = 0; i < num_slices; i++) {
    slices[i] = (int*) calloc(2, sizeof(int));
    memcpy(slices[i], read_slice_dimension(), 2 * sizeof(int));

    // If there was an error, set the last element to {0,0} to set the stopping point for the free function, then
    // gracefully exit.
    if (!(slices[i][0] && slices[i][1])) {
      slices[i][0] = 0;
      slices[i][1] = 0;
      goto error;
    }
  }

  // [0,0] terminate the array so that one can find the end without calculating the length.
  slices[i] = (int*) calloc(2, sizeof(int));

  return slices;

error:
  free_2d_array(slices);
  flag_error("ERROR: Could not parse line entered into a 2 integer array representing the slice's dimensions.");
  return NULL;
}

// gives 'num_elems' squares in descending order
int* get_squares(int num_elems) {
  int* squares = (int*) malloc(num_elems * sizeof(int));

  int i;
  for (i = 0; i < num_elems; i++) {
    int num = num_elems - i;
    squares[i] = num * num;
  }

  return squares;
}

bool is_perfect_slice_dimension(int slice[2], int square) {
  int area = slice[0] * slice[1];
  int num = sqrt(square);

  return !((area % square) || (slice[0] % num) || (slice[1] % num));
}

// is in the 'find' function in Haskell, takes a predicate and a list and returns the first value which evaluates to
// true.  The found argument is used to determine whether a matching element was found so that a list containing
// any integers could be used.
int find( bool (*predicate)(int), int* list, int num_elems, bool* found) {
  int value = 0;

  *found = false;
  int i;
  for (i = 0; i < num_elems; i++) {
    if (predicate(list[i])) {
      value = list[i];
      *found = true;
      break;
    }
  }

  return value;
}

void* curry(void* f, int num_args, ...) {
  // TODO: figure out how to pass back a pointer to the curried function
  return NULL;
}

int min_number_of_slices(int dimensions[2]) {
  int num_slices;
  if (dimensions[0] == dimensions[1]) {
    num_slices = 1;
  } else {
    int num_squares = dimensions[0] < dimensions[1] ? dimensions[0] : dimensions[1];
    int* squares = (int*) malloc(num_squares * sizeof(int));
    squares = get_squares(num_squares);
    int largest_square = find(&is_perfect_slice_dimension);
  }

}

int main() {
  int** slices = get_slices();

  int i = 0;
  while (slices[i][0]) {
    int j;
    for (j = 0; j < 2; j++) {
      printf("%i\n", slices[i][j]);
    }
    i++;
  }

  free_2d_array(slices);

  return 0;
}

