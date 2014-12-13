def parse_line(line)
  line.split(" ").map(&:to_i)
end

def get_number_of_slices
  parse_line(ARGF.readline).first
end

def get_all_slice_dimensions(number_of_slices)
  number_of_slices.times.collect { parse_line(ARGF.readline) }
end


# Use the Euclidean algorithm, but don't do it recursively because Ruby does not have TCO turned on by default.
def greatest_common_divisor(slice_dimension)
  width = slice_dimension[0]
  length = slice_dimension[1]
  while length != 0
    remainder = width % length
    width = length
    length = remainder
  end

  puts width
  width
end

def area(slice_dimension)
  slice_dimension[0] * slice_dimension[1]
end

def min_number_of_slices(slice_dimension)
  area(slice_dimension) / greatest_common_divisor(slice_dimension)**2
end

def main
  puts get_all_slice_dimensions(get_number_of_slices).map { |slice_dim| min_number_of_slices(slice_dim) }
end

main

