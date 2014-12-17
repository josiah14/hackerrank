computeSliceArea(X, Y, Area):- Area is X * Y.

computerGreatestCommonDivisor(X, 0, GCD):- GCD is X.
computerGreatestCommonDivisor(X, Y, GCD):- Rem is mod(X,Y), computerGreatestCommonDivisor(Y, Rem, GCD).

findMinNumSlices(X, Y, MinNumSlices):- computeSliceArea(X, Y, SliceArea),
                                       computerGreatestCommonDivisor(X, Y, GCD),
                                       SquareArea is GCD ** 2,
                                       MinNumSlices is SliceArea / SquareArea.

iterateRead(0).
iterateRead(N):- N > 0,
                 read(X),
                 read(Y),
                 findMinNumSlices(X, Y, MinSlices),
                 writeln(MinSlices),
                 M is N - 1,
                 iterateRead(M).

main:- read(N), iterateRead(N).

