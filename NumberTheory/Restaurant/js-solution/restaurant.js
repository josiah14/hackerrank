var readline = require("readline");

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

var parseLine = function (line) {
  var parsedLine = [];
  line.split(" ").forEach(function (numberString) {
    parsedLine.push(Number(numberString));
  });
  return parsedLine;
};

rl.iterateEventCallback = function (numTimes, eventName, callback, continueFunc) {
  var values = [];

  this.on(eventName, function (eventValue) {
    values.push(callback(eventValue));
    if (numTimes <= 0) {
      this.close();
    }
    numTimes -= 1;
  });

  rl.on('close', function () {
    continueFunc(values);
  });
};

var greatestCommonDivisor = function (loafDimension) {
  var width = loafDimension[0];
  var len = loafDimension[1];
  while (len != 0) {
    var remainder = width % len;
    width = len;
    len = remainder;
  }
  return width;
};

var area = function (loafDimension) {
  return loafDimension[0] * loafDimension[1];
};

var minNumberOfSlices = function (loafDimension) {
  return area(loafDimension) / Math.pow(greatestCommonDivisor(loafDimension), 2);
};

var getMinSlicesForAllLoaves = function (loaves) {
  var results = [];
  loaves.forEach(function (loaf) {
    results.push(minNumberOfSlices(loaf));
  });
  return results;
};

var printResults = function (loaves) {
  getMinSlicesForAllLoaves(loaves).forEach(function (result) {
    console.log(result);
  });
};

rl.once('line', function (line) {
  var num_loaves = Number(line);
  rl.iterateEventCallback(num_loaves, 'line', parseLine, printResults);
});

