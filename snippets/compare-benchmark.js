
function binarySearch(array, first, last, key, compare) {
  var length = last - first;
  while (length > 0) {
    var step = Math.floor(length / 2);
    var middle = first + step;
    var result = compare(array[middle], key);
    if (result < 0) {
      first = middle + 1;
      length -= step + 1;
    } else if (result == 0) {
      return middle;
    } else {
      length = step;
    }
  }

  return last === key ? last : -1;
}


function compare1(a, b) {
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
}


function compare2(a, b) {
    return a - b;
}


function run(compare) {
  // Create a sorted array
  var array = [];
  for (var i = 0; i < 8192; ++i) {
    array.push(i * 2);
  }

  var found = 0;

  for (var j = -1; j < 16383; ++j) {
    var index = binarySearch(
      array,              // sorted array
      0,                  // start index
      array.length - 1,   // end index
      j,                  // element we are looking for
      compare);           // comparison function

    if (index !== -1 && array[index] === j) {
      found += 1;
    }
  }

  if (found !== 8191) {
    console.error('Found bug', found);
  }
}


function bench() {
  var Benchmark = require('benchmark');
  var suite = new Benchmark.Suite();

  suite
    .add('compare1', function() {
      run(compare1);
    })
    .add('compare2', function() {
      run(compare2);
    })
    .on('cycle', function(event) {
      console.log(String(event.target));
    })
    .on('complete', function() {
      console.log('Fastest is ' + this.filter('fastest').map('name'));
    })
    .run({});
}

bench();
