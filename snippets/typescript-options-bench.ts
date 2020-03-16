
import Benchmark = require('benchmark'); // from 'benchmark';

interface IOptions {
  option1: boolean;
  option2: string;
}

const partialOptions: Array<Partial<IOptions>> = [
  {
    option1: true,
  },
  {
    option1: true,
    option2: 'foo',
  },
  {
    option2: 'foo',
  },
];

function setDefaults1(options: Partial<IOptions> = {}): IOptions {
  return Object.assign({
    option1: true,
    option2: 'bar',
  }, options);
}

function setDefaults2(options: Partial<IOptions> = {}): IOptions {
  return {
    option1: true,
    option2: 'bar',
    ...options,
  };
}

function setDefaults3(options?: Partial<IOptions>): IOptions {
  return {
    option1: options.option1 !== undefined ? options.option1 : true,
    option2: options.option2 !== undefined ? options.option2 : 'bar',
  };
}

function setDefaults4({
  option1 = true,
  option2 = 'bar',
}: Partial<IOptions> = {}): IOptions {
  return { option1, option2 };
}

new Benchmark.Suite()
  .add('#setDefaults Object.assign', () => {
    for (let i = 0; i < partialOptions.length; i += 1) {
      setDefaults1(partialOptions[i]);
    }
  })
  .add('#setDefaults spread', () => {
    for (let i = 0; i < partialOptions.length; i += 1) {
      setDefaults2(partialOptions[i]);
    }
  })
  .add('#setDefaults inline', () => {
    for (let i = 0; i < partialOptions.length; i += 1) {
      setDefaults3(partialOptions[i]);
    }
  })
  .add('#setDefaults defaults', () => {
    for (let i = 0; i < partialOptions.length; i += 1) {
      setDefaults4(partialOptions[i]);
    }
  })
  .on('cycle', (event) => {
    console.log(String(event.target));
  })
  .run();
