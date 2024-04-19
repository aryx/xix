// Build and test using setup-ocaml@v2 mostly.

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

local checkout = {
  uses: 'actions/checkout@v3',
};

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job = {
  strategy: {
    //'fail-fast': false,
    matrix: {
      os: [
	'ubuntu-latest',
	//TODO: 'macos-latest'
	//TODO: 'windows-latest'
	],
      'ocaml-compiler': [
	// Version that I ported ocamlrun to plan9 (need stdcompat
	// so we can use |> and bytes type without issues)
	'3.10.0',
	// Old version with |> builtin
	'4.02.1',
	// first version with valid ocamlformat
	'4.04.1',
	//TODO: Ideally 4.14.1 and ocaml latest (5.2.0)
	],
    }
  },
  'runs-on': '${{ matrix.os }}',
  steps: [
    checkout,
    {
      uses: "ocaml/setup-ocaml@v2",
      with: {
	'ocaml-compiler': '${{ matrix.ocaml-compiler }}',
      }
    },
    {
      name: 'Install dependencies',
      run: |||
        opam install --deps-only .
      |||,
    },
    {
      name: 'Build mk/rc',
      run: |||
        eval $(opam env)
        ./bootstrap-mk.sh
      |||,
    },
    {
      name: 'Basic test',
      run: |||
        ./test.sh
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The workflow
// ----------------------------------------------------------------------------
{
  name: 'build-and-test',
  on: {
    // can be run manually from the GHA dashboard
    workflow_dispatch: null,
    // on the PR
    pull_request: null,
    // and another time once the PR is merged on master
    push: {
      branches: [
        'master',
      ],
    },
    schedule: [
      {
        // every day at 12:59
        cron: '59 12 * * *',
      },
    ],
  },
  jobs: {
    job: job,
  },
}
