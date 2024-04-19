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
	// Old version with |> builtin but ideally we would like even before
	'4.02.1',
	// first version with valid ocamlformat
	'4.04.1',
	//TODO: 3.10.0, but pb with |> 
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
        echo No dependencies! This is xix! It does not need anything!
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
