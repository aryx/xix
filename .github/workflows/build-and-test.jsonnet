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
	// Old OCaml version where I ported ocamlrun to plan9
	// This needs stdcompat so we can use |> and bytes type without issues.
	// The |> operator was introduced in 4.02.0, that we could add in
	// the matrix, but stdcompat does not compile with it.
	'3.10.0',
	// First OCaml version with a working ocamlformat OPAM package
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
	// available only for OCaml >= 4.0.0 and we want also 3.10.0
	'opam-depext': false,
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
